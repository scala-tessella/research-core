package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol

import scala.collection.mutable

/** G2 — the METRIC layer of the equilateral relaxation: which angle assignments realize a euclidean symbol as
  * an edge-to-edge plane tiling with every edge of unit length.
  *
  * Variables: one interior angle γ per CORNER ORBIT (σ₁-pair of chambers), in units of π. Constraints:
  *
  *   - VERTEX equations — per 12-orbit `o`: `Σ_{D∈o} γ(corner(D)) = 2k/v` (the angles around a geometric
  *     vertex sum to 2π, folded by the orbit's symmetry; k = 1 chain / 2 cycle, v the branching);
  *   - FACE angle sums — per 01-orbit `o`: `Σ_{D∈o} γ(corner(D)) = (c·v − 2k)/v` (a p-gon's interior angles
  *     sum to (p−2)π, folded likewise; c = orbit length). Mutual consistency of the two families is EXACTLY
  *     the curvature identity K = 0 of the G0 audit.
  *   - CLOSURE — per face, the equilateral polygon with those angles must close: `F = Σ_j e^{iφ_j} = 0`, φ
  *     the running edge direction. No stabilizer case-analysis: on the angle-sum subspace a face with
  *     rotational stabilizer v ≥ 2 closes identically (v-periodic edge vectors sum by roots of unity), a
  *     mirror face loses one equation — the closure Jacobian RESTRICTED TO the linear nullspace has the right
  *     rank by itself.
  *
  * MODULI DIMENSION of a symbol = dim ker(exact linear system) − rank(closure Jacobian at the regular point,
  * restricted to that kernel). The regular point `γ = (p−2)/p` solves the full system for every relaxed k = 1
  * symbol: the uniform tilings, carried by every subgroup symbol of their nets.
  */
object MetricLayer:

  /** Corner-orbit index per chamber (σ₁-pairs) and the variable count. */
  private def cornerIndex(ds: DSymbol): (Array[Int], Int) =
    val idx  = Array.fill(ds.size + 1)(-1)
    var next = 0
    var d    = 1
    while d <= ds.size do
      if idx(d) < 0 then
        idx(d) = next
        idx(ds.get(1, d)) = next
        next += 1
      d += 1
    (idx, next)

  /** The exact linear layer: rows `(coefficients, rhs)` over the corner variables, angles in units of π. */
  final case class AngleSystem(vars: Int, corner: Array[Int], rows: Vector[(Array[Frac], Frac)])

  def angleSystem(ds: DSymbol): AngleSystem =
    val (corner, vars) = cornerIndex(ds)
    val rows           = ds.orbs.map: o =>
      val coeffs = Array.fill(vars)(Frac(0, 1))
      for d <- o.elements do coeffs(corner(d)) = coeffs(corner(d)) + Frac(1, 1)
      val k      = if o.isChain then 1 else 2
      val v      = ds.v(o.i, o.j, o.elements.head)
      val rhs    =
        if o.i == 1 then Frac.make(2L * k, v)           // vertex: 2π folded
        else Frac.make(o.length.toLong * v - 2L * k, v) // face: (p−2)π folded
      (coeffs, rhs)
    AngleSystem(vars, corner, rows)

  /** The regular point `γ_c = (p−2)/p`, p the side-count of the corner's face. */
  def regularPoint(ds: DSymbol): Array[Frac] =
    val (corner, vars) = cornerIndex(ds)
    val x              = Array.fill(vars)(Frac(0, 1))
    var d              = 1
    while d <= ds.size do
      val p = ds.m(0, 1, d)
      x(corner(d)) = Frac.make(p - 2L, p)
      d += 1
    x

  def satisfies(sys: AngleSystem, x: Array[Frac]): Boolean =
    sys.rows.forall: (coeffs, rhs) =>
      var acc = Frac(0, 1)
      var i   = 0
      while i < sys.vars do { acc = acc + coeffs(i) * x(i); i += 1 }
      (acc - rhs).isZero

  /** Basis of the HOMOGENEOUS solution space (exact RREF over Frac). */
  def nullspaceBasis(sys: AngleSystem): Vector[Array[Frac]] =
    val m          = sys.rows.map(_._1.clone).toArray
    val rows       = m.length
    val cols       = sys.vars
    val pivotOfCol = Array.fill(cols)(-1)
    var r          = 0
    var c          = 0
    while r < rows && c < cols do
      var pr = r
      while pr < rows && m(pr)(c).isZero do pr += 1
      if pr < rows then
        val t   = m(pr); m(pr) = m(r); m(r) = t
        val inv = m(r)(c)
        for j <- c until cols do m(r)(j) = m(r)(j) / inv
        for i <- 0 until rows if i != r && !m(i)(c).isZero do
          val f = m(i)(c)
          for j <- c until cols do m(i)(j) = m(i)(j) - f * m(r)(j)
        pivotOfCol(c) = r
        r += 1
      c += 1
    (0 until cols).iterator
      .filter(pivotOfCol(_) < 0)
      .map: f =>
        val v = Array.fill(cols)(Frac(0, 1))
        v(f) = Frac(1, 1)
        for pc <- 0 until cols if pivotOfCol(pc) >= 0 do v(pc) = Frac(0, 1) - m(pivotOfCol(pc))(f)
        v
      .toVector

  /** Exact consistency of an augmented rational system: false iff elimination produces `0 = c ≠ 0`. */
  def linearConsistent(rows: Vector[(Array[Frac], Frac)], vars: Int): Boolean =
    val m  = rows.map((c, r) => c.clone :+ r).toArray
    val nr = m.length
    var r  = 0
    var c  = 0
    while r < nr && c < vars do
      var pr = r
      while pr < nr && m(pr)(c).isZero do pr += 1
      if pr < nr then
        val t = m(pr); m(pr) = m(r); m(r) = t
        for i <- 0 until nr if i != r && !m(i)(c).isZero do
          val f = m(i)(c) / m(r)(c)
          for j <- c to vars do m(i)(j) = m(i)(j) - f * m(r)(j)
        r += 1
      c += 1
    m.forall(row => !(row.take(vars).forall(_.isZero) && !row(vars).isZero))

  /** The geometric corner sequence around the face through `d0` (corner-variable indices, length m₀₁): the
    * quotient walk `cur := σ₁(σ₀(cur))` — cross the edge, turn within the face — repeated to the full m
    * (`vertexConfig`'s folding convention, face-side).
    */
  private def faceCornerSeq(ds: DSymbol, corner: Array[Int], d0: Int): Array[Int] =
    val frag = mutable.ArrayBuffer.empty[Int]
    var cur  = d0
    var go   = true
    while go do
      frag += corner(cur)
      cur = ds.get(1, ds.get(0, cur))
      if cur == d0 then go = false
    val m    = ds.m(0, 1, d0)
    require(m % frag.length == 0, s"face walk length ${frag.length} does not divide m = $m")
    Array.tabulate(m)(i => frag(i % frag.length))

  /** `F = Σ_j e^{iφ_j}` for the equilateral polygon with corner angles `x` (π units) in `seq` order. */
  private def closureF(seq: Array[Int], x: Array[Double]): (Double, Double) =
    var phi = 0.0
    var sx  = 0.0
    var sy  = 0.0
    var j   = 0
    while j < seq.length do
      sx += math.cos(phi)
      sy += math.sin(phi)
      phi += math.Pi * (1.0 - x(seq(j)))
      j += 1
    (sx, sy)

  /** Largest per-face closure residual `|F|` at `x` — 0 (numerically) iff every face closes. */
  def maxClosureResidual(ds: DSymbol, x: Array[Double]): Double =
    val (corner, _) = cornerIndex(ds)
    var mx          = 0.0
    for o <- ds.orbs if o.i == 0 do
      val (fx, fy) = closureF(faceCornerSeq(ds, corner, o.elements.head), x)
      mx = mx.max(math.hypot(fx, fy))
    mx

  /** The two closure Jacobian rows of one face at `x`: `∂F/∂x_c = π Σ_{l: seq(l)=c} Σ_{j>l} (sin φ_j, −cos
    * φ_j)` (suffix sums, since φ_j depends on the corners BEFORE edge j).
    */
  private def closureJacobian(seq: Array[Int], x: Array[Double], vars: Int): (Array[Double], Array[Double]) =
    val p    = seq.length
    val phis = new Array[Double](p)
    var phi  = 0.0
    var j    = 0
    while j < p do
      phis(j) = phi
      phi += math.Pi * (1.0 - x(seq(j)))
      j += 1
    val rowX = new Array[Double](vars)
    val rowY = new Array[Double](vars)
    var sSin = 0.0
    var sCos = 0.0
    var l    = p - 1
    while l >= 0 do
      rowX(seq(l)) += math.Pi * sSin
      rowY(seq(l)) -= math.Pi * sCos
      sSin += math.sin(phis(l))
      sCos += math.cos(phis(l))
      l -= 1
    (rowX, rowY)

  /** Stacked closure residuals (2 per face orbit) and Jacobian rows over the full corner variables. */
  private def closureFJ(
      ds: DSymbol,
      corner: Array[Int],
      vars: Int,
      x: Array[Double]
  ): (Array[Double], Array[Array[Double]]) =
    val fs = mutable.ArrayBuffer.empty[Double]
    val js = mutable.ArrayBuffer.empty[Array[Double]]
    for o <- ds.orbs if o.i == 0 do
      val seq      = faceCornerSeq(ds, corner, o.elements.head)
      val (fx, fy) = closureF(seq, x)
      val (rx, ry) = closureJacobian(seq, x, vars)
      fs += fx; js += rx
      fs += fy; js += ry
    (fs.toArray, js.toArray)

  /** Rank of the closure Jacobian at the regular point, restricted to the linear nullspace. */
  def closureRank(ds: DSymbol, basis: Vector[Array[Frac]]): Int =
    if basis.isEmpty then 0
    else
      val (corner, vars) = cornerIndex(ds)
      val (_, jf)        = closureFJ(ds, corner, vars, regularPoint(ds).map(_.toDouble))
      val bs             = basis.map(_.map(_.toDouble))
      numericRank(jf.map(row => bs.map(b => dot(row, b)).toArray))

  private def dot(row: Array[Double], b: Array[Double]): Double =
    var acc = 0.0
    var i   = 0
    while i < row.length do { acc += row(i) * b(i); i += 1 }
    acc

  private def numericRank(m: Array[Array[Double]]): Int =
    val rows = m.length
    if rows == 0 then return 0
    val cols = m(0).length
    val tol  = 1e-7 * math.max(1.0, m.iterator.flatMap(_.iterator).map(math.abs).max)
    var rank = 0
    var c    = 0
    while c < cols && rank < rows do
      var pr = rank
      var pv = math.abs(m(pr)(c))
      for i <- rank + 1 until rows if math.abs(m(i)(c)) > pv do { pr = i; pv = math.abs(m(i)(c)) }
      if pv > tol then
        val t = m(pr); m(pr) = m(rank); m(rank) = t
        for i <- rank + 1 until rows do
          val f = m(i)(c) / m(rank)(c)
          for j <- c until cols do m(i)(j) -= f * m(rank)(j)
        rank += 1
      c += 1
    rank

  /** dim ker(linear) − rank(closure restricted): the moduli dimension at the regular point. */
  def moduliDimension(ds: DSymbol): Int =
    val ns = nullspaceBasis(angleSystem(ds))
    ns.size - closureRank(ds, ns)

  // ---- track B: EXACT closure ranks over ℚ(ζ₂₄) --------------------------------------------------------

  /** Track B — rank of the closure Jacobian at the regular point restricted to the linear nullspace, EXACTLY:
    * at the regular point every direction angle is an integer multiple of π/12 (faces are {3,4,6,8,12}-gons,
    * preserved by quotients), so every Jacobian entry lies in ℚ(ζ₂₄) and the rank is a field computation with
    * exact zero tests ([[Cyclo24]]). The common scalar π of the numeric rows is dropped — rank-invariant.
    */
  def closureRankExact(ds: DSymbol, basis: Vector[Array[Frac]]): Int =
    Cyclo24.rank(restrictedJacobianExact(ds, basis))

  /** Track B/D — the closure Jacobian at the regular point restricted to the linear nullspace, as an exact
    * ℚ(ζ₂₄) matrix (2 rows per face orbit × nullity columns): the object whose rank is certified.
    */
  def restrictedJacobianExact(ds: DSymbol, basis: Vector[Array[Frac]]): Array[Array[Cyclo24.Cyclo]] =
    import Cyclo24.Cyclo
    if basis.isEmpty then Array.empty
    else
      val (corner, vars) = cornerIndex(ds)
      val reg            = regularPoint(ds)
      val twelfths       = reg.map: g =>
        require(12L * g.num % g.den == 0, s"regular corner angle $g is not a multiple of π/12")
        (12L * g.num / g.den).toInt
      val rows           = mutable.ArrayBuffer.empty[Array[Cyclo]]
      for o <- ds.orbs if o.i == 0 do
        val seq  = faceCornerSeq(ds, corner, o.elements.head)
        val p    = seq.length
        val ks   = new Array[Int](p) // φ_j in π/12 units
        var k    = 0
        for j <- 0 until p do
          ks(j) = k
          k += 12 - twelfths(seq(j))
        val rowX = Array.fill(vars)(Cyclo.zero)
        val rowY = Array.fill(vars)(Cyclo.zero)
        var sSin = Cyclo.zero
        var sCos = Cyclo.zero
        for l <- p - 1 to 0 by -1 do
          rowX(seq(l)) = rowX(seq(l)) + sSin
          rowY(seq(l)) = rowY(seq(l)) - sCos
          sSin = sSin + Cyclo.sinPi12(ks(l))
          sCos = sCos + Cyclo.cosPi12(ks(l))
        rows += basis.map(b => dotExact(rowX, b)).toArray
        rows += basis.map(b => dotExact(rowY, b)).toArray
      rows.toArray

  /** Track D — the angle system's coefficient matrix embedded in ℚ(ζ₂₄), so the linear layer's rank (hence
    * the nullity behind every moduli dimension) gets the same witness treatment.
    */
  def linearMatrixExact(sys: AngleSystem): Array[Array[Cyclo24.Cyclo]] =
    sys.rows.map((coeffs, _) => coeffs.map(f => Cyclo24.Cyclo.one.scale(Cyclo24.Rat(f)))).toArray

  private def dotExact(row: Array[Cyclo24.Cyclo], b: Array[Frac]): Cyclo24.Cyclo =
    var acc = Cyclo24.Cyclo.zero
    var i   = 0
    while i < row.length do
      if !b(i).isZero && !row(i).isZero then acc = acc + row(i).scale(Cyclo24.Rat(b(i)))
      i += 1
    acc

  /** [[moduliDimension]] with the closure rank certified exact — the paper's authoritative dimension. */
  def moduliDimensionExact(ds: DSymbol): Int =
    val ns = nullspaceBasis(angleSystem(ds))
    ns.size - closureRankExact(ds, ns)

  /** Kernel basis (with tolerance) of a small Double matrix acting on `cols` coordinates. */
  private def doubleKernel(m: Array[Array[Double]], cols: Int): Vector[Array[Double]] =
    val rows       = m.map(_.clone)
    val nr         = rows.length
    val pivotOfCol = Array.fill(cols)(-1)
    val mx         = if nr == 0 then 0.0 else rows.iterator.flatMap(_.iterator).map(math.abs).max
    val tol        = 1e-7 * math.max(1.0, mx)
    var r          = 0
    var c          = 0
    while r < nr && c < cols do
      var pr = r
      var pv = math.abs(rows(pr)(c))
      for i <- r + 1 until nr if math.abs(rows(i)(c)) > pv do { pr = i; pv = math.abs(rows(i)(c)) }
      if pv > tol then
        val t0  = rows(pr); rows(pr) = rows(r); rows(r) = t0
        val inv = rows(r)(c)
        for j <- c until cols do rows(r)(j) /= inv
        for i <- 0 until nr if i != r do
          val f = rows(i)(c)
          for j <- c until cols do rows(i)(j) -= f * rows(r)(j)
        pivotOfCol(c) = r
        r += 1
      c += 1
    (0 until cols).iterator
      .filter(pivotOfCol(_) < 0)
      .map: fcol =>
        val v = new Array[Double](cols)
        v(fcol) = 1.0
        for pc <- 0 until cols if pivotOfCol(pc) >= 0 do v(pc) = -rows(pivotOfCol(pc))(fcol)
        v
      .toVector

  private def solveSquare(a: Array[Array[Double]], b: Array[Double]): Array[Double] =
    val n = b.length
    val m = Array.tabulate(n)(i => a(i) :+ b(i))
    var r = 0
    while r < n do
      var pr = r
      for i <- r + 1 until n if math.abs(m(i)(r)) > math.abs(m(pr)(r)) do pr = i
      val t0 = m(pr); m(pr) = m(r); m(r) = t0
      for i <- 0 until n if i != r && m(r)(r) != 0.0 do
        val f = m(i)(r) / m(r)(r)
        for j <- r to n do m(i)(j) -= f * m(r)(j)
      r += 1
    Array.tabulate(n)(i => if m(i)(i) == 0.0 then 0.0 else m(i)(n) / m(i)(i))

  /** Basis of the MODULI TANGENT at the regular point (full variable space, π units): the kernel of the
    * closure Jacobian restricted to the linear nullspace, pushed back to corner coordinates. Its size is
    * [[moduliDimension]] by construction.
    */
  def tangentBasis(ds: DSymbol): Vector[Array[Double]] =
    val bs = nullspaceBasis(angleSystem(ds)).map(_.map(_.toDouble))
    if bs.isEmpty then Vector.empty
    else
      val (corner, vars) = cornerIndex(ds)
      val (_, jf)        = closureFJ(ds, corner, vars, regularPoint(ds).map(_.toDouble))
      val m              = jf.map(row => Array.tabulate(bs.size)(j => dot(row, bs(j))))
      doubleKernel(m, bs.size).map: yc =>
        Array.tabulate(vars)(i => bs.indices.map(j => yc(j) * bs(j)(i)).sum)

  /** A GENUINE moduli point near `regular + t·dir` (`dir` from [[tangentBasis]], π units): Gauss–Newton
    * correction of the closure system within the linear nullspace, so the vertex and face angle sums stay
    * exact and the returned point closes every face to machine precision (caller should assert the residual).
    * For families on which closure vanishes identically the correction is a no-op.
    */
  def moduliPoint(ds: DSymbol, dir: Array[Double], t: Double): Array[Double] =
    val bs                                  = nullspaceBasis(angleSystem(ds)).map(_.map(_.toDouble))
    val (corner, vars)                      = cornerIndex(ds)
    val x0                                  = regularPoint(ds).map(_.toDouble)
    val n                                   = bs.size
    def at(y: Array[Double]): Array[Double] =
      Array.tabulate(vars)(i => x0(i) + (0 until n).map(j => y(j) * bs(j)(i)).sum)
    val gram                                = Array.tabulate(n, n)((i, j) => dot(bs(i), bs(j)))
    val y                                   = solveSquare(gram, Array.tabulate(n)(i => t * dot(bs(i), dir)))
    for _ <- 1 to 40 do
      val (f, jf) = closureFJ(ds, corner, vars, at(y))
      val m       = jf.map(row => Array.tabulate(n)(j => dot(row, bs(j))))
      val r       = m.length
      val mmt     = Array.tabulate(r, r)((i, j) => dot(m(i), m(j)) + (if i == j then 1e-12 else 0.0))
      val lam     = solveSquare(mmt, f)
      for j <- 0 until n do y(j) -= (0 until r).map(i => lam(i) * m(i)(j)).sum
    at(y)

  /** Per-chamber corner angles in RADIANS at the point `x` (π units) — the developing renderer's input. */
  def chamberAngles(ds: DSymbol, x: Array[Double]): Array[Double] =
    val (corner, _) = cornerIndex(ds)
    Array.tabulate(ds.size + 1)(d => if d == 0 then 0.0 else math.Pi * x(corner(d)))

  /** G3 — EXACT-symmetry realizability near the regular point: some unit-edge realization has metric symmetry
    * EXACTLY the symbol's group. A symmetry upgrade of a realization is a proper covering onto a smaller
    * symbol, moduli pull back injectively along coverings (so quotient dims only shrink, and first-step
    * quotients dominate), and finitely many proper subspaces cover the tangent space iff one equals it —
    * hence the criterion: the moduli dimension strictly exceeds EVERY proper quotient's. Minimal symbols pass
    * vacuously (their regular point IS the maximal-symmetry uniform tiling); a non-minimal rigid symbol
    * always fails (straight unit edges cannot be wiggled into less symmetry). Verdicts are tangent-space
    * local at the regular point (see the G2/G3 caveats).
    */
  def exactSymmetryRealizable(ds: DSymbol): Boolean =
    val d = moduliDimension(ds)
    DelaneySymbols.properQuotients(ds).forall(q => moduliDimension(q) < d)
