package io.github.scala_tessella.research_core

/** Interval-certified dihedral angles for arbitrary unit-edge uniform polyhedra, derived from the vertex
  * configuration ALONE (no coordinate tables): the vertex figure of a uniform polyhedron with configuration
  * (p₁..p_k) is a cyclic polygon whose vertices are the unit-distant neighbors and whose side lengths are the
  * corner chords 2cos(π/pᵢ); its circumradius is enclosed by certified bisection, the corner is
  * reconstructed, and dihedrals come out as intervals.
  *
  * Interval discipline: double arithmetic is correctly rounded (≤ 1/2 ulp), so results are widened by one ulp
  * per operation; for java.lang.Math trig/atan the documented accuracy is ≤ 1 ulp, widened here by two ulps.
  * An interval certifies: the true value lies inside. Exclusions proved with these intervals are genuine
  * proofs of non-equality; only interval HITS (which would need agreement to ~1e-10) require exact follow-up.
  */
object CertifiedDihedrals:

  final case class Iv(lo: Double, hi: Double):
    def +(o: Iv): Iv                 = Iv.widen(lo + o.lo, hi + o.hi)
    def -(o: Iv): Iv                 = Iv.widen(lo - o.hi, hi - o.lo)
    def *(o: Iv): Iv                 =
      val ps = Array(lo * o.lo, lo * o.hi, hi * o.lo, hi * o.hi)
      Iv.widen(ps.min, ps.max)
    def /(o: Iv): Iv                 = // denominator must not straddle 0
      require(o.lo > 0 || o.hi < 0, s"division by interval straddling zero: $o")
      val ps = Array(lo / o.lo, lo / o.hi, hi / o.lo, hi / o.hi)
      Iv.widen(ps.min, ps.max)
    def unary_- : Iv                 = Iv(-hi, -lo)
    def sqrtIv: Iv                   = Iv.widen(math.sqrt(math.max(0.0, lo)), math.sqrt(math.max(0.0, hi)))
    def width: Double                = hi - lo
    def mid: Double                  = (lo + hi) / 2
    def contains(x: Double): Boolean = lo <= x && x <= hi
    def intersects(o: Iv): Boolean   = lo <= o.hi && o.lo <= hi
    def strictlyBelow(x: Double)     = hi < x
    def strictlyAbove(x: Double)     = lo > x

  object Iv:
    private def d2(x: Double)       = Math.nextDown(Math.nextDown(x))
    private def u2(x: Double)       = Math.nextUp(Math.nextUp(x))
    def widen(a: Double, b: Double) = Iv(Math.nextDown(a), Math.nextUp(b))
    def point(x: Double)            = Iv(x, x)
    val pi: Iv                      = Iv(Math.nextDown(Math.PI), Math.nextUp(Math.PI))

    /** cos over [0, π] (decreasing). */
    def cosDec(x: Iv): Iv = Iv(d2(Math.cos(x.hi)), u2(Math.cos(x.lo)))

    /** asin over [−1, 1] (increasing); interval slop beyond ±1 is clamped (the true value is inside). */
    def asinInc(x: Iv): Iv =
      require(x.lo >= -1 - 1e-3 && x.hi <= 1 + 1e-3, s"asin domain: $x")
      Iv(d2(Math.asin(math.max(-1.0, x.lo))), u2(Math.asin(math.min(1.0, x.hi))))

    /** acos over [−1, 1] (decreasing), clamped as above. */
    def acosDec(x: Iv): Iv =
      require(x.lo >= -1 - 1e-3 && x.hi <= 1 + 1e-3, s"acos domain: $x")
      Iv(d2(Math.acos(math.min(1.0, x.hi))), u2(Math.acos(math.max(-1.0, x.lo))))

    /** general cos enclosure (handles extrema inside the interval). */
    def cosGen(x: Iv): Iv  =
      var lo = math.min(Math.cos(x.lo), Math.cos(x.hi))
      var hi = math.max(Math.cos(x.lo), Math.cos(x.hi))
      val k0 = math.ceil(x.lo / Math.PI - 1e-12).toLong
      val k1 = math.floor(x.hi / Math.PI + 1e-12).toLong
      var k  = k0
      while k <= k1 do
        if k % 2 == 0 then hi = 1.0 else lo = -1.0
        k += 1
      Iv(d2(lo), u2(hi))
    def sinGen(x: Iv): Iv  = cosGen(x - pi * point(0.5))
    def atanInc(x: Iv): Iv = Iv(d2(Math.atan(x.lo)), u2(Math.atan(x.hi)))
    def toDeg(x: Iv): Iv   = x * (point(180.0) / pi)

  type V3 = (Iv, Iv, Iv)
  private def dot(a: V3, b: V3): Iv   = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def sub(a: V3, b: V3): V3   = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def scale(a: V3, s: Iv): V3 = (a._1 * s, a._2 * s, a._3 * s)

  /** The corner figure of the unit-edge uniform polyhedron with vertex configuration `ps`: the unit
    * DIRECTIONS to the k neighbor vertices, cyclic. On the sphere of directions the cell occupies the
    * spherical k-gon with these vertices: side i spans directions i, i+1 with arc length the interior angle
    * of face ps(i), and the polygon angle at direction i is the dihedral of the face pair (ps(i−1), ps(i)).
    * Requires the vertex figure to be a cyclic polygon containing its circumcenter (true for every convex
    * uniform polyhedron treated here; checked at runtime).
    */
  def cornerFigure(ps: List[Int]): Vector[V3] =
    val k                  = ps.size
    val chords             = ps.map(p => Iv.point(2.0) * Iv.cosDec(Iv.pi / Iv.point(p.toDouble))).toVector
    val cMax               = chords.map(_.hi).max
    // g(ρ) = Σ 2 asin(cᵢ / 2ρ) − 2π, strictly decreasing in ρ on (cMax/2, 1)
    def g(rho: Double): Iv =
      val r = Iv.point(rho)
      chords.map(c => Iv.asinInc(c / (r * Iv.point(2.0))) * Iv.point(2.0)).reduce(_ + _) -
        Iv.pi * Iv.point(2.0)
    var lo                 = cMax / 2 * (1 + 1e-13)
    var hi                 = 1.0 - 1e-13
    require(g(lo).lo > 0, s"vertex figure of $ps: circumcenter not inside (g(cMax/2) ≤ 0)")
    require(g(hi).hi < 0, s"vertex figure of $ps: no solution below 1 (g(1) ≥ 0)")
    var it                 = 0
    while it < 200 && hi - lo > 1e-14 do
      val m  = (lo + hi) / 2
      val gm = g(m)
      if gm.lo > 0 then lo = m
      else if gm.hi < 0 then hi = m
      else it = 200 // sign indeterminate at interval precision: bracket is tight enough
      it += 1
    val rho                = Iv(Math.nextDown(lo), Math.nextUp(hi))
    val h                  = (Iv.point(1.0) - rho * rho).sqrtIv
    val phis               = chords.map(c => Iv.asinInc(c / (rho * Iv.point(2.0))) * Iv.point(2.0))
    val psis               = phis.scanLeft(Iv.point(0.0))(_ + _) // ψ₀..ψ_k
    psis.take(k).map(psi => (rho * Iv.cosGen(psi), rho * Iv.sinGen(psi), h))

  /** Certified dihedrals of the unit-edge uniform polyhedron with vertex configuration `ps`, in degrees, one
    * entry per edge at the vertex: ((left face, right face), dihedral).
    */
  def dihedrals(ps: List[Int]): Vector[((Int, Int), Iv)] =
    val k    = ps.size
    val nbrs = cornerFigure(ps)
    // edge i = (v, neighbor i) is shared by faces ps(i−1) and ps(i)
    (0 until k).toVector.map { i =>
      val e     = nbrs(i)
      val nPrev = nbrs((i + k - 1) % k)
      val nNext = nbrs((i + 1) % k)
      val ee    = dot(e, e)
      val u1    = sub(nPrev, scale(e, dot(nPrev, e) / ee))
      val u2v   = sub(nNext, scale(e, dot(nNext, e) / ee))
      val cosD  = dot(u1, u2v) / (dot(u1, u1).sqrtIv * dot(u2v, u2v).sqrtIv)
      ((ps((i + k - 1) % k), ps(i)), Iv.toDeg(Iv.acosDec(cosD)))
    }

  /** Distinct edge types (unordered face pair → dihedral hull). */
  def edgeTypesOf(ps: List[Int]): Map[(Int, Int), Iv] =
    dihedrals(ps)
      .groupMapReduce(t => (math.min(t._1._1, t._1._2), math.max(t._1._1, t._1._2)))(_._2) { (a, b) =>
        Iv(math.min(a.lo, b.lo), math.max(a.hi, b.hi))
      }

  /** Vertex configurations: the 13 core cells (validation anchors) and the outsiders. */
  val coreConfigs: Map[String, List[Int]] = Map(
    "tet"       -> List(3, 3, 3),
    "cube"      -> List(4, 4, 4),
    "oct"       -> List(3, 3, 3, 3),
    "truncTet"  -> List(3, 6, 6),
    "co"        -> List(3, 4, 3, 4),
    "truncOct"  -> List(4, 6, 6),
    "truncCube" -> List(3, 8, 8),
    "rco"       -> List(3, 4, 4, 4),
    "tco"       -> List(4, 6, 8),
    "p3"        -> List(3, 4, 4),
    "p6"        -> List(6, 4, 4),
    "p8"        -> List(8, 4, 4),
    "p12"       -> List(12, 4, 4)
  )

  val outsiderConfigs: Map[String, List[Int]] = Map(
    "dodec"           -> List(5, 5, 5),
    "icos"            -> List(3, 3, 3, 3, 3),
    "icosidodec"      -> List(3, 5, 3, 5),
    "truncDodec"      -> List(3, 10, 10),
    "truncIcos"       -> List(5, 6, 6),
    "rhombicosidodec" -> List(3, 4, 5, 4),
    "truncIcosidodec" -> List(4, 6, 10),
    "snubDodec"       -> List(3, 3, 3, 3, 5),
    "snubCube"        -> List(3, 3, 3, 3, 4)
  )

  def antiprismConfig(q: Int): List[Int] = List(q, 3, 3, 3)
