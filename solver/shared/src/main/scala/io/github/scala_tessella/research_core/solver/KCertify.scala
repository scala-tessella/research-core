package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import io.github.scala_tessella.research_core.solver.SatSolver.SolverSink
import io.github.scala_tessella.research_core.solver.SymbolAssembly.{ClauseSink, NullSink, TeeSink}

import scala.collection.mutable

/** [[K2Certify]] generalized from TWO vertex-orbit classes to `k`, the SAT side of the k = 3 (and beyond)
  * completeness obligations. Same search object and same K1 core (pair variables, exactly-one, the (σ₀σ₂)² =
  * id manifold axiom, BFS-consistent numbering); what changes is the orbit layer and the arity of the
  * curvature bookkeeping:
  *
  *   - AT MOST k VERTEX ORBITS. The single boolean `col` becomes `k` indicators per chamber with exactly-one,
  *     σ₁/σ₂-INVARIANT (so classes are unions of vertex orbits), chamber 1 pinned to class 0. Each class
  *     carries an ANCHOR — `anchor(j)(d)`, forced by its own premises to the minimal chamber of class j — and
  *     anchored LEVEL-REACHABILITY makes each class ONE orbit: `lev(d, t)` = "within t σ₁/σ₂-steps of an
  *     anchor", one-directional step witnesses, coverage units `lev(d, C−1)`. A D-set with more than k orbits
  *     admits no such coloring; a genuine ≤ k-orbit labeling extends.
  *   - CLASS-ORDER SYMMETRY BREAKING (new at k ≥ 3, where it is not free): anchors must increase with the
  *     class index, so the k! label permutations collapse to the single canonical assignment. Sound because
  *     ordering classes by minimal chamber is a bijection on colorings of any given labeled D-set.
  *   - TIER-1 CURVATURE, unchanged per chamber (`good(d) ⟺ (σ₀σ₁)³(d) = d`, certified one-directionally), but
  *     with a cardinality counter and chain flag PER CLASS and the 7-case contribution selectors emitted k
  *     times; the master constraint `#good ≥ 3C − Σ_j v_j` then ranges over 7^k case tuples (49 at k = 2, 343
  *     at k = 3). K2Certify derived class 2's cardinality as the complement C − card₁; with k classes each
  *     gets its own counter, which is the honest generalization of the same statement.
  *
  * As in track A2 no exact curvature arithmetic enters the CNF: the tier-1 lemma is the bridge
  * (euclidean-feasible ⇒ tier-1) and euclidean filtering stays the exact JVM tail. Base + blocking UNSAT with
  * a DRAT proof = completeness of the tier-1 universe at that chamber count.
  *
  * NOTE on scope, so nobody mistakes what this buys: the A2 obligation shape (enumerate the universe, block
  * it, prove the residue UNSAT) needs the universe to be BLOCKABLE. At k ≤ 2 / C ≤ 24 it was 2,710 D-sets;
  * the tier-1 universe at k = 3 is far larger (the tier-1 walk is toothless mid-window, which is what forced
  * the walk's euclid mode), so the reachable chamber counts must be MEASURED, not assumed. That measurement
  * is the point of the first campaign, not a formality.
  */
object KCertify:

  final case class Encoding(x: Map[(Int, Int, Int), Int], maxVar: Int):
    def apply(i: Int, a: Int, b: Int): Int = x((i, math.min(a, b), math.max(a, b)))

  /** Emit the C-chamber, k-class CNF into `sink`; returns the pair-variable map (auxiliaries follow it).
    *
    * With `staircase = true` the STAIRCASE tile side is added ON TOP of the tier-1 constraint: per chamber a
    * weight w ∈ {0, 1, 2, 3} — the bin floor of the exact tile deficit rate by the chamber's period r under
    * σ₀σ₁ — where LOWERING the weight needs a one-directional period certificate (w = 0: r ∈ {1, 3}, the
    * tier-1 `good` verbatim; w ≤ 1: r ≤ 5, via (σ₀σ₁)^r(d) = d for some r ∈ {3, 4, 5}; w ≤ 2: r ≤ 11, via r ∈
    * {6..11} — divisor closure covers every smaller period), and the master constraint
    * `Σ_d w(d) ≤ Σ_j v_j − 2C` ranges over the SAME 7^k selector tuples as tier-1. Period tests are
    * meet-in-the-middle diagonals of the power relations q₂…q₆, each built one-directionally from the
    * existing q exactly as q itself is built from the pair variables. The staircase block is emitted LAST, so
    * with the flag off the encoding is byte-identical to before — the k = 2 op-for-op pin against the
    * DRAT-certified [[K2Certify]] is preserved by construction.
    */
  def encode(c: Int, k: Int, sink: ClauseSink, staircase: Boolean = false): Encoding =
    require(k >= 1, "at least one vertex-orbit class")
    val pv                        = mutable.LinkedHashMap.empty[(Int, Int, Int), Int]
    var next                      = 0
    def fresh(): Int              = { next += 1; next }
    for i <- 0 to 2; a <- 1 to c; b <- a to c do pv((i, a, b)) = fresh()
    val enc                       = Encoding(pv.toMap, next)
    def x(i: Int, a: Int, b: Int) = enc(i, a, b)

    // ---- K1 core: involutions, manifold axiom, BFS-consistent numbering (verbatim from K2Certify) -----
    for i <- 0 to 2; d <- 1 to c do sink.exactlyOne((1 to c).map(e => x(i, d, e)).distinct.toArray)
    for a <- 1 to c; b <- 1 to c; e <- 1 to c; d <- 1 to c do
      val lits = List(-x(2, a, b), -x(0, b, e), -x(0, a, d), x(2, d, e))
      if lits.distinct.size == lits.size then sink.clause(lits)
    if c >= 2 then
      val seen                        =
        Array.tabulate(c + 1)(m => if m < 2 then Array.empty[Int] else new Array[Int](3 * (m - 1)))
      for m <- 2 to c; s <- 0 until 3 * (m - 1) do seen(m)(s) = fresh()
      def produces(s: Int, m: Int)    = x(s % 3, s / 3 + 1, m)
      for m <- 2 to c do
        val cap = 3 * (m - 1)
        for s <- 0 until cap do
          val p = produces(s, m)
          if s == 0 then
            sink.clause(List(-seen(m)(0), p))
            sink.clause(List(-p, seen(m)(0)))
          else
            sink.clause(List(-seen(m)(s - 1), seen(m)(s)))
            sink.clause(List(-p, seen(m)(s)))
            sink.clause(List(-seen(m)(s), seen(m)(s - 1), p))
        sink.clause(List(seen(m)(cap - 1)))
      def seenAt(m: Int, s: Int): Int = seen(m)(math.min(s, 3 * (m - 1) - 1))
      for m <- 2 until c; s <- 0 until 3 * m do sink.clause(List(-seenAt(m + 1, s), seenAt(m, s)))

    // ---- at most k vertex orbits: invariant k-coloring, anchors, anchored reachability ----------------
    val cls    = Array.tabulate(c + 1, k)((d, _) => if d == 0 then 0 else fresh())
    for d <- 1 to c do sink.exactlyOne((0 until k).map(j => cls(d)(j)).toArray)
    sink.clause(List(cls(1)(0)))
    for i <- 1 to 2; a <- 1 to c; b <- a + 1 to c; j <- 0 until k do
      sink.clause(List(-x(i, a, b), -cls(a)(j), cls(b)(j)))
      sink.clause(List(-x(i, a, b), cls(a)(j), -cls(b)(j)))
    // anchor(j)(d): d is the minimal chamber of class j (premises force it, hence unique)
    val anchor = Array.tabulate(k, c + 1)((j, d) => if j == 0 || d < 2 then 0 else fresh())
    for j <- 1 until k; d <- 2 to c do
      sink.clause(List(-anchor(j)(d), cls(d)(j)))
      for e <- 1 until d do sink.clause(List(-anchor(j)(d), -cls(e)(j)))
    // class-order symmetry breaking: anchors strictly increase in the class index
    for j <- 1 until k - 1; d <- 2 to c; e <- 2 to d do
      sink.clause(List(-anchor(j)(d), -anchor(j + 1)(e)))
    val tMax   = math.max(0, c - 1)
    val lev    = Array.tabulate(c + 1, tMax + 1)((d, _) => if d == 0 then 0 else fresh())
    sink.clause(List(lev(1)(0)))
    for d <- 2 to c do sink.clause(-lev(d)(0) :: (1 until k).map(j => anchor(j)(d)).toList)
    for d <- 1 to c; t <- 1 to tMax do sink.clause(List(-lev(d)(t - 1), lev(d)(t)))
    for d <- 2 to c; t <- 1 to tMax do
      val steps =
        for i <- 1 to 2; e <- 1 to c if e != d yield
          val s = fresh()
          sink.clause(List(-s, x(i, d, e)))
          sink.clause(List(-s, lev(e)(t - 1)))
          s
      sink.clause(-lev(d)(t) :: lev(d)(t - 1) :: steps.toList)
    for d <- 1 to c do sink.clause(List(lev(d)(tMax)))

    // ---- tier-1 curvature: good(d) ⟺ (σ₀σ₁)³(d) = d, certified one-directionally (verbatim) ----------
    val q    = Array.tabulate(c + 1, c + 1)((d, e) => if d == 0 || e == 0 then 0 else fresh())
    for d <- 1 to c; e <- 1 to c do
      val ws =
        for f <- 1 to c yield
          val u = fresh()
          sink.clause(List(-u, x(1, d, f)))
          sink.clause(List(-u, x(0, f, e)))
          u
      sink.clause(-q(d)(e) :: ws.toList)
    val good = Array.tabulate(c + 1)(d => if d == 0 then 0 else fresh())
    for d <- 1 to c do
      val ts =
        for e <- 1 to c; f <- 1 to c yield
          val t = fresh()
          sink.clause(List(-t, q(d)(e)))
          sink.clause(List(-t, q(e)(f)))
          sink.clause(List(-t, q(f)(d)))
          t
      sink.clause(-good(d) :: ts.toList)

    // ---- exact unary counters (sequential, both polarities) — verbatim from K2Certify -----------------
    def unaryCounter(bits: IndexedSeq[Int]): Array[Int] =
      val n = bits.length
      val p = Array.ofDim[Int](n + 1, n + 1)
      for d <- 1 to n; j <- 1 to d do p(d)(j) = fresh()
      sink.clause(List(-p(1)(1), bits(0)))
      sink.clause(List(-bits(0), p(1)(1)))
      for d <- 2 to n; j <- 1 to d do
        val a     = if j <= d - 1 then p(d - 1)(j) else 0
        val bprev = if j == 1 then -1 else p(d - 1)(j - 1)
        if bprev == -1 then
          if a == 0 then sink.clause(List(-p(d)(j), bits(d - 1)))
          else sink.clause(List(-p(d)(j), a, bits(d - 1)))
        else if a == 0 then
          sink.clause(List(-p(d)(j), bprev))
          sink.clause(List(-p(d)(j), bits(d - 1)))
        else
          sink.clause(List(-p(d)(j), a, bprev))
          sink.clause(List(-p(d)(j), a, bits(d - 1)))
        if a != 0 then sink.clause(List(-a, p(d)(j)))
        if bprev == -1 then sink.clause(List(-bits(d - 1), p(d)(j)))
        else sink.clause(List(-bprev, -bits(d - 1), p(d)(j)))
      Array.tabulate(n + 1)(j => if j == 0 then -1 else p(n)(j))

    val card  = Array.tabulate(k)(j => unaryCounter((1 to c).map(d => cls(d)(j))))
    val goodG = unaryCounter((1 to c).map(good))

    // ---- chain flag per class (exact, both directions) ------------------------------------------------
    def chainFlag(j: Int): Int =
      val cf = fresh()
      val ws =
        for d <- 1 to c yield
          val w = fresh()
          sink.clause(List(-w, cls(d)(j)))
          sink.clause(List(-w, x(1, d, d), x(2, d, d)))
          w
      sink.clause(-cf :: ws.toList)
      for d <- 1 to c; i <- 1 to 2 do sink.clause(List(-cls(d)(j), -x(i, d, d), cf))
      cf

    // ---- per-class contribution selectors (each case implies its certifying structure) ----------------
    val values                      = Array(0, 4, 6, 12, 8, 12, 24) // E, C1, C2, C3 (chains 1/2/≥3), Y2, Y4, Y6 (cycles)
    def emitSel(j: Int): Array[Int] =
      val cf                                                                                = chainFlag(j)
      val sel                                                                               = Array.fill(7)(fresh())
      def ge(m: Int): Int                                                                   = card(j)(m)
      def lt(m: Int): Int                                                                   = -ge(m)
      sink.clause(sel.toList)
      for i <- sel.indices; jj <- i + 1 until sel.length do sink.clause(List(-sel(i), -sel(jj)))
      // E: class empty — impossible for class 0, which contains chamber 1
      if j == 0 then sink.clause(List(-sel(0)))
      else sink.clause(List(-sel(0), lt(1)))
      def sizedCase(s: Int, chain: Boolean, exact: Option[Int], atLeast: Option[Int]): Unit =
        sink.clause(List(-sel(s), if chain then cf else -cf))
        exact.foreach: l =>
          if l <= c then
            sink.clause(List(-sel(s), ge(l)))
            if l + 1 <= c then sink.clause(List(-sel(s), lt(l + 1)))
          else sink.clause(List(-sel(s)))
        atLeast.foreach: l =>
          if l <= c then sink.clause(List(-sel(s), ge(l)))
          else sink.clause(List(-sel(s)))
      sizedCase(1, chain = true, Some(1), None)
      sizedCase(2, chain = true, Some(2), None)
      sizedCase(3, chain = true, None, Some(3))
      sizedCase(4, chain = false, Some(2), None)
      sizedCase(5, chain = false, Some(4), None)
      sizedCase(6, chain = false, None, Some(6))
      sel
    val sels                        = Array.tabulate(k)(emitSel)

    // ---- the tier-1 requirement: #good ≥ 3C − Σ_j v_j, over all 7^k case tuples ----------------------
    def emitMaster(j: Int, acc: List[Int], vSum: Int): Unit =
      if j == k then
        val m = 3 * c - vSum
        if m > c then sink.clause(acc.map(-_))
        else if m >= 1 then sink.clause(goodG(m) :: acc.map(-_))
      else for i <- values.indices do emitMaster(j + 1, sels(j)(i) :: acc, vSum + values(i))
    emitMaster(0, Nil, 0)

    // ---- STAIRCASE tile side — emitted LAST so `staircase = false` is
    // byte-identical to the pre-staircase encoding ------------------------------------------------------
    if staircase then
      // power relations q_n(d)(e) ⟺ (σ₀σ₁)^n(d) = e, one-directional, q_n from q_{n−1} ∘ q
      val qPow                                                     = mutable.Map(1 -> q)
      for n <- 2 to 6 do
        val prev = qPow(n - 1)
        val qn   = Array.tabulate(c + 1, c + 1)((d, e) => if d == 0 || e == 0 then 0 else fresh())
        for d <- 1 to c; e <- 1 to c do
          val ws =
            for f <- 1 to c yield
              val u = fresh()
              sink.clause(List(-u, prev(d)(f)))
              sink.clause(List(-u, q(f)(e)))
              u
          sink.clause(-qn(d)(e) :: ws.toList)
        qPow(n) = qn
      // period-membership certificate: some split r = a + b with (σ₀σ₁)^a(d) = e and (σ₀σ₁)^b(e) = d
      def periodCert(d: Int, splits: List[(Int, Int)]): Int        =
        val v  = fresh()
        val ws =
          for (a, b) <- splits; e <- 1 to c yield
            val u = fresh()
            sink.clause(List(-u, qPow(a)(d)(e)))
            sink.clause(List(-u, qPow(b)(e)(d)))
            u
        sink.clause(-v :: ws)
        v
      // r ∈ {3, 4, 5}: divisor closure {1, 2, 3, 4, 5} — every rate there is ≥ the floor 1 or has w = 0
      val le5                                                      = Array.tabulate(c + 1)(d =>
        if d == 0 then 0 else periodCert(d, List((2, 1), (2, 2), (2, 3)))
      )
      // r ∈ {6..11}: divisor closure {1..11} — every rate there is ≥ the floor 2 or has a lower bin
      val le11                                                     = Array.tabulate(c + 1)(d =>
        if d == 0 then 0 else periodCert(d, List((3, 3), (3, 4), (4, 4), (4, 5), (5, 5), (5, 6)))
      )
      // weight indicators w ≥ 1 / ≥ 2 / ≥ 3: lowering needs the bin's certificate; default is 3
      val w1                                                       = Array.tabulate(c + 1)(d => if d == 0 then 0 else fresh())
      val w2                                                       = Array.tabulate(c + 1)(d => if d == 0 then 0 else fresh())
      val w3                                                       = Array.tabulate(c + 1)(d => if d == 0 then 0 else fresh())
      for d <- 1 to c do
        sink.clause(List(w1(d), good(d))) // w = 0 ⇒ r ∈ {1, 3}
        sink.clause(List(w2(d), le5(d)))  // w ≤ 1 ⇒ r ≤ 5
        sink.clause(List(w3(d), le11(d))) // w ≤ 2 ⇒ r ≤ 11
        sink.clause(List(-w3(d), w2(d)))
        sink.clause(List(-w2(d), w1(d)))
      val wSum                                                     = unaryCounter((1 to c).flatMap(d => List(w1(d), w2(d), w3(d))))
      // the staircase master: each selector tuple's budget Σ_j v_j − 2C caps the weighted sum
      def emitStairMaster(j: Int, acc: List[Int], vSum: Int): Unit =
        if j == k then
          val b = vSum - 2 * c
          if b < 0 then sink.clause(acc.map(-_))
          else if b + 1 <= 3 * c then sink.clause(-wSum(b + 1) :: acc.map(-_))
        else for i <- values.indices do emitStairMaster(j + 1, sels(j)(i) :: acc, vSum + values(i))
      emitStairMaster(0, Nil, 0)

    Encoding(pv.toMap, next)

  /** Enumerate ALL models with SAT4J, blocking on the pair variables — same contract as
    * [[K2Certify.enumerate]], with the class count as a parameter.
    */
  def enumerate(
      c: Int,
      k: Int,
      baseSink: ClauseSink = NullSink,
      blockingSink: ClauseSink = NullSink,
      onModel: Array[Int] => Unit = _ => (),
      staircase: Boolean = false
  ): List[DSet] =
    val solver  = PlatformSolver.default(timeoutSeconds = 36000)
    val out     = mutable.ListBuffer.empty[DSet]
    val encSink = if baseSink eq NullSink then SolverSink(solver) else TeeSink(baseSink, SolverSink(solver))
    try
      val enc = encode(c, k, encSink, staircase)
      var go  = true
      while go && solver.solve() do
        val model    = solver.model()
        val trues    = model.filter(_ > 0).toSet
        val chosen   = enc.x.collect { case (_, v) if trues(v) => v }.toSeq.sorted
        val a        = Array.ofDim[Int](c + 1, 3)
        for ((i, p, qq), v) <- enc.x if trues(v) do
          a(p)(i) = qq
          a(qq)(i) = p
        out += new DSet(a)
        onModel(model)
        val blocking = chosen.map(-_)
        blockingSink.clause(blocking)
        try solver.addClause(blocking)
        catch case _: SatSolver.Contradiction => go = false
    catch case _: SatSolver.Contradiction => ()
    finally solver.close()
    out.toList
