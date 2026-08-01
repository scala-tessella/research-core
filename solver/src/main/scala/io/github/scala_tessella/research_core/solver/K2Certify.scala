package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols
import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import io.github.scala_tessella.research_core.solver.SymbolAssembly.{ClauseSink, NullSink, Sat4jSink, TeeSink}
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.ContradictionException

import scala.collection.mutable

/** ADR-0009 paper certification, track A2 — the SAT encoding behind the k ≤ 2 completeness obligation: "there
  * is NO D-set on C chambers, with ≤ 2 vertex orbits, satisfying the tier-1 curvature relaxation, beyond the
  * enumerated list". Search object: a LABELED D-set on chambers 1..C, as in [[K1Certify]] — involutions as
  * unordered pair variables, the 2-manifold axiom (σ₀σ₂)² = id as commutation clauses, and BFS-consistent
  * numbering (each isomorphism class appears once per valid start, [[DelaneySymbols.bfsRelabelings]]) — plus
  * two layers replacing / extending the k = 1 orbit constraint:
  *
  *   - AT MOST TWO vertex orbits. The 2^{C−1} cut clauses of [[K1Certify]] are infeasible at C = 24; instead:
  *     a 2-coloring `col` of the chambers with `col(1)` true, σ₁/σ₂-INVARIANT (so color classes are unions of
  *     vertex orbits); `anchor2(d)` exactly at the minimal chamber of the false class (its defining premises
  *     force uniqueness); and anchored LEVEL-REACHABILITY — `lev(d, t)` = "d within t σ₁/σ₂-steps of its
  *     class anchor", with one-directional step witnesses and the coverage units `lev(d, C−1)` — forcing each
  *     color class to be ONE orbit. A ≥ 3-orbit D-set admits no invariant 2-coloring with both classes
  *     connected, and a genuine ≤ 2-orbit labeling extends by coloring the orbit of chamber 1 true.
  *   - THE TIER-1 CURVATURE RELAXATION ([[DelaneySymbols.tier1Feasible]], the lemma there): #good ≥ 3C −
  *     12·vSum, where good(d) ⟺ (σ₀σ₁)³(d) = d (⟺ d's tile orbit has r ∈ {1, 3}) and 12·vSum is the
  *     vertex-orbit contribution read off the two color classes (cardinality + chain flag → the 4/6/12 /
  *     8/12/24 table). `good` is certified one-directionally through witness chains (spuriously true is
  *     impossible; genuinely true is realizable), cardinalities through exact unary counters, and the class
  *     contributions through selectors whose every case implies its certifying structure — so a model can
  *     never overstate vSum or #good, and every tier-1 labeling extends to a model. This is what lets
  *     curvature into the CNF without orbit-length machinery, and it cuts the top chamber counts from ~10⁸
  *     raw D-sets to the enumerable tier-1 universe.
  *
  * No exact curvature arithmetic enters the CNF: euclidean filtering is the exact JVM tail
  * ([[DelaneySymbols.euclideanSymbolsOf]]), bridged by the tier-1 lemma (euclidean-feasible ⇒ tier-1). Base +
  * blocking UNSAT with a DRAT proof = completeness of the tier-1 universe.
  */
object K2Certify:

  final case class Encoding(x: Map[(Int, Int, Int), Int], maxVar: Int):
    /** The σᵢ(a) = b pair variable (unordered). */
    def apply(i: Int, a: Int, b: Int): Int = x((i, math.min(a, b), math.max(a, b)))

  /** Emit the C-chamber CNF into `sink`; returns the pair-variable map (auxiliaries follow it). */
  def encode(c: Int, sink: ClauseSink): Encoding =
    val pv                        = mutable.LinkedHashMap.empty[(Int, Int, Int), Int]
    var next                      = 0
    def fresh(): Int              = { next += 1; next }
    for i <- 0 to 2; a <- 1 to c; b <- a to c do pv((i, a, b)) = fresh()
    val enc                       = Encoding(pv.toMap, next)
    def x(i: Int, a: Int, b: Int) = enc(i, a, b)

    // ---- K1 core: involutions, manifold axiom, BFS-consistent numbering -------------------------------
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

    // ---- at most two vertex orbits: invariant 2-coloring + anchored reachability ----------------------
    val col     = Array.tabulate(c + 1)(d => if d == 0 then 0 else fresh())
    sink.clause(List(col(1)))
    for i <- 1 to 2; a <- 1 to c; b <- a + 1 to c do
      sink.clause(List(-x(i, a, b), -col(a), col(b)))
      sink.clause(List(-x(i, a, b), col(a), -col(b)))
    val anchor2 = Array.tabulate(c + 1)(d => if d < 2 then 0 else fresh())
    for d <- 2 to c do
      sink.clause(List(-anchor2(d), -col(d)))
      for e <- 2 until d do sink.clause(List(-anchor2(d), col(e)))
    val tMax    = math.max(0, c - 1)
    val lev     = Array.tabulate(c + 1, tMax + 1)((d, _) => if d == 0 then 0 else fresh())
    sink.clause(List(lev(1)(0)))
    for d <- 2 to c do sink.clause(List(-lev(d)(0), anchor2(d)))
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

    // ---- tier-1 curvature: good(d) ⟺ (σ₀σ₁)³(d) = d, certified one-directionally ---------------------
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

    // ---- exact unary counters (sequential, both polarities) -------------------------------------------
    // thresholds(j) ⟺ at least j of `bits` are true, for j = 1..bits.length
    def unaryCounter(bits: IndexedSeq[Int]): Array[Int] =
      val n = bits.length
      val p = Array.ofDim[Int](n + 1, n + 1) // p(d)(j), 1 ≤ j ≤ d
      for d <- 1 to n; j <- 1 to d do p(d)(j) = fresh()
      // p(1)(1) ↔ bits(0)
      sink.clause(List(-p(1)(1), bits(0)))
      sink.clause(List(-bits(0), p(1)(1)))
      for d <- 2 to n; j <- 1 to d do
        val a     = if j <= d - 1 then p(d - 1)(j) else 0  // 0 = false
        val bprev = if j == 1 then -1 else p(d - 1)(j - 1) // -1 = true
        // → : p(d,j) → a ∨ (bprev ∧ bit(d))
        if bprev == -1 then
          if a == 0 then sink.clause(List(-p(d)(j), bits(d - 1)))
          else sink.clause(List(-p(d)(j), a, bits(d - 1)))
        else
          if a == 0 then
            sink.clause(List(-p(d)(j), bprev))
            sink.clause(List(-p(d)(j), bits(d - 1)))
          else
            sink.clause(List(-p(d)(j), a, bprev))
            sink.clause(List(-p(d)(j), a, bits(d - 1)))
        // ← : a → p(d,j);  bprev ∧ bit(d) → p(d,j)
        if a != 0 then sink.clause(List(-a, p(d)(j)))
        if bprev == -1 then sink.clause(List(-bits(d - 1), p(d)(j)))
        else sink.clause(List(-bprev, -bits(d - 1), p(d)(j)))
      Array.tabulate(n + 1)(j => if j == 0 then -1 else p(n)(j))

    val cardG = unaryCounter((1 to c).map(col))  // cardG(j) ⟺ |class 1| ≥ j
    val goodG = unaryCounter((1 to c).map(good)) // goodG(j) ⟺ #good ≥ j

    // ---- chain flags per class (exact, both directions) -----------------------------------------------
    // cf(class) ⟺ some chamber of the class is fixed by σ₁ or σ₂
    def chainFlag(classLit: Int => Int): Int =
      val cf = fresh()
      val ws =
        for d <- 1 to c yield
          val w = fresh()
          sink.clause(List(-w, classLit(d)))
          sink.clause(List(-w, x(1, d, d), x(2, d, d)))
          w
      sink.clause(-cf :: ws.toList)
      for d <- 1 to c; i <- 1 to 2 do sink.clause(List(-classLit(d), -x(i, d, d), cf))
      cf
    val cf1                                  = chainFlag(d => col(d))
    val cf2                                  = chainFlag(d => -col(d))

    // ---- per-class contribution selectors (each case implies its certifying structure) ----------------
    // cases: (name, 12·contribution): E empty 0; C1/C2/C3 chains of length 1/2/≥3 → 4/6/12;
    //        Y2/Y4/Y6 cycles of length 2/4/≥6 → 8/12/24
    val values                                 = Array(0, 4, 6, 12, 8, 12, 24)                    // E, C1, C2, C3, Y2, Y4, Y6
    // class-1 cardinality: ≥ j ⟺ cardG(j); = j ⟺ cardG(j) ∧ ¬cardG(j+1)
    // class-2 cardinality: ≥ j ⟺ ¬cardG(c−j+1); = j ⟺ ¬cardG(c−j+1) ∧ cardG(c−j)  (card₂ = C − card₁)
    def ge(cls: Int, j: Int): Int              = if cls == 1 then cardG(j) else -cardG(c - j + 1) // literal
    def lt(cls: Int, j: Int): Int              = -ge(cls, j)
    def emitSel(cls: Int, cf: Int): Array[Int] =
      val sel                                                                               = Array.fill(7)(fresh())
      sink.clause(sel.toList)
      for i <- sel.indices; j <- i + 1 until sel.length do sink.clause(List(-sel(i), -sel(j)))
      // E: class empty (class 1 never — contains chamber 1)
      if cls == 1 then sink.clause(List(-sel(0)))
      else sink.clause(List(-sel(0), lt(2, 1)))
      def sizedCase(s: Int, chain: Boolean, exact: Option[Int], atLeast: Option[Int]): Unit =
        sink.clause(List(-sel(s), if chain then cf else -cf))
        exact.foreach { L =>
          if L <= c then
            sink.clause(List(-sel(s), ge(cls, L)))
            if L + 1 <= c then sink.clause(List(-sel(s), lt(cls, L + 1)))
          else sink.clause(List(-sel(s)))
        }
        atLeast.foreach { L =>
          if L <= c then sink.clause(List(-sel(s), ge(cls, L)))
          else sink.clause(List(-sel(s)))
        }
      sizedCase(1, chain = true, Some(1), None)
      sizedCase(2, chain = true, Some(2), None)
      sizedCase(3, chain = true, None, Some(3))
      sizedCase(4, chain = false, Some(2), None)
      sizedCase(5, chain = false, Some(4), None)
      sizedCase(6, chain = false, None, Some(6))
      sel
    val sel1                                   = emitSel(1, cf1)
    val sel2                                   = emitSel(2, cf2)

    // ---- the tier-1 requirement: #good ≥ 3C − vc₁ − vc₂ -----------------------------------------------
    for i <- values.indices; j <- values.indices do
      val m = 3 * c - values(i) - values(j)
      if m > c then sink.clause(List(-sel1(i), -sel2(j)))
      else if m >= 1 then sink.clause(List(-sel1(i), -sel2(j), goodG(m)))

    Encoding(pv.toMap, next)

  /** Enumerate ALL models with SAT4J (blocking on the true pair variables — the auxiliaries carry no
    * information beyond them), tee-ing the encoding into `baseSink` and each blocking clause into
    * `blockingSink`; models are decoded to labeled [[DSet]]s. Identical contract to [[K1Certify.enumerate]].
    */
  def enumerate(
      c: Int,
      baseSink: ClauseSink = NullSink,
      blockingSink: ClauseSink = NullSink,
      onModel: Array[Int] => Unit = _ => ()
  ): List[DSet] =
    val solver  = SolverFactory.newDefault()
    solver.setTimeout(36000)
    val out     = mutable.ListBuffer.empty[DSet]
    val encSink = if baseSink eq NullSink then Sat4jSink(solver) else TeeSink(baseSink, Sat4jSink(solver))
    try
      val enc = encode(c, encSink)
      var go  = true
      while go && solver.isSatisfiable do
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
        try solver.addClause(new VecInt(blocking.toArray))
        catch case _: ContradictionException => go = false
    catch case _: ContradictionException => ()
    out.toList
