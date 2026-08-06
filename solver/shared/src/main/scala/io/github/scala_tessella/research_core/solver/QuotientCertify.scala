package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.solver.SymbolAssembly.ClauseSink

import java.nio.file.Path
import scala.collection.mutable

/** ADR-0009 paper certification, track C — the SAT encoding behind the quotient-list completeness obligation:
  * "symbol S has no proper m-constant op-congruence avoiding every listed generator". A model is a
  * congruence, directly propositional — variables `e(a,b)` (chambers identified) with
  *
  *   - TRANSITIVITY over all chamber triples;
  *   - OP-CONGRUENCE: a ~ b → σᵢ(a) ~ σᵢ(b);
  *   - M-CONSTANCY: unit clauses ¬e(a,b) wherever m₀₁ or m₁₂ differ;
  *   - PROPER: at least one pair identified;
  *   - ¬e(1, d₀) for every listed first-step generator.
  *
  * UNSAT ⇒ every proper m-constant op-congruence contains a listed generator (its closure is then forced
  * inside the congruence by the constraints), so every quotient of S is dominated by a listed first-step
  * quotient — the exactness criterion's input list is complete. Non-listed generators need no handling: their
  * closures contain an m-conflicting pair, so e(1, d) is already unsatisfiable. For a MINIMAL symbol the list
  * is empty and the obligation certifies minimality itself. The instance-wise UNSAT also absorbs the
  * constant-fiber lemma (no congruence can leave chamber 1 in a singleton class).
  */
object QuotientCertify:

  /** The first-step generators: d₀ whose single-pair closure `1 ~ d₀` is m-constant — the same union-find
    * closure as `reduceOnce`/`properQuotients`, recomputed here so the certificate does not import the code
    * under certification.
    */
  def generators(ds: DSymbol): Set[Int] =
    val n = ds.size
    (2 to n).filter { d0 =>
      val parent                = Array.tabulate(n + 1)(identity)
      def find(x: Int): Int     = { var r = x; while parent(r) != r do r = parent(r); r }
      def union(a: Int, b: Int) =
        val (ra, rb) = (find(a), find(b))
        if ra == rb then false else { parent(ra) = rb; true }
      val queue                 = mutable.Queue((1, d0))
      union(1, d0): Unit
      while queue.nonEmpty do
        val (a, b) = queue.dequeue()
        for i <- 0 to 2 do
          val (ai, bi) = (ds.get(i, a), ds.get(i, b))
          if union(ai, bi) then queue.enqueue((ai, bi))
      (1 to n).groupBy(find).values.forall { cls =>
        cls.map(d => ds.m(0, 1, d)).distinct.size == 1 && cls.map(d => ds.m(1, 2, d)).distinct.size == 1
      }
    }.toSet

  /** The closure of `1 ~ d₀` as a pair-variable assignment (for the fidelity and SAT-direction gates). */
  def closureModel(ds: DSymbol, d0: Int, pv: Map[(Int, Int), Int]): Array[Int] =
    val n                     = ds.size
    val parent                = Array.tabulate(n + 1)(identity)
    def find(x: Int): Int     = { var r = x; while parent(r) != r do r = parent(r); r }
    def union(a: Int, b: Int) =
      val (ra, rb) = (find(a), find(b))
      if ra == rb then false else { parent(ra) = rb; true }
    val queue                 = mutable.Queue((1, d0))
    union(1, d0): Unit
    while queue.nonEmpty do
      val (a, b) = queue.dequeue()
      for i <- 0 to 2 do
        val (ai, bi) = (ds.get(i, a), ds.get(i, b))
        if union(ai, bi) then queue.enqueue((ai, bi))
    pv.toArray.map((p, v) => if find(p._1) == find(p._2) then v else -v)

  /** Emit the obligation CNF (`excluded` = the generators barred by constraint 5); returns the pair map. */
  def encode(ds: DSymbol, excluded: Set[Int], sink: ClauseSink): Map[(Int, Int), Int] =
    val n                              = ds.size
    val pairs                          = for a <- 1 to n; b <- a + 1 to n yield (a, b)
    val pv                             = pairs.zipWithIndex.map((p, i) => p -> (i + 1)).toMap
    def e(a: Int, b: Int): Option[Int] =
      if a == b then None else Some(pv(if a < b then (a, b) else (b, a)))
    for (a, b) <- pairs do
      // m-constancy
      if ds.m(0, 1, a) != ds.m(0, 1, b) || ds.m(1, 2, a) != ds.m(1, 2, b) then
        sink.clause(List(-pv((a, b))))
      // op-congruence
      for i <- 0 to 2 do
        e(ds.get(i, a), ds.get(i, b)).foreach(w => if w != pv((a, b)) then sink.clause(List(-pv((a, b)), w)))
    // transitivity
    for a <- 1 to n; b <- a + 1 to n; c <- b + 1 to n do
      val (ab, bc, ac) = (pv((a, b)), pv((b, c)), pv((a, c)))
      sink.clause(List(-ab, -bc, ac))
      sink.clause(List(-ab, -ac, bc))
      sink.clause(List(-ac, -bc, ab))
    // proper
    sink.clause(pairs.map(pv).toList)
    // avoid every listed generator
    for d0 <- excluded do sink.clause(List(-pv((1, d0))))
    pv

  /** kissat satisfiability verdict (exit 10 = SAT, 20 = UNSAT) — the SAT-direction gate needs SAT. */
  def kissatSat(cnf: Path): Boolean =
    import cats.effect.unsafe.implicits.global
    CertifyRunner.kissatSatIO(cnf).unsafeRunSync()
