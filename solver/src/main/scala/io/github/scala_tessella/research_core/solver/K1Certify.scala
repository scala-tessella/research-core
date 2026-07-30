package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import io.github.scala_tessella.research_core.solver.SymbolAssembly.{ClauseSink, NullSink, Sat4jSink, TeeSink}
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.ContradictionException

import scala.collection.mutable

/** ADR-0009 paper certification, track A — the SAT encoding behind the k = 1 completeness obligation: "there
  * is NO D-set on C chambers, with one vertex orbit, beyond the enumerated list". Search object: a LABELED
  * D-set on chambers 1..C —
  *
  *   - three involutions σ₀, σ₁, σ₂ as unordered pair variables (self-pairs = fixed points), exactly one
  *     partner per (i, chamber);
  *   - the 2-manifold axiom (σ₀σ₂)² = id as commutation clauses;
  *   - ONE vertex orbit: every proper chamber subset containing 1 is crossed by a σ₁/σ₂ edge (cut clauses —
  *     2^{C−1} of them, trivial at C ≤ 12);
  *   - BFS-CONSISTENT NUMBERING: chambers are numbered in first-seen scan order (slots (d, i) ordered by d
  *     then i; `seen` prefix chains, first occurrences strictly increasing in the chamber number). Each
  *     isomorphism class then appears once per valid start ([[DelaneySymbols.bfsRelabelings]]), never C!
  *     times, and blocking those relabelings blocks the class.
  *
  * No curvature arithmetic enters the CNF: euclidean filtering is the exact JVM tail
  * ([[DelaneySymbols.euclideanSymbolsOf]]). Base + blocking UNSAT with a DRAT proof = completeness.
  */
object K1Certify:

  final case class Encoding(x: Map[(Int, Int, Int), Int], maxVar: Int):
    /** The σᵢ(a) = b pair variable (unordered). */
    def apply(i: Int, a: Int, b: Int): Int = x((i, math.min(a, b), math.max(a, b)))

  /** Emit the C-chamber CNF into `sink`; returns the pair-variable map (auxiliaries follow it). */
  def encode(c: Int, sink: ClauseSink): Encoding =
    val pv                        = mutable.LinkedHashMap.empty[(Int, Int, Int), Int]
    var next                      = 0
    for i <- 0 to 2; a <- 1 to c; b <- a to c do { next += 1; pv((i, a, b)) = next }
    val enc                       = Encoding(pv.toMap, next)
    def x(i: Int, a: Int, b: Int) = enc(i, a, b)
    // involutions: exactly one partner (possibly self) per chamber and op
    for i <- 0 to 2; d <- 1 to c do sink.exactlyOne((1 to c).map(e => x(i, d, e)).distinct.toArray)
    // (σ₀σ₂)² = id: σ₂(a)=b ∧ σ₀(b)=e ∧ σ₀(a)=d → σ₂(d)=e
    for a <- 1 to c; b <- 1 to c; e <- 1 to c; d <- 1 to c do
      val lits = List(-x(2, a, b), -x(0, b, e), -x(0, a, d), x(2, d, e))
      if lits.distinct.size == lits.size then sink.clause(lits)
    // one vertex orbit: every proper S ∋ 1 has an outgoing σ₁/σ₂ edge
    for mask <- 0 until (1 << (c - 1)) - 1 do
      def inS(d: Int) = d == 1 || ((mask >> (d - 2)) & 1) == 1
      sink.clause(
        (for d <- 1 to c if inS(d); e <- 1 to c if !inS(e); i <- 1 to 2 yield x(i, d, e)).distinct.toList
      )
    // BFS numbering: seen(m)(s) ⟺ m produced by some slot ≤ s of a chamber < m; slots (d,i) → 3(d−1)+i
    if c >= 2 then
      val seen                        = Array.tabulate(c + 1)(m => if m < 2 then Array.empty[Int] else new Array[Int](3 * (m - 1)))
      for m <- 2 to c; s <- 0 until 3 * (m - 1) do { next += 1; seen(m)(s) = next }
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
        sink.clause(List(seen(m)(cap - 1))) // every m ≥ 2 is produced by a smaller chamber
      // first occurrences strictly increasing: seenAt(m+1, s) → seenAt(m, s)
      def seenAt(m: Int, s: Int): Int = seen(m)(math.min(s, 3 * (m - 1) - 1))
      for m <- 2 until c; s <- 0 until 3 * m do sink.clause(List(-seenAt(m + 1, s), seenAt(m, s)))
    Encoding(pv.toMap, next)

  /** Enumerate ALL models with SAT4J (blocking on the true pair variables — the auxiliaries are functionally
    * determined), tee-ing the encoding into `baseSink` and each blocking clause into `blockingSink`; models
    * are decoded to labeled [[DSet]]s.
    */
  def enumerate(
      c: Int,
      baseSink: ClauseSink = NullSink,
      blockingSink: ClauseSink = NullSink,
      onModel: Array[Int] => Unit = _ => ()
  ): List[DSet] =
    val solver  = SolverFactory.newDefault()
    solver.setTimeout(3600)
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
        for ((i, p, q), v) <- enc.x if trues(v) do
          a(p)(i) = q
          a(q)(i) = p
        out += new DSet(a)
        onModel(model)
        val blocking = chosen.map(-_)
        blockingSink.clause(blocking)
        try solver.addClause(new VecInt(blocking.toArray))
        catch case _: ContradictionException => go = false
    catch case _: ContradictionException => ()
    out.toList
