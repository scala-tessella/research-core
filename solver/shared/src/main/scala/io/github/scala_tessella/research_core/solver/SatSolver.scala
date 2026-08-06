package io.github.scala_tessella.research_core.solver

/** The incremental CDCL surface the enumerators actually use — extracted so the live solver can be SAT4J on
  * the JVM and an IPASIR binding (CaDiCaL) on Scala Native. Deliberately free of any solver import: this file
  * is platform-neutral.
  *
  * Contract:
  *   - variables are positive ints, clauses are non-empty literal lists (DIMACS convention);
  *   - `addClause`/`exactlyOne` may throw [[SatSolver.Contradiction]] if the solver detects root-level
  *     inconsistency EAGERLY (SAT4J does); lazy solvers (IPASIR) may never throw — the contradiction then
  *     surfaces as `solve() == false`. Callers must handle both (the prefix-UNSAT soundness argument of
  *     [[SymbolAssembly.enumerateSigma0]] covers the eager abort);
  *   - `model()` is only valid after `solve()` returned true and is VARIABLE-ORDERED: entry `i` is the
  *     literal of variable `i + 1` (±(i+1)) — blocking-clause extraction relies on this;
  *   - `exactlyOne` uses the solver's native cardinality constraint if it has one, else the same
  *     auxiliary-free pairwise expansion as the DIMACS sink (variable numbering must stay aligned).
  */
trait SatSolver extends AutoCloseable:
  def addClause(lits: Seq[Int]): Unit
  def exactlyOne(lits: Array[Int]): Unit
  def solve(): Boolean
  def model(): Array[Int]
  def close(): Unit = ()

object SatSolver:

  /** Solver-agnostic replacement for SAT4J's `ContradictionException`: adding the clause made the formula
    * root-level UNSAT. Stackless — it is pure control flow in the enumeration loops.
    */
  final class Contradiction
      extends RuntimeException(null, null, false, false) // scalafix:ok DisableSyntax.null

  /** `solve()` hit the solver's time budget — SAT4J's `TimeoutException` (JVM) and the IPASIR terminate
    * callback (Native), translated to one platform-neutral signal. Deliberately NOT caught by the
    * enumerators: a timed-out enumeration is not a completed one, so it must propagate.
    */
  final class Timeout(timeoutSeconds: Int)
      extends RuntimeException(s"SAT solve exceeded the $timeoutSeconds s budget")

  /** [[SymbolAssembly.ClauseSink]] view of a live solver — the generic successor of `Sat4jSink`. */
  final class SolverSink(solver: SatSolver) extends SymbolAssembly.ClauseSink:
    def clause(lits: Seq[Int]): Unit       = solver.addClause(lits)
    def exactlyOne(lits: Array[Int]): Unit = solver.exactlyOne(lits)
