package io.github.scala_tessella.research_core.solver

import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.{ContradictionException, ISolver}

/** The JVM [[SatSolver]]: SAT4J's default MiniSat-style solver, exactly as the enumerators have always
  * configured it. `ContradictionException` is translated to the solver-agnostic
  * [[SatSolver.Contradiction]] and `TimeoutException` to [[SatSolver.Timeout]]; `exactlyOne` keeps SAT4J's
  * native cardinality constraint.
  */
final class Sat4jSolver private (val underlying: ISolver, timeoutSeconds: Int) extends SatSolver:

  def addClause(lits: Seq[Int]): Unit =
    try underlying.addClause(new VecInt(lits.toArray))
    catch case _: ContradictionException => throw new SatSolver.Contradiction

  def exactlyOne(lits: Array[Int]): Unit =
    try underlying.addExactly(new VecInt(lits), 1)
    catch case _: ContradictionException => throw new SatSolver.Contradiction

  def solve(): Boolean =
    try underlying.isSatisfiable
    catch case _: org.sat4j.specs.TimeoutException => throw new SatSolver.Timeout(timeoutSeconds)

  def model(): Array[Int] = underlying.model()

object Sat4jSolver:

  def apply(timeoutSeconds: Int = 3600): Sat4jSolver =
    val s = SolverFactory.newDefault()
    s.setTimeout(timeoutSeconds)
    new Sat4jSolver(s, timeoutSeconds)

/** The JVM default live solver behind the shared enumerators. */
private[solver] object PlatformSolver:
  def default(timeoutSeconds: Int = 3600): SatSolver = Sat4jSolver(timeoutSeconds)

/** SAT4J-specific [[SymbolAssembly.ClauseSink]], kept for downstream source compatibility (formerly
  * `SymbolAssembly.Sat4jSink` — now JVM-only, since `SymbolAssembly` cross-compiles). May throw
  * `ContradictionException` mid-stream (trivially UNSAT — callers catch); certification sinks must not.
  */
final class Sat4jSink(solver: ISolver) extends SymbolAssembly.ClauseSink:
  def clause(lits: Seq[Int]): Unit       = solver.addClause(new VecInt(lits.toArray))
  def exactlyOne(lits: Array[Int]): Unit = solver.addExactly(new VecInt(lits), 1)
