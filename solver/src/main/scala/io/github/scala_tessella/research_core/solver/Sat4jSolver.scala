package io.github.scala_tessella.research_core.solver

import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.{ContradictionException, ISolver}

/** The JVM [[SatSolver]]: SAT4J's default MiniSat-style solver, exactly as the enumerators have always
  * configured it. `ContradictionException` is translated to the solver-agnostic
  * [[SatSolver.Contradiction]]; `exactlyOne` keeps SAT4J's native cardinality constraint.
  */
final class Sat4jSolver private (val underlying: ISolver) extends SatSolver:

  def addClause(lits: Seq[Int]): Unit =
    try underlying.addClause(new VecInt(lits.toArray))
    catch case _: ContradictionException => throw new SatSolver.Contradiction

  def exactlyOne(lits: Array[Int]): Unit =
    try underlying.addExactly(new VecInt(lits), 1)
    catch case _: ContradictionException => throw new SatSolver.Contradiction

  def solve(): Boolean = underlying.isSatisfiable

  def model(): Array[Int] = underlying.model()

object Sat4jSolver:

  def apply(timeoutSeconds: Int = 3600): Sat4jSolver =
    val s = SolverFactory.newDefault()
    s.setTimeout(timeoutSeconds)
    new Sat4jSolver(s)
