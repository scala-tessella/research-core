package io.github.scala_tessella.research_core.solver

import scala.scalanative.unsafe.*

/** The IPASIR incremental-SAT C interface (reentrant subset the enumerators need), provided by CaDiCaL.
  * Literal conventions are DIMACS: `ipasir_add` streams clause literals terminated by 0; `ipasir_solve`
  * returns 10 (SAT) / 20 (UNSAT) / 0 (interrupted); `ipasir_val(v)` returns `v` (true), `-v` (false) or 0
  * (unassigned).
  */
@link("cadical")
@extern
private object Ipasir:
  def ipasir_init(): Ptr[Byte]                            = extern
  def ipasir_release(solver: Ptr[Byte]): Unit             = extern
  def ipasir_add(solver: Ptr[Byte], litOrZero: CInt): Unit = extern
  def ipasir_solve(solver: Ptr[Byte]): CInt               = extern
  def ipasir_val(solver: Ptr[Byte], lit: CInt): CInt      = extern

/** The Scala Native [[SatSolver]]: CaDiCaL in-process through IPASIR — no process spawn, no DIMACS
  * round-trip. IPASIR detects contradictions lazily, so [[SatSolver.Contradiction]] is never thrown and
  * root-level UNSAT surfaces as `solve() == false` (the callers' contract covers both behaviors).
  * `exactlyOne` expands PAIRWISE with no auxiliary variables — the numbering-identical contract shared with
  * the DIMACS `ExpandingSink`, which on this platform also makes the live encoding literally identical to
  * the certified one. `timeoutSeconds` is currently not enforced (IPASIR terminate callbacks are not wired
  * yet); the parameter is kept for signature parity with the JVM solver.
  */
final class CadicalSolver private (handle: Ptr[Byte]) extends SatSolver:

  private var nVars    = 0
  private var released = false

  private def add(lit: Int): Unit =
    val a = math.abs(lit)
    if a > nVars then nVars = a
    Ipasir.ipasir_add(handle, lit)

  def addClause(lits: Seq[Int]): Unit =
    lits.foreach(add)
    Ipasir.ipasir_add(handle, 0)

  def exactlyOne(lits: Array[Int]): Unit =
    addClause(lits.toSeq)
    for i <- lits.indices; j <- i + 1 until lits.length do addClause(List(-lits(i), -lits(j)))

  def solve(): Boolean = Ipasir.ipasir_solve(handle) match
    case 10    => true
    case 20    => false
    case other => throw new IllegalStateException(s"ipasir_solve returned $other (interrupted?)")

  def model(): Array[Int] = Array.tabulate(nVars) { i =>
    val v = i + 1
    val r = Ipasir.ipasir_val(handle, v)
    if r == 0 then -v else r // unassigned counts as false, matching the JVM model semantics
  }

  override def close(): Unit =
    if !released then
      released = true
      Ipasir.ipasir_release(handle)

object CadicalSolver:
  def apply(timeoutSeconds: Int = 3600): CadicalSolver = new CadicalSolver(Ipasir.ipasir_init())
