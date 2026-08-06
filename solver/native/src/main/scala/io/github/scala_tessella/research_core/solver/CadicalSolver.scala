package io.github.scala_tessella.research_core.solver

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*

// Ptr reinterpretation casts are the C-interop idiom here (IPASIR's void* handle and callback state);
// pattern matching cannot express them
// scalafix:off DisableSyntax.asInstanceOf

/** The IPASIR incremental-SAT C interface (reentrant subset the enumerators need), provided by CaDiCaL.
  * Literal conventions are DIMACS: `ipasir_add` streams clause literals terminated by 0; `ipasir_solve`
  * returns 10 (SAT) / 20 (UNSAT) / 0 (interrupted); `ipasir_val(v)` returns `v` (true), `-v` (false) or 0
  * (unassigned); `ipasir_set_terminate` installs a callback polled during solving — non-zero return
  * terminates the run.
  */
@link("cadical")
@extern
private object Ipasir:
  def ipasir_init(): Ptr[Byte]                             = extern
  def ipasir_release(solver: Ptr[Byte]): Unit              = extern
  def ipasir_add(solver: Ptr[Byte], litOrZero: CInt): Unit = extern
  def ipasir_solve(solver: Ptr[Byte]): CInt                = extern
  def ipasir_val(solver: Ptr[Byte], lit: CInt): CInt       = extern
  def ipasir_set_terminate(
      solver: Ptr[Byte],
      state: Ptr[Byte],
      terminate: CFuncPtr1[Ptr[Byte], CInt]
  ): Unit                                                  = extern

/** The Scala Native [[SatSolver]]: CaDiCaL in-process through IPASIR — no process spawn, no DIMACS
  * round-trip. IPASIR detects contradictions lazily, so [[SatSolver.Contradiction]] is never thrown and
  * root-level UNSAT surfaces as `solve() == false` (the callers' contract covers both behaviors).
  * `exactlyOne` expands PAIRWISE with no auxiliary variables — the numbering-identical contract shared with
  * the DIMACS `ExpandingSink`, which on this platform also makes the live encoding literally identical to the
  * certified one. `timeoutSeconds` is enforced through the IPASIR terminate callback (the deadline lives in
  * malloc'd state, because a C function pointer cannot capture); expiry surfaces as [[SatSolver.Timeout]],
  * the counterpart of SAT4J's `TimeoutException` on the JVM.
  */
final class CadicalSolver private (handle: Ptr[Byte], deadline: Ptr[CLongLong], timeoutSeconds: Int)
    extends SatSolver:

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
    case 10 => true
    case 20 => false
    case _  => throw new SatSolver.Timeout(timeoutSeconds)

  def model(): Array[Int] = Array.tabulate(nVars) { i =>
    val v = i + 1
    val r = Ipasir.ipasir_val(handle, v)
    if r == 0 then -v else r // unassigned counts as false, matching the JVM model semantics
  }

  override def close(): Unit =
    if !released then
      released = true
      Ipasir.ipasir_release(handle)
      stdlib.free(deadline.asInstanceOf[Ptr[Byte]])

object CadicalSolver:

  /** Non-capturing by construction: everything it needs arrives through `state` (the deadline in epoch
    * millis). Polled by CaDiCaL during solving; returning non-zero terminates the run.
    */
  private val pastDeadline: CFuncPtr1[Ptr[Byte], CInt] =
    (state: Ptr[Byte]) =>
      if System.currentTimeMillis() >= !state.asInstanceOf[Ptr[CLongLong]] then 1 else 0

  def apply(timeoutSeconds: Int = 3600): CadicalSolver =
    val handle   = Ipasir.ipasir_init()
    val deadline = stdlib.malloc(sizeof[CLongLong]).asInstanceOf[Ptr[CLongLong]]
    !deadline = System.currentTimeMillis() + timeoutSeconds.toLong * 1000
    Ipasir.ipasir_set_terminate(handle, deadline.asInstanceOf[Ptr[Byte]], pastDeadline)
    new CadicalSolver(handle, deadline, timeoutSeconds)
