package io.github.scala_tessella.research_core.solver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Using

/** The [[SatSolver]] CONTRACT, tested against the platform's live solver — SAT4J on the JVM, CaDiCaL via
  * IPASIR on Scala Native — so both implementations answer to the same spec: variable-ordered model
  * semantics, root-level UNSAT in either style the contract admits (eager [[SatSolver.Contradiction]] or lazy
  * `solve() == false`), incremental blocking-clause re-solve (the enumerators' loop), and the time budget
  * surfacing as [[SatSolver.Timeout]] on a resolution-hard instance.
  */
class PlatformSolverSpec extends AnyFlatSpec with Matchers:

  "the platform SatSolver" should "solve a satisfiable instance with variable-ordered model semantics" in:
    Using.resource(PlatformSolver.default()) { s =>
      s.addClause(List(1, 2))
      s.addClause(List(-1))
      s.solve() shouldBe true
      val m = s.model()
      m(0) shouldBe -1 // entry i is the literal of variable i + 1
      m(1) shouldBe 2
    }

  it should "report root-level UNSAT eagerly (Contradiction) or lazily (solve() == false)" in:
    Using.resource(PlatformSolver.default()) { s =>
      val addedWithoutThrowing =
        try
          s.addClause(List(1))
          s.addClause(List(-1))
          true
        catch case _: SatSolver.Contradiction => false // the eager style — SAT4J's
      if addedWithoutThrowing then s.solve() shouldBe false // the lazy style — IPASIR's
    }

  it should "re-solve incrementally after blocking clauses, exhausting exactly-one over two variables" in:
    Using.resource(PlatformSolver.default()) { s =>
      s.exactlyOne(Array(1, 2))
      s.solve() shouldBe true
      val first = s.model().filter(_ > 0).toList
      s.addClause(first.map(-_)) // block the found model
      s.solve() shouldBe true
      val second = s.model().filter(_ > 0).toList
      second should not be first
      s.addClause(second.map(-_))
      s.solve() shouldBe false // exactly-one over two variables has exactly two models
    }

  it should "surface the time budget as SatSolver.Timeout" in:
    // pigeonhole PHP(15, 14): resolution-hard, far beyond a 1 s budget; var p(i,j) = (i-1)*holes + j
    val pigeons           = 15
    val holes             = 14
    def p(i: Int, j: Int) = (i - 1) * holes + j
    val t0                = System.currentTimeMillis()
    Using.resource(PlatformSolver.default(timeoutSeconds = 1)) { s =>
      for i <- 1 to pigeons do s.addClause((1 to holes).map(p(i, _)))
      for j <- 1 to holes; i <- 1 to pigeons; i2 <- i + 1 to pigeons do
        s.addClause(List(-p(i, j), -p(i2, j)))
      a[SatSolver.Timeout] should be thrownBy s.solve()
    }
    // termination must come from the budget, not from the instance being solved or the suite hanging
    (System.currentTimeMillis() - t0) should be < 60000L
