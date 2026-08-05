package io.github.scala_tessella.research_core.solver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Using

/** Direct teeth for the IPASIR binding (the shared gate specs exercise it end to end; these pin the leaf
  * semantics): solve/model conventions, incremental re-solve after a blocking clause, and the terminate
  * callback enforcing `timeoutSeconds` on a resolution-hard pigeonhole instance.
  */
class CadicalSolverSpec extends AnyFlatSpec with Matchers:

  "CadicalSolver" should "solve a satisfiable instance with variable-ordered model semantics" in:
    Using.resource(CadicalSolver()) { s =>
      s.addClause(List(1, 2))
      s.addClause(List(-1))
      s.solve() shouldBe true
      val m = s.model()
      m(0) shouldBe -1 // entry i is the literal of variable i + 1
      m(1) shouldBe 2
    }

  it should "report UNSAT lazily (contradiction surfaces at solve, never as an exception)" in:
    Using.resource(CadicalSolver()) { s =>
      s.addClause(List(1))
      noException should be thrownBy s.addClause(List(-1))
      s.solve() shouldBe false
    }

  it should "re-solve incrementally after a blocking clause" in:
    Using.resource(CadicalSolver()) { s =>
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

  it should "enforce timeoutSeconds through the IPASIR terminate callback" in:
    // pigeonhole PHP(15, 14): resolution-hard, far beyond a 1 s budget; var p(i,j) = (i-1)*holes + j
    val pigeons        = 15
    val holes          = 14
    def p(i: Int, j: Int) = (i - 1) * holes + j
    val t0             = System.currentTimeMillis()
    Using.resource(CadicalSolver(timeoutSeconds = 1)) { s =>
      for i <- 1 to pigeons do s.addClause((1 to holes).map(p(i, _)))
      for j <- 1 to holes; i <- 1 to pigeons; i2 <- i + 1 to pigeons do
        s.addClause(List(-p(i, j), -p(i2, j)))
      a[SatSolver.Timeout] should be thrownBy s.solve()
    }
    // termination must come from the deadline, not from the instance being solved or the suite hanging
    (System.currentTimeMillis() - t0) should be < 60000L
