package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.solver.Certification.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** Direct teeth for the certification leaves — until now covered only through the tools-guarded
  * `CertifyRunner` end-to-end path, so on a machine without kissat/drat-trim NONE of this surface ran. These
  * are platform-neutral and always run: the sink contracts (pairwise auxiliary-free exactly-one, the
  * trivial-refutation detector), the DIMACS emit/assemble/parse round-trip, the model-evaluation checks, and
  * the frame-key format and hash pins (artifact directory names are part of the recorded manifests — the
  * 16-hex-char SHA-256 prefix must never drift).
  */
class CertificationSpec extends AnyFlatSpec with Matchers:

  "ExpandingSink.exactlyOne" should "expand pairwise with NO auxiliary variables" in:
    val sink = BufferSink()
    sink.exactlyOne(Array(1, 2, 3))
    sink.clauses.toList shouldBe List(Seq(1, 2, 3), List(-1, -2), List(-1, -3), List(-2, -3))

  "CountingSink" should "detect the empty clause (the trivial-refutation fast path)" in:
    val sink = CountingSink()
    sink.sawEmptyClause shouldBe false
    sink.clause(List(1, -3))
    sink.sawEmptyClause shouldBe false
    sink.maxVar shouldBe 3
    sink.clause(Nil)
    sink.sawEmptyClause shouldBe true

  it should "track maxVar across clause and exactlyOne without expanding" in:
    val sink = CountingSink()
    sink.clause(List(-2, 5))
    sink.exactlyOne(Array(1, 7, 3))
    sink.maxVar shouldBe 7
    // detection-only contract: exactlyOne counts as ONE emission, deliberately unexpanded (O(1)) —
    // NOT the ExpandingSink clause count; header sizing must use the emitting sink's own counters
    sink.clauseCount shouldBe 2

  "DimacsSink + assemble + parseCnf" should "round-trip a clause stream" in:
    val dir             = Files.createTempDirectory("certification-spec")
    val body            = dir.resolve("test.body")
    val (maxVar, count) = scala.util.Using.resource(DimacsSink(body)) { sink =>
      sink.clause(List(1, -2))
      sink.exactlyOne(Array(2, 3))
      (sink.maxVar, sink.clauseCount)
    }
    maxVar shouldBe 3
    count shouldBe 3 // 1 emitted + exactly-one expanded to at-least-one + one at-most-one pair
    val cnf = dir.resolve("test.cnf")
    assemble(cnf, maxVar, count, body)
    Files.readAllLines(cnf).get(0) shouldBe "p cnf 3 3"
    parseCnf(cnf).map(_.toList).toList shouldBe List(List(1, -2), List(2, 3), List(-2, -3))

  "clauseSatisfied / violatedClauses" should "evaluate models with absent-variables-as-false semantics" in:
    val model = Array(1, -2, 3)
    clauseSatisfied(List(-2), model) shouldBe true
    clauseSatisfied(List(2), model) shouldBe false
    clauseSatisfied(List(4), model) shouldBe false // beyond the model: false
    clauseSatisfied(List(-4), model) shouldBe true // its negation: true
    val clauses = Array(Array(1, 2), Array(2), Array(-1, -3), Array(-4))
    violatedClauses(clauses, model) shouldBe Vector(1, 2)

  it should "agree between the pre-parsed and Path overloads" in:
    val dir   = Files.createTempDirectory("certification-spec-path")
    val cnf   = dir.resolve("check.cnf")
    Files.writeString(cnf, "p cnf 3 3\n1 2 0\n2 0\n-1 -3 0\n")
    val model = Array(1, -2, 3)
    violatedClauses(cnf, model) shouldBe violatedClauses(parseCnf(cnf), model)

  "frameKey / frameKeyHash" should "pin the manifest key format and the 16-hex-char hash prefix" in:
    frameKey(List(List(3, 12, 12)), Vector.empty) shouldBe "[3.12.12]|"
    frameKey(List(List(3, 12, 12), List(4, 8, 8)), Vector.empty) shouldBe "[3.12.12;4.8.8]|"
    // golden values computed independently (`printf %s <key> | shasum -a 256 | cut -c1-16`)
    frameKeyHash("test") shouldBe "9f86d081884c7d65"
    frameKeyHash("[3.12.12]|") shouldBe "2885f87b6bc67b12"
