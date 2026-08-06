package io.github.scala_tessella.research_core

import MonoShell.Flags
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** JVM-only smoke over the honeycomb result engines that are otherwise asserted only downstream
  * (`convex-uniform-honeycombs`): the full completeness audit re-closes everything with zero flags — one run
  * covers `CompletenessAudit`, `TransitivePatterns`, `MonoShell` and the certification chain — and
  * `SymbolRealization` derives the minimal symbols of a sample of species with its internal loud checks
  * (symbol axioms, minimality, stabilizer subgroup) passing and the class counts matching the audit. JVM-only
  * placement keeps the Native suite's wall time flat; the engines are platform-neutral code already compiled
  * (not linked) on Native.
  */
class HoneycombGateSmokeSpec extends AnyFlatSpec with Matchers:

  "the completeness audit" should "close every skeleton and raise no flags (26 species, 28 classes)" in:
    val (audits, flags) = CompletenessAudit.results
    flags shouldBe empty
    audits.size shouldBe 26
    audits.map(_.classes).sum shouldBe 28
    audits.forall(_.ok) shouldBe true

  "SymbolRealization" should "derive valid minimal symbols with the audit's class counts (sample)" in:
    val flags = Flags()
    // species 0..2 carry one class each; species 25 is one of the two doubled species (audit table)
    for (i, expected) <- List(0 -> 1, 1 -> 1, 2 -> 1, 25 -> 2) do
      withClue(s"species $i: ") {
        SymbolRealization.derivedSymbolsOf(i, flags).size shouldBe expected
      }
