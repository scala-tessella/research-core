package io.github.scala_tessella.research_core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import MonoShell.Flags

/** Regression teeth for `searchPatterns`'s `capped` flag over the real species corpus. Ground truth for
  * "the cap actually truncated the search" is a re-run at cap + 1 finding more patterns. A false NEGATIVE
  * (truncated but reported uncapped) poisons the exhaustion certificate (`!capped && allKnown`) and MUST
  * never happen — the pre-fix code had 43/43 truncated corpus searches reported uncapped (the Some(v)
  * backtracking branch never set the flag, and real cap-hit unwinds pass only through it). A false
  * POSITIVE (complete but reported capped) merely wastes a re-run at a higher cap and is reported here
  * informationally.
  */
class CappedProbeSpec extends AnyFlatSpec with Matchers:

  "searchPatterns.capped" should "never report a truncated search as uncapped (species corpus, caps 1 and 40)" in:
    val flags    = Flags()
    var checked  = 0
    var falseNeg = 0
    var falsePos = 0
    val rows     = List.newBuilder[String]
    for idx <- SpeciesEnumerator.species.indices do
      val acc = TransitivePatterns.acceptedOf(idx, flags)
      for (domains, si) <- acc.skeletons.zipWithIndex; cap <- List(1, 40) do
        val (found, capped) = TransitivePatterns.searchPatterns(acc.g, domains, acc.stab, cap, _ => ())
        val (foundMore, _)  = TransitivePatterns.searchPatterns(acc.g, domains, acc.stab, cap + 1, _ => ())
        val truncated       = foundMore > found
        checked += 1
        if capped != truncated then
          if truncated then falseNeg += 1 else falsePos += 1
          rows += s"species $idx skeleton $si cap $cap: found=$found reportedCapped=$capped actuallyTruncated=$truncated"
    rows.result().foreach(info(_))
    info(s"checked $checked (species, skeleton, cap) cases: $falseNeg false negatives, $falsePos false positives")
    checked should be > 0
    falseNeg shouldBe 0
