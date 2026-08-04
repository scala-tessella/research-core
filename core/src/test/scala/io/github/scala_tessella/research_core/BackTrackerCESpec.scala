package io.github.scala_tessella.research_core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Parity + timing teeth for the Cats Effect engine ([[DelaneySymbols.BackTracker.parallelForeachCE]])
  * against the ForkJoin engine on the identical D-set generation tree. The counts must agree exactly (both
  * engines walk the same tree, only the scheduling differs); the printed timings are the benchmark seam —
  * callers are NOT switched to CE until these look at least comparable.
  */
class BackTrackerCESpec extends AnyFlatSpec with Matchers:

  private def timed[A](body: => A): (A, Long) =
    val t0 = System.nanoTime()
    val a  = body
    (a, (System.nanoTime() - t0) / 1000000)

  "parallelForeachCE" should "count exactly the same D-sets as the ForkJoin engine (maxSize 18)" in:
    val par          = 1.max(Runtime.getRuntime.availableProcessors - 1)
    // warm both engines (JIT + CE runtime spin-up) before the timed runs
    DelaneySymbols.countDSetsParallel(14, par)
    DelaneySymbols.countDSetsParallelCE(14, par)
    val (fjp, fjpMs) = timed(DelaneySymbols.countDSetsParallel(18, par))
    val (ce, ceMs)   = timed(DelaneySymbols.countDSetsParallelCE(18, par))
    info(s"ForkJoin engine: $fjpMs ms, CE engine: $ceMs ms (counts $fjp)")
    ce shouldBe fjp

  it should "agree with the sequential walk at parallelism 1" in:
    DelaneySymbols.countDSetsParallelCE(12, parallelism = 1) shouldBe
      DelaneySymbols.countDSetsParallel(12, parallelism = 1)
