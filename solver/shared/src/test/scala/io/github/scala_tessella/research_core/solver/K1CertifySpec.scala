package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols
import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** Fast teeth for the [[K1Certify]] encoding, mirroring `K2CertifySpec`: the SAT enumeration must equal,
  * op for op, the BFS relabelings of the single-vertex-orbit slice of the relaxed universe generator —
  * two independent enumerators, one universe. K1's encoding has no tier-1 layer, so the oracle runs
  * `tier1 = false`; and the ≤ 1-orbit restriction has teeth: the strictly-2-orbit slice of the `maxN = 2`
  * universe is non-empty and entirely excluded.
  */
class K1CertifySpec extends AnyFlatSpec with Matchers:

  private def ops(ds: DSet): List[Int] = (1 to ds.size).flatMap(d => (0 to 2).map(i => ds.get(i, d))).toList

  private def universe(maxN: Int, maxSize: Int): Vector[DSet] =
    val out = mutable.ArrayBuffer.empty[DSet]
    DelaneySymbols.relaxedOrbitBoundedDSets(
      maxN = maxN,
      maxSize = maxSize,
      parallelism = 1,
      sink = ds => out.synchronized(out += ds),
      tier1 = false
    )
    out.toVector

  private def closure(dsets: Vector[DSet]): Set[List[Int]] =
    dsets.flatMap(DelaneySymbols.bfsRelabelings).map(ops).toSet

  "K1Certify" should "agree with the single-orbit universe generator at C <= 8, op for op" in:
    val byC = universe(maxN = 1, maxSize = 8).groupBy(_.size)
    for c <- 1 to 8 do
      val expected = closure(byC.getOrElse(c, Vector.empty))
      withClue(s"C = $c: ") { K1Certify.enumerate(c).map(ops).toSet shouldBe expected }

  it should "exclude the strictly-2-orbit slice of the universe, non-vacuously" in:
    val one = closure(universe(maxN = 1, maxSize = 8))
    val two = closure(universe(maxN = 2, maxSize = 8))
    val strictly2 = two -- one
    strictly2 should not be empty // the exclusion is not vacuous
    val models = (1 to 8).flatMap(c => K1Certify.enumerate(c).map(ops)).toSet
    models.intersect(strictly2) shouldBe empty
