package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols
import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** ADR-0009 paper certification, track A2 — fast teeth for the [[K2Certify]] encoding (the heavy per-C
  * obligations live in `K2CompletenessProbe`, guarded):
  *
  *   - AGREEMENT at C ≤ 8: the SAT enumeration equals, op-for-op, the BFS relabelings of the tier-1 universe
  *     generator — two independent enumerators, one universe, including genuinely 2-orbit classes;
  *   - the ≤ 2-orbit layer has teeth: a valid, connected 3-vertex-orbit D-set (σ₀ = (13)(24), σ₁ = (34), σ₂ =
  *     id on 4 chambers) is excluded from the model set;
  *   - the tier-1 layer has teeth: every ≤ 2-orbit D-set FAILING [[DelaneySymbols.tier1Feasible]] is
  *     excluded, and the raw-universe slice at C = 5..6 contains such D-sets (so the exclusion is not
  *     vacuous).
  */
class K2CertifySpec extends AnyFlatSpec with Matchers:

  private def ops(ds: DSet): List[Int] = (1 to ds.size).flatMap(d => (0 to 2).map(i => ds.get(i, d))).toList

  private def universe(maxSize: Int, tier1: Boolean): Vector[DSet] =
    val out = mutable.ArrayBuffer.empty[DSet]
    DelaneySymbols.relaxedOrbitBoundedDSets(
      maxN = 2,
      maxSize = maxSize,
      parallelism = 1,
      // NOT `out.synchronized(out += _)`: synchronized is by-name, that form locks only the lambda's
      // CONSTRUCTION and hands the generator an unsynchronized callback
      sink = ds => out.synchronized(out += ds),
      tier1 = tier1
    )
    out.toVector

  "K2Certify" should "agree with the tier-1 universe generator at C <= 8, op for op" in:
    val byC = universe(8, tier1 = true).groupBy(_.size)
    for c <- 1 to 8 do
      val expected = byC.getOrElse(c, Vector.empty).flatMap(DelaneySymbols.bfsRelabelings).map(ops).toSet
      val models   = K2Certify.enumerate(c)
      withClue(s"C=$c: "):
        models.map(ops).toSet shouldBe expected
        models.size shouldBe expected.size

  it should "exclude a valid 3-vertex-orbit D-set (orbit layer has teeth)" in:
    // chambers 1..4: σ₀ = (13)(24), σ₁ = (34), σ₂ = id — connected, (σ₀σ₂)² = id, vertex orbits {1},{2},{3,4}
    val a                                 = Array.ofDim[Int](5, 3)
    def set(i: Int, d: Int, e: Int): Unit = { a(d)(i) = e; a(e)(i) = d }
    set(0, 1, 3); set(0, 2, 4)
    set(1, 3, 4); set(1, 1, 1); set(1, 2, 2)
    set(2, 1, 1); set(2, 2, 2); set(2, 3, 3); set(2, 4, 4)
    val ds                                = new DSet(a)
    val models                            = K2Certify.enumerate(4).map(ops).toSet
    DelaneySymbols.bfsRelabelings(ds).map(ops).foreach(l => models should not contain l)

  it should "exclude tier-1-infeasible <= 2-orbit D-sets, non-vacuously" in:
    val raw        = universe(6, tier1 = false)
    val infeasible = raw.filterNot(DelaneySymbols.tier1Feasible)
    infeasible should not be empty // the exclusion is exercised, not vacuous
    val modelsByC = (1 to 6).map(c => c -> K2Certify.enumerate(c).map(ops).toSet).toMap
    for ds <- infeasible; l <- DelaneySymbols.bfsRelabelings(ds) do
      modelsByC(ds.size) should not contain ops(l)
