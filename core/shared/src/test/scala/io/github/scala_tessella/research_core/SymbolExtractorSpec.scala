package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{canonicalKey, DSet, DSymbol}
import io.github.scala_tessella.research_core.SymbolExtractor.*
import io.github.scala_tessella.research_core.TilePatch.{seed, State}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[SymbolExtractor]], closing the loop geometry → symbol: the two regular tilings
  * extract and fold to their one-chamber symbols, and the fusion-28 #2 patch — developed from a 28-chamber
  * entry, quotiented, extracted and minimised — comes back CANONICALLY ISOMORPHIC to the minimal image of the
  * very symbol it was developed from.
  */
class SymbolExtractorSpec extends AnyFlatSpec with Matchers:

  private def symbolFromKey(key: String): DSymbol =
    val rows          = key.split(";").map(_.trim).filter(_.nonEmpty)
    val n             = rows.length
    val op            = Array.ofDim[Int](n + 1, 3)
    val m01           = Array.ofDim[Int](n + 1)
    val m12           = Array.ofDim[Int](n + 1)
    for (r, i) <- rows.zipWithIndex do
      val Array(ops, ms) = r.split('|')
      val parts          = ops.split(',').map(_.toInt)
      op(i + 1)(0) = parts(0); op(i + 1)(1) = parts(1); op(i + 1)(2) = parts(2)
      m01(i + 1) = parts(3); m12(i + 1) = ms.toInt
    val dset          = new DSet(op)
    val (orbs, index) = DelaneySymbols.collectOrbits(dset)
    val vs            = Array.tabulate(orbs.length): k =>
      val o = orbs(k)
      val d = o.elements.head
      (if o.i == 0 then m01(d) else m12(d)) / o.r
    new DSymbol(dset, orbs, index, vs)

  private def rigidPoint(ds: DSymbol, z: List[Int]): Array[Frac] =
    val sys      = MetricLayer.angleSystem(ds)
    val unforced = UClass
      .candidates(ds, z)
      .filter: r =>
        val irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
        UClass.noneForcedRegular(ds, r, irregular)
    MetricLayer.particularSolution(MetricLayer.designatedRows(ds, unforced.maxBy(_.size)), sys.vars).get

  private def extractProfile(state: State): (Int, Int, String) =
    val Some((t1, t2, cen)) = Periodicity.certifiedCell(state): @unchecked
    cen.areaCertified shouldBe true
    val Some(torus)         = torusSymbol(state, t1, t2): @unchecked
    torus.size shouldBe cen.chambers // the extracted symbol matches the certified census
    val min = minimalImage(torus)
    (min.size, min.orbs.count(_.i == 1), min.canonicalKey)

  "the square tiling" should "extract and fold to its one-chamber symbol" in:
    val ds          = symbolFromKey("1,1,1,4|4")
    val state       = seed(ExactDeveloper.develop(ds, MetricLayer.regularPoint(ds), 4.0), List(4, 4, 4, 4))
    val (c, k, key) = extractProfile(state)
    (c, k) shouldBe (1, 1)
    key shouldBe minimalImage(ds).canonicalKey

  "the hexagon tiling" should "extract and fold to its one-chamber symbol" in:
    val ds          = symbolFromKey("1,1,1,6|3")
    val state       = seed(ExactDeveloper.develop(ds, MetricLayer.regularPoint(ds), 5.0), List(6, 6, 6))
    val (c, k, key) = extractProfile(state)
    (c, k) shouldBe (1, 1)
    key shouldBe minimalImage(ds).canonicalKey

  "entryKey" should "serialise a symbol so it parses back isomorphically" in:
    val ds  = symbolFromKey("1,1,1,4|4")
    symbolFromKey(entryKey(ds)).canonicalKey shouldBe ds.canonicalKey
    val f28 = symbolFromKey(EngineWitnesses.entries.find(_._1 == "(3.3.4.12) fusion28 #2").get._3)
    symbolFromKey(entryKey(f28)).canonicalKey shouldBe f28.canonicalKey

  "the fusion-28 #2 patch" should "round-trip: extracted minimal symbol ≅ the entry's minimal image" in:
    val (_, z, key) = EngineWitnesses.entries.find(_._1 == "(3.3.4.12) fusion28 #2").get
    val ds          = symbolFromKey(key)
    val state       = seed(ExactDeveloper.develop(ds, rigidPoint(ds, z), 14.0), z)
    val (c, k2, mk) = extractProfile(state)
    val entryMin    = minimalImage(ds)
    c shouldBe entryMin.size
    k2 shouldBe entryMin.orbs.count(_.i == 1)
    mk shouldBe entryMin.canonicalKey // geometry → symbol closes the loop exactly
