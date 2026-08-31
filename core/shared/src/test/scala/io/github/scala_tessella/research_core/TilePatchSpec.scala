package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.research_core.TilePatch.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[TilePatch]], ending in the full constructive-path loop on real campaign objects:
  * the P2 patch, developed exactly, greedily exhausts in exactly the two de-fusion triangle moves to a
  * saturated endpoint whose irregular tiles are the P1 hexagon and 12-gon — the known P2 → P1 de-fusion, now
  * reproduced by the engine end to end.
  */
class TilePatchSpec extends AnyFlatSpec with Matchers:

  private val square = UnitPolygon(12, Vector(0, 3, 6, 9))
  private val house  = UnitPolygon(12, Vector(0, 3, 4, 8, 9))

  "shapeKey" should "identify congruent shapes across shift, rotation and reflection" in:
    shapeKey(UnitPolygon(12, Vector(3, 6, 9, 0))) shouldBe shapeKey(square)    // index shift
    shapeKey(UnitPolygon(12, Vector(1, 4, 7, 10))) shouldBe shapeKey(square)   // rotated 30°
    shapeKey(UnitPolygon(12, Vector(9, 10, 2, 3, 6))) shouldBe shapeKey(house) // mirrored house
    shapeKey(square) should not be shapeKey(UnitPolygon(12, Vector(0, 4, 8)))

  "regularSizeOf" should "recognise regular polygons only" in:
    regularSizeOf(square) shouldBe Some(4)
    regularSizeOf(UnitPolygon(12, Vector(2, 6, 10))) shouldBe Some(3)
    regularSizeOf(house) shouldBe None
    regularSizeOf(UnitPolygon(42, Vector(0, 1, 7, 21, 22, 28))) shouldBe None // the P1 hexagon

  "vertexLegal" should "enforce the ratified class semantics on vertex words" in:
    val z = List(3, 7, 42)
    vertexLegal(Vector(Some(7), Some(42), Some(3)), z) shouldBe true // species, rotated
    vertexLegal(Vector(Some(42), Some(7), Some(3)), z) shouldBe true // species, reflected
    vertexLegal(Vector(Some(3), Some(3), Some(7)), z) shouldBe false // all-regular non-species
    vertexLegal(Vector(None, Some(3), Some(7)), z) shouldBe true     // spliced arc (3,7)
    vertexLegal(Vector(None, Some(42), Some(42)), z) shouldBe false  // (42,42) is no arc of z
    vertexLegal(Vector(Some(3), None), z) shouldBe true              // valence-2 interface
    vertexLegal(Vector(None, None), z) shouldBe true                 // empty splice is vacuous
    vertexLegal(Vector(Some(42), None, Some(7), None), z) shouldBe true // splice joins to (42,7)

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
    val reg      = unforced.maxBy(_.size)
    val rows     = MetricLayer.designatedRows(ds, reg)
    MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).size shouldBe 0
    MetricLayer.particularSolution(rows, sys.vars).get

  private def entryState(name: String, radius: Double): State =
    val (_, z, key) = EngineWitnesses.entries.find(_._1 == name).get
    val ds          = symbolFromKey(key)
    seed(ExactDeveloper.develop(ds, rigidPoint(ds, z), radius), z)

  "a regular-tiling state" should "be valid, move-free and its own endpoint" in:
    val sq44  = symbolFromKey("1,1,1,4|4")
    val state = seed(ExactDeveloper.develop(sq44, MetricLayer.regularPoint(sq44), 3.0), List(4, 4, 4, 4))
    valid(state) shouldBe true
    state.interiorWords.nonEmpty shouldBe true
    moves(state) shouldBe empty
    exhaust(state)._2 shouldBe empty

  // ~30 s: the radius must reach 20 so complete 42-gons make whole octagons checkable — below
  // that the witness rule (correctly) starves the second de-fusion of evidence
  "the P2 state" should "greedily exhaust to the P1 endpoint in two triangle de-fusions" in:
    val p2          = entryState("(3.7.42) P2 fused k=6", 20.0)
    valid(p2) shouldBe true
    p2.interiorWords.nonEmpty shouldBe true
    val (end, path) = exhaust(p2)
    path.map(m => (m.kind, m.q, m.a)) shouldBe List(("flush", 3, 2), ("flush", 3, 2))
    valid(end) shouldBe true
    admissible(end) shouldBe empty // saturated
    val irregular = end.tiles.map(_.poly).filter(regularSizeOf(_).isEmpty)
    irregular.map(_.dirs.length).toSet shouldBe Set(6, 12) // no fused shapes left
    irregular.filter(_.dirs.length == 6).map(_.interiorAngles.sorted).distinct shouldBe
      Vector(Vector(7, 7, 15, 15, 20, 20)) // every hexagon is THE P1 hexagon
