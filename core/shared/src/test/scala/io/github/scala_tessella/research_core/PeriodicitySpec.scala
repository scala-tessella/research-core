package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.research_core.Periodicity.*
import io.github.scala_tessella.research_core.TilePatch.{seed, PlacedTile, State}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[Periodicity]], anchored on the two textbook quotients: the square tiling (1 tile
  * class, 8 chambers, 1 vertex class per cell) and the hexagon tiling (1 tile class, 12 chambers, 2 vertex
  * classes) — each self-certified by the exact area identity.
  */
class PeriodicitySpec extends AnyFlatSpec with Matchers:

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

  private def regularState(key: String, z: List[Int], radius: Double): State =
    val ds = symbolFromKey(key)
    seed(ExactDeveloper.develop(ds, MetricLayer.regularPoint(ds), radius), z)

  "norm2Less and crossSign" should "compare exactly" in:
    norm2Less(Cyc.root(12, 0), Cyc.root(12, 0) + Cyc.root(12, 0)) shouldBe true
    norm2Less(Cyc.root(12, 0), Cyc.root(12, 3)) shouldBe false // equal norms
    crossSign(Cyc.root(12, 0), Cyc.root(12, 3)) shouldBe 1
    crossSign(Cyc.root(12, 0), Cyc.root(12, 6)) shouldBe 0 // anti-parallel

  "canonicalPlacement" should "rotate the word to its least position, keeping the placement" in:
    val sq = UnitPolygon(12, Vector(6, 9, 0, 3))
    val pt = canonicalPlacement(PlacedTile(sq, Cyc.zero(12)))
    pt.poly.dirs shouldBe Vector(0, 3, 6, 9)
    (pt.anchor === (Cyc.root(12, 6) + Cyc.root(12, 9))) shouldBe true // vertex 2 of the original

  "reduceBasis" should "shorten a skewed basis with exact acceptance" in:
    val (a, b) = reduceBasis(Cyc.root(4, 0), Cyc.root(4, 0).scaled(3) + Cyc.root(4, 1))
    (norm2(a) === norm2(Cyc.root(4, 0))) shouldBe true
    (norm2(b) === norm2(Cyc.root(4, 1))) shouldBe true

  "latticeEquiv" should "solve and verify the integer combination" in:
    val eq = latticeEquiv(Cyc.root(12, 0), Cyc.root(12, 3)) // the Gaussian lattice inside ζ₁₂
    eq(Cyc.zero(12), Cyc.root(12, 0).scaled(3) - Cyc.root(12, 3).scaled(2)) shouldBe true
    eq(Cyc.root(12, 3), Cyc.root(12, 3).scaled(4)) shouldBe true
    eq(Cyc.zero(12), Cyc.root(12, 1)) shouldBe false // (cos 30°, sin 30°) is not Gaussian

  "the square tiling" should "quotient to its textbook cell" in:
    val state          = regularState("1,1,1,4|4", List(4, 4, 4, 4), 4.0)
    val Some((t1, t2)) = latticeBasis(state): @unchecked
    (norm2(t1) === norm2(Cyc.root(4, 0))) shouldBe true // both basis vectors unit length
    (norm2(t2) === norm2(Cyc.root(4, 0))) shouldBe true
    val cen              = cellCensus(state, t1, t2)
    cen.tileClasses.length shouldBe 1
    cen.chambers shouldBe 8
    cen.vertexClasses shouldBe 1
    cen.areaCertified shouldBe true
    cen.cellArea.approx._2 shouldBe 1.0 +- 1e-9
    val Some((_, _, cc)) = certifiedCell(state): @unchecked
    (cc.chambers, cc.vertexClasses, cc.areaCertified) shouldBe (8, 1, true)

  "the hexagon tiling" should "quotient to its textbook cell" in:
    val state             = regularState("1,1,1,6|3", List(6, 6, 6), 5.0)
    val Some((_, _, cen)) = certifiedCell(state): @unchecked
    cen.tileClasses.length shouldBe 1
    cen.chambers shouldBe 12
    cen.vertexClasses shouldBe 2
    cen.areaCertified shouldBe true
    cen.cellArea.approx._2 shouldBe 3 * math.sqrt(3) / 2 +- 1e-9

  "densityCompare" should "order and tie exactly" in:
    val e = Cyc.root(12, 3) // Im = 1
    densityCompare((2, e), (1, e)) shouldBe 1
    densityCompare((1, e), (2, e)) shouldBe -1
    densityCompare((2, e.scaled(2)), (1, e)) shouldBe 0 // 2/2 = 1/1, an exact tie
