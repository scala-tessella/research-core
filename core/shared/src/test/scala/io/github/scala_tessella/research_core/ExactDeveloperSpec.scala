package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.ExactDeveloper.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[ExactDeveloper]]: the σ-step verified against a hand-walked square, then full
  * developments of the two one-chamber regular symbols ({4,4} and {6³}) at their regular points — every
  * extracted face exactly closed, simple and regular, every vertex angle sum ≤ N with the origin interior —
  * and of the (3.7.42) P1 witness at its rigid point via the designation solver now public on
  * [[MetricLayer]].
  */
class ExactDeveloperSpec extends AnyFlatSpec with Matchers:

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

  private val square44 = symbolFromKey("1,1,1,4|4") // the regular square tiling, one chamber
  private val hex63    = symbolFromKey("1,1,1,6|3") // the regular hexagon tiling, one chamber

  "latticeOf" should "give the least even lattice fitting all denominators" in:
    latticeOf(List(Frac(1, 2))) shouldBe 4
    latticeOf(List(Frac(2, 3))) shouldBe 6
    latticeOf(List(Frac(2, 3), Frac(1, 2))) shouldBe 12
    latticeOf(List(Frac(6, 7), Frac(2, 3), Frac(1, 3))) shouldBe 42 // the P1 witness angles

  "angleUnits" should "convert π-unit angles exactly, reflex included" in:
    angleUnits(Frac(1, 2), 12) shouldBe 3
    angleUnits(Frac(2, 3), 6) shouldBe 2
    angleUnits(Frac(7, 6), 12) shouldBe 7 // 210°, the reflex corner class
    an[IllegalArgumentException] should be thrownBy angleUnits(Frac(1, 5), 12)

  private val stepSq = step(square44, _ => 1, 4)
  private val flag0  = Flag(1, Cyc.zero(4), 0, 1)

  "step" should "walk the edge on σ₀, turn by the corner on σ₁, cross on σ₂" in:
    val f1 = stepSq(flag0, 0)
    (f1.pos === Cyc.root(4, 0)) shouldBe true
    (f1.dir, f1.chir) shouldBe (2, -1)
    val f2 = stepSq(f1, 1)
    (f2.pos === f1.pos) shouldBe true
    (f2.dir, f2.chir) shouldBe (1, 1) // 90° = 1 unit, sense −1: the next CCW square edge
    val f3 = stepSq(flag0, 2)
    (f3.pos === flag0.pos, f3.dir, f3.chir) shouldBe (true, 0, -1)

  it should "close the square in four σ₁∘σ₀ strides" in:
    val walk = Iterator.iterate(flag0)(f => stepSq(stepSq(f, 0), 1)).take(5).toVector
    walk.map(_.dir % 4) shouldBe Vector(0, 1, 2, 3, 0)
    walk.last.key(4) shouldBe flag0.key(4)

  private val sqPatch = develop(square44, MetricLayer.regularPoint(square44), 2.5)

  "develop on {4,4}" should "produce exactly the unit squares with all corners in range" in:
    sqPatch.n shouldBe 4
    sqPatch.faces.length shouldBe 12
    for f <- sqPatch.faces do
      f.poly.isClosed shouldBe true
      f.poly.isSimpleCertified shouldBe true
      f.poly.interiorAngles shouldBe Vector(1, 1, 1, 1)
    sqPatch.faces.map(f => (f.anchor.reducedKey, f.poly.dirs)).distinct.length shouldBe 12

  it should "have no overlap and an interior origin" in:
    val sums = sqPatch.vertexAngleSums
    sums.values.max should be <= 4
    sums(Cyc.zero(4).reducedKey) shouldBe 4 // the origin is surrounded by four squares
    sums.values.count(_ == 4) shouldBe 5 // origin and its four lattice neighbours

  it should "grow monotonically with the radius" in:
    develop(square44, MetricLayer.regularPoint(square44), 4.0).faces.length should be > 12

  "develop on {6³}" should "produce exact regular hexagons with interior vertices" in:
    val patch = develop(hex63, MetricLayer.regularPoint(hex63), 3.5)
    patch.n shouldBe 6
    // the ring-1 hexagon opposite the seed has a vertex at ≈3.73 — clipped at radius 3.5
    patch.faces.length shouldBe 6
    for f <- patch.faces do
      f.poly.isClosed shouldBe true
      f.poly.isSimpleCertified shouldBe true
      f.poly.interiorAngles shouldBe Vector(2, 2, 2, 2, 2, 2)
    val sums  = patch.vertexAngleSums
    sums.values.max should be <= 6
    sums.values.count(_ == 6) shouldBe 4 // central corners not touching the clipped hexagon

  /** The rigid angle point of a witness symbol: maximal unforced designation, its linear rows, dimension-0
    * check, exact particular solution — the probes' recipe on the now-public [[MetricLayer]] methods.
    */
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

  "develop on the (3.7.42) P1 witness" should "reproduce an exact patch of the banked pattern" in:
    val (_, z, key) = EngineWitnesses.entries.find(_._1 == "(3.7.42) P1 k=10").get
    val ds          = symbolFromKey(key)
    val patch       = develop(ds, rigidPoint(ds, z), 5.0)
    patch.n shouldBe 42                                                  // every angle lands on the π/21 lattice — the 12-gons are irregular
    for f <- patch.faces do
      f.poly.isClosed shouldBe true
      f.poly.isSimpleCertified shouldBe true
    patch.faces.map(_.poly.dirs.length).toSet shouldBe Set(3, 6, 7, 12)  // 42-gons clipped here
    for f <- patch.faces if f.poly.dirs.length == 6 do
      f.poly.interiorAngles.sorted shouldBe Vector(7, 7, 15, 15, 20, 20) // (60°,128.6°,171.4°)²
    patch.vertexAngleSums.values.max should be <= 42 // exact no-overlap: the holonomy audit
