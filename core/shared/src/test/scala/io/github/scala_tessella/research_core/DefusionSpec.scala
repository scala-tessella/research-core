package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.Defusion.*
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[Defusion]]: the flush-split surgery verified by hand on the "house" (a unit square
  * with a fused equilateral roof — both directions: roof off leaves the square, square off leaves the roof),
  * the rejection paths (incompatible arc, zero seam, degenerate remainder), and the real campaign object —
  * P2's fused reflex octagon, developed exactly and de-fused by two triangle splits back to the P1 hexagon.
  */
class DefusionSpec extends AnyFlatSpec with Matchers:

  // the house: square (0,0)–(1,1) with the equilateral triangle fused on its top edge
  private val house   = UnitPolygon(12, Vector(0, 3, 4, 8, 9))
  private val slitOct = UnitPolygon(12, Vector(0, 4, 3, 2, 6, 10, 9, 8))

  "regularUnits" should "give the regular interior angle on the lattice, or refuse" in:
    regularUnits(12, 3) shouldBe 2  // 60°
    regularUnits(12, 4) shouldBe 3  // 90°
    regularUnits(42, 7) shouldBe 15 // 5π/7
    an[IllegalArgumentException] should be thrownBy regularUnits(12, 5)

  "regularGon" should "step by N/q and certify" in:
    regularGon(12, 4, 9).dirs shouldBe Vector(9, 0, 3, 6)
    regularGon(12, 4, 9).isSimpleCertified shouldBe true

  "splitFlush" should "cleave the roof off the house, leaving the square" in:
    val s = splitFlush(house, 2, 2, 3).get
    s.regular.dirs shouldBe Vector(4, 8, 0)
    s.remainders.map(_.poly.dirs) shouldBe Vector(Vector(9, 0, 3, 6))
    s.remainders.head.poly.interiorAngles shouldBe Vector(3, 3, 3, 3)
    s.remainders.head.rel.isZero shouldBe true // a plain flush remainder anchors at the seam

  it should "cleave the square off the house, leaving the roof" in:
    val s = splitFlush(house, 4, 3, 4).get
    s.regular.dirs shouldBe Vector(9, 0, 3, 6)
    s.remainders.map(_.poly.interiorAngles) shouldBe Vector(Vector(2, 2, 2))

  it should "recognise a regular tile as the full-arc degenerate case" in:
    splitFlush(UnitPolygon(12, Vector(0, 3, 6, 9)), 1, 4, 4).get.remainders shouldBe empty

  it should "reject incompatible arcs and zero seams" in:
    splitFlush(house, 0, 2, 3) shouldBe None // arc (0,3) does not step by N/3
    splitFlush(house, 4, 2, 4) shouldBe None // square flush on 2 edges: zero leftover at (1,0)

  it should "pinch-split the fusion-28 star where the square notch lands on the far vertex" in:
    // the (3.3.4.12) fusion star: (60°,150°,240°)×4; the square notched on a 150°/240° edge is
    // exactly star-arm-sized — its far corner meets the opposite 240° vertex and the remainder
    // pinches into two embedded pieces (two tiles at a vertex), not one self-touching polygon
    val star = UnitPolygon(12, Vector(8, 9, 7, 11, 0, 10, 2, 3, 1, 5, 6, 4))
    star.interiorAngles.sorted shouldBe Vector(2, 2, 2, 2, 5, 5, 5, 5, 8, 8, 8, 8)
    val s    = splitFlush(star, 1, 1, 4).get
    s.regular.dirs.length shouldBe 4
    s.remainders.length shouldBe 2
    for p <- s.remainders do p.poly.isSimpleCertified shouldBe true

  it should "reject splits whose remainder is not an embedded polygon" in:
    // a triangle flush on one slit-side edge of the degenerate octagon: seams pass (210° − 60°),
    // but the region has no room — the surgered remainder fails the embedding certificate
    splitFlush(slitOct, 2, 1, 3) shouldBe None

  "vertexTouchSplits" should "find the star's inward triangle on a notch chord" in:
    // the fusion-28 star: the two 150° vertices flanking an arm tip are a unit chord with no
    // edge between them; the INWARD unit triangle on that chord touches the boundary at
    // exactly those two vertices and cuts the star into the tip triangle plus a 12-gon
    val star = UnitPolygon(12, Vector(0, 1, 5, 3, 4, 8, 6, 7, 11, 9, 10, 2))
    val vts  = Defusion.vertexTouchSplits(star, List(3, 4))
    val hit  = vts.filter(vt =>
      vt.q == 3 && vt.contacts.length == 2 &&
        vt.remainders.map(_.poly.dirs.length).sorted == Vector(3, 12)
    )
    hit.nonEmpty shouldBe true
    for p <- hit.head.remainders do p.poly.isSimpleCertified shouldBe true

  it should "find the house's inward triangle on the eave chord, and nothing on a square" in:
    // the house's two eaves (1,1) and (0,1) are a unit chord: the DOWNWARD triangle on it
    // touches only those two vertices and cuts the house into its roof triangle + a pentagon —
    // a genuine vertex-touch split the flush family cannot see (both placements are the same
    // split found from either contact anchor)
    val hvts = Defusion.vertexTouchSplits(house, List(3, 4, 6))
    hvts.length shouldBe 2
    for vt <- hvts do
      vt.q shouldBe 3
      vt.contacts shouldBe Vector(2, 4)
      vt.remainders.map(_.poly.dirs.length).sorted shouldBe Vector(3, 5)
    Defusion.vertexTouchSplits(UnitPolygon(12, Vector(0, 3, 6, 9)), List(3, 4)) shouldBe empty

  "flushSplits" should "find exactly the hand-checked splits of the house" in:
    val found = flushSplits(house, List(3, 4)).map(f => (f.edge, f.arcLen, f.q))
    found should contain allOf ((2, 2, 3), (4, 3, 4))

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

  "the P2 fused octagon" should "de-fuse to the P1 hexagon by two triangle splits" in:
    val (_, z, key) = EngineWitnesses.entries.find(_._1 == "(3.7.42) P2 fused k=6").get
    val ds          = symbolFromKey(key)
    val patch       = ExactDeveloper.develop(ds, rigidPoint(ds, z), 5.0)
    val oct         = patch.faces.map(_.poly).find(_.dirs.length == 8).get
    oct.isSimpleCertified shouldBe true                    // genuinely embedded, reflex and all
    oct.interiorAngles.exists(_ > oct.n / 2) shouldBe true // the fused seams are reflex
    val reachableHexes =
      for
        f <- flushSplits(oct, List(3)) if f.split.remainders.map(_.poly.dirs.length) == Vector(7)
        s <- flushSplits(f.split.remainders.head.poly, List(3))
        if s.split.remainders.map(_.poly.dirs.length) == Vector(6)
      yield s.split.remainders.head.poly.interiorAngles.sorted
    reachableHexes should contain(Vector(7, 7, 15, 15, 20, 20)) // exactly the P1 hexagon
