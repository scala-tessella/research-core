package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[ExactPlane]] — the campaign's known shapes as fixtures. */
class ExactPlaneSpec extends AnyFlatSpec with Matchers:

  private val square   = UnitPolygon(12, Vector(0, 3, 6, 9))
  private val triangle = UnitPolygon(12, Vector(0, 4, 8))
  // the (3².4.12) DEGENERATE "octagon" — two unit triangles joined by a doubly-traversed slit,
  // interior angles (60°, 210°, 210°, 60°)², all healthy, boundary self-touching (2026-08-17)
  private val slitOct  = UnitPolygon(12, Vector(0, 4, 3, 2, 6, 10, 9, 8))
  // the (3.7.42) P1 witness hexagon: interior (171.4°, 128.6°, 60°)² on the π/21 lattice
  private val p1Hex    = UnitPolygon(42, Vector(0, 1, 7, 21, 22, 28))

  "isClosed" should "hold for the regular shapes and the witness hexagon" in:
    square.isClosed shouldBe true
    triangle.isClosed shouldBe true
    p1Hex.isClosed shouldBe true
    UnitPolygon(12, Vector(0, 3, 6)).isClosed shouldBe false // three sides of a square

  "interiorAngles" should "give the regular values in 2π/N units" in:
    square.interiorAngles shouldBe Vector(3, 3, 3, 3) // 90° = 3·30°
    triangle.interiorAngles shouldBe Vector(2, 2, 2)  // 60°
    p1Hex.interiorAngles shouldBe Vector(7, 20, 15, 7, 20, 15) // (60°, 171.4°, 128.6°)²

  it should "sum to (p−2)·π" in:
    for poly <- List(square, triangle, slitOct, p1Hex) do
      poly.interiorAngles.sum shouldBe (poly.dirs.length - 2) * poly.n / 2

  "cornersSane" should "accept reflex corners but reject straight ones" in:
    slitOct.cornersSane shouldBe true // 210° corners are reflex, not straight
    UnitPolygon(12, Vector(0, 0, 4, 8)).cornersSane shouldBe false // a straight corner

  "selfTouches" should "expose the slit octagon and clear the genuine shapes" in:
    slitOct.selfTouches shouldBe true
    square.selfTouches shouldBe false
    triangle.selfTouches shouldBe false
    p1Hex.selfTouches shouldBe false

  // the unit pentagram: equilateral, closed, sane 36° corners, NO coincident vertices — its
  // edges cross properly, the exact failure mode `selfTouches` cannot see
  private val pentagram = UnitPolygon(10, Vector(0, 4, 8, 2, 6))

  "orientation" should "read the exact turn of a point triple" in:
    import ExactPlane.orientation
    val (o, e0, up) = (Cyc.zero(12), Cyc.root(12, 0), Cyc.root(12, 3))
    orientation(o, e0, e0 + up) shouldBe 1
    orientation(o, up, e0 + up) shouldBe -1
    orientation(o, e0, e0 + e0) shouldBe 0

  "segmentsIntersect" should "decide crossings, touches and disjointness exactly" in:
    import ExactPlane.segmentsIntersect
    val (o, e0)  = (Cyc.zero(12), Cyc.root(12, 0))
    val (up, dn) = (Cyc.root(12, 3), Cyc.root(12, 9))
    segmentsIntersect(o, e0 + e0, e0 + dn, e0 + up) shouldBe true // proper crossing at (1, 0)
    segmentsIntersect(o, e0, e0, e0 + up) shouldBe true           // endpoint touch (closed)
    segmentsIntersect(o, e0, up, e0 + up) shouldBe false          // parallel, disjoint
    segmentsIntersect(o, e0 + e0, e0, e0 + e0 + e0) shouldBe true // collinear overlap

  "selfIntersects" should "catch the pentagram's crossings that selfTouches misses" in:
    pentagram.selfTouches shouldBe false // five distinct star points — the vertex test is blind
    pentagram.selfIntersects shouldBe true
    square.selfIntersects shouldBe false
    p1Hex.selfIntersects shouldBe false
    slitOct.selfIntersects shouldBe true // vertex coincidence is a shared point too

  "isSimpleCertified" should "combine closure, winding, corner sanity and embedding" in:
    square.isSimpleCertified shouldBe true
    p1Hex.isSimpleCertified shouldBe true
    slitOct.isSimpleCertified shouldBe false
    pentagram.isSimpleCertified shouldBe false // winding 2 AND crossings — both now checked

  "doubleArea and areaCompare" should "measure exactly, proving ties across different shapes" in:
    square.areaApprox shouldBe 1.0 +- 1e-9
    triangle.areaApprox shouldBe math.sqrt(3) / 4 +- 1e-9
    ExactPlane.areaCompare(square, triangle) shouldBe 1
    ExactPlane.areaCompare(triangle, square) shouldBe -1
    ExactPlane.areaCompare(square, UnitPolygon(12, Vector(1, 4, 7, 10))) shouldBe 0 // congruent tie
    // a 60°-rhombus and the slit octagon (two unit triangles) both cover √3/2 — an EXACT tie
    // between non-congruent shapes, decided algebraically
    val rhombus = UnitPolygon(12, Vector(0, 2, 6, 8))
    rhombus.areaApprox shouldBe math.sqrt(3) / 2 +- 1e-9
    ExactPlane.areaCompare(rhombus, slitOct) shouldBe 0

  "verticesFrom" should "return distinct exact points closed by the last edge" in:
    val vs = square.verticesFrom(Cyc.zero(12))
    vs.length shouldBe 4
    for i <- vs.indices; j <- i + 1 until vs.length do
      withClue(s"vertices $i,$j distinct: ")(vs(i) === vs(j) shouldBe false)
    (vs.last + Cyc.root(12, 9)) === vs.head shouldBe true // the last edge closes the cycle
