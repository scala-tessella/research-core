package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.render.*
import io.github.scala_tessella.research_core.render.FigureCanvas.Shape
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The format layer, tested as a format: does the container it emits parse, does the drawing it describes
  * survive both backends, and — the reason this package exists — does the stroke policy actually satisfy the
  * rule it was written for?
  *
  * THE LINE-WEIGHT CHECKS ARE THE POINT. A journal artwork rule that lives in a comment gets violated and
  * nobody notices, which is exactly what happened on 2026-08-11. Here the band is an assertion over
  * [[FigurePolicy]], evaluated at both documented placements.
  */
class RenderSpec extends AnyFlatSpec with Matchers:

  /** The emitters' own number format. Written out rather than reached for with `f"…%.2f"`, which formats in
    * the DEFAULT locale — on this machine `it_IT`, so it would expect `1,10` where the emitter correctly
    * writes `1.10`. The trap this whole layer guards against, met in the test that guards it.
    */
  private def num(v: Double): String = String.format(java.util.Locale.ROOT, "%.2f", v)

  /** A unit square and a unit triangle beside it — two faces, seven corners, enough to exercise every emitter
    * without pulling the symbol engine into a format test.
    */
  private val faces: Vector[(Int, Vector[Pt])] = Vector(
    4 -> Vector((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)),
    3 -> Vector((1.0, 0.0), (2.0, 0.0), (1.5, 0.866))
  )

  behavior of "the stroke policy"

  it should "keep a standalone panel in the printed band at its documented placement" in:
    // a panel ~447 pt wide intrinsically, placed at 75 mm (212.6 pt) in a 2x2 composite
    val printed = FigurePolicy.printedPt(FigurePolicy.StrokeUnits, FigurePolicy.PtPerUnit, 212.6 / 447)
    printed shouldBe 0.52 +- 0.01
    FigurePolicy.inBand(printed) shouldBe true

  it should "keep a honeycomb page in the band at 26 pt per unit edge" in:
    // the measurement of 2026-08-13: ~212 pt intrinsic, placed at 0.30 of a 425 pt textwidth
    val printed = FigurePolicy.printedPt(FigurePolicy.StrokeUnits, 26.0, 127.6 / 211.8)
    printed shouldBe 0.78 +- 0.02
    FigurePolicy.inBand(printed) shouldBe true

  it should "reject the old widths that prompted the policy" in:
    FigurePolicy.inBand(FigurePolicy.printedPt(0.03, FigurePolicy.PtPerUnit, 212.6 / 447)) shouldBe false
    FigurePolicy.inBand(FigurePolicy.printedPt(0.012, 26.0, 127.6 / 211.8)) shouldBe false

  it should "keep artwork emitted at final size in band even if halved into one column" in:
    FigurePolicy.inBand(FigurePolicy.StrokePt) shouldBe true
    FigurePolicy.inBand(FigurePolicy.StrokePt / 2) shouldBe true

  behavior of "the PDF container"

  it should "point every xref entry at the object it claims" in:
    val objs = Vector("<< /A 1 >>", "<< /B 2 >>", "<< /C 3 >>")
    val doc  = PdfDocument(objs)
    doc should startWith("%PDF-1.4\n")
    doc should endWith("%%EOF\n")
    doc should include(s"/Size ${objs.size + 1}")
    // the offsets in the table are byte positions of "<n> 0 obj"
    val rows = doc.linesIterator.filter(_.endsWith("00000 n ")).map(_.take(10).toInt).toVector
    rows should have size objs.size
    for (off, i) <- rows.zipWithIndex do doc.substring(off) should startWith(s"${i + 1} 0 obj\n")

  it should "point startxref at the xref keyword" in:
    val doc       = PdfDocument(Vector("<< /A 1 >>"))
    val startxref = doc.split("startxref\n")(1).takeWhile(_.isDigit).toInt
    doc.substring(startxref) should startWith("xref\n")

  it should "carry a resource dictionary even for pure path content" in:
    // without it pdfTeX warns "/Resources missing" on every \includegraphics of these figures
    PdfDocument.page(10.0, 20.0, "<< >>") should include("/Resources << >>")
    PdfDocument.page(10.0, 20.0, "<< >>") should include("/MediaBox [0 0 10.00 20.00]")

  behavior of "the PDF panel emitter"

  it should "tell a regular face from an irregular one by its boundary alone" in:
    FaceShape.isRegular(faces(0)._2) shouldBe true
    FaceShape.isRegular(faces(1)._2) shouldBe true
    val rhombus = Vector[Pt]((0.0, 0.0), (1.0, 0.0), (1.5, 0.866), (0.5, 0.866))
    FaceShape.isRegular(rhombus) shouldBe false
    val hexagon =
      Vector[Pt]((1.0, 0.0), (0.5, 0.866), (-0.5, 0.866), (-1.0, 0.0), (-0.5, -0.866), (0.5, -0.866))
    FaceShape.isRegular(hexagon) shouldBe true
    FaceShape.isRegular(hexagon.take(2)) shouldBe false

  it should "tint only the irregular faces, and not at all by default" in:
    val rhombus = 4 -> Vector[Pt]((0.0, 0.0), (1.0, 0.0), (1.5, 0.866), (0.5, 0.866))
    val mixed   = faces :+ rhombus
    PdfFigure.toPdf(mixed) shouldBe PdfFigure.toPdf(mixed, 0.0)
    val plain   = PdfFigure.toPdf(mixed).linesIterator.filter(_.endsWith(" rg")).toVector
    val tinted  =
      PdfFigure.toPdf(mixed, FigurePolicy.IrregularTint).linesIterator.filter(_.endsWith(" rg")).toVector
    plain.take(2) shouldBe tinted.take(2) // the square and the triangle keep their colours
    plain(2) should not be tinted(2)      // the rhombus is lighter
    val (r, g, b) = Palette.rgbTinted(4, FigurePolicy.IrregularTint)
    tinted(2) shouldBe s"${num(r)} ${num(g)} ${num(b)} rg"
    r should be > Palette.rgbOf(4)._1
    Palette.fillTinted(4, 0.0) shouldBe Palette.fillOf(4)
    Palette.fillTinted(4, 1.0) shouldBe "#ffffff"
    SvgFigure.toSvg(mixed, "t", FigurePolicy.IrregularTint) should
      include(Palette.fillTinted(4, FigurePolicy.IrregularTint))
    SvgFigure.toSvg(mixed, "t") should not include Palette.fillTinted(4, FigurePolicy.IrregularTint)

  it should "emit one filled-and-stroked path per face, at the policy stroke" in:
    val pdf = PdfFigure.toPdf(faces)
    pdf.linesIterator.count(_.endsWith(" h B")) shouldBe faces.size
    pdf should include(s"${num(FigurePolicy.StrokeUnits * FigurePolicy.PtPerUnit)} w")

  it should "size the page to the drawing plus its margin" in:
    val pdf = PdfFigure.toPdf(faces)
    // x spans 0..2 and y 0..1, plus 0.25 of white on each side, at PtPerUnit
    pdf should
      include(s"/MediaBox [0 0 ${num(2.5 * FigurePolicy.PtPerUnit)} ${num(1.5 * FigurePolicy.PtPerUnit)}]")

  behavior of "the SVG emitter"

  it should "emit one polygon per face plus the banner" in:
    val svg = SvgFigure.toSvg(faces, "test banner")
    svg should startWith("<svg")
    svg should include("test banner")
    svg.linesIterator.count(_.startsWith("<polygon")) shouldBe faces.size
    svg should include(s"""stroke-width="${FigurePolicy.StrokeUnits}"""")

  it should "write decimal points, whatever the default locale is" in:
    // it_IT would render 0,50 and silently break every SVG this programme emits
    val previous = java.util.Locale.getDefault
    try
      java.util.Locale.setDefault(java.util.Locale.ITALY)
      SvgFigure.toSvg(faces, "banner") should include("1.50000,-0.86600")
      PdfFigure.toPdf(faces) should not include ","
    finally java.util.Locale.setDefault(previous)

  behavior of "the two-backend canvas"

  private val drawing = FigureCanvas.fitted(
    Vector(
      Shape.Poly(faces.head._2, Palette.rgbOf(4)),
      Shape.Seg((0.0, 0.0), (1.0, 1.0)),
      Shape.Label(0.5, 0.5, "(a)", false, 11.0)
    ),
    margin = 2.0,
    strokePt = FigurePolicy.StrokePt
  )

  it should "describe the same drawing in both formats" in:
    val (pdf, eps) = (FigureCanvas.toPdf(drawing), FigureCanvas.toEps(drawing))
    for (pdfOp, epsOp) <-
        Vector("h B" -> "fill grestore stroke", " l S" -> "lineto stroke", "Tj ET" -> ") show")
    do pdf.linesIterator.count(_.endsWith(pdfOp)) shouldBe eps.linesIterator.count(_.endsWith(epsOp))

  it should "keep the EPS pure ASCII with an outward-rounded bounding box" in:
    val eps = FigureCanvas.toEps(drawing)
    // an 8-bit byte in an EPS is a font-encoding accident waiting to happen in production
    eps.forall(_ < 128) shouldBe true
    eps should
      include(s"%%BoundingBox: 0 0 ${math.ceil(drawing.width).toInt} ${math.ceil(drawing.height).toInt}")

  it should "crop to the drawing's extent plus the margin" in:
    val fig = FigureCanvas.fitted(Vector(Shape.Seg((3.0, 5.0), (7.0, 11.0))), margin = 2.0, strokePt = 1.0)
    fig.width shouldBe 8.0 +- 1e-12   // 4 wide + 2 either side
    fig.height shouldBe 10.0 +- 1e-12 // 6 tall + 2 either side
    fig.shapes.head match
      case Shape.Seg(a, _) => (a.x, a.y) shouldBe (2.0, 2.0)
      case other           => fail(s"expected the segment back, got $other")

  it should "bound an S-curved path by its control polygon" in:
    val square = Vector((0.0, 0.0): Pt, (1.0, 0.0), (1.0, 1.0), (0.0, 1.0))
    val flat   = FigureCanvas.fitted(Vector(Shape.Curved(square, 0.0, None, 1.0, (0, 0, 0))), 0.0, 1.0)
    val bulged = FigureCanvas.fitted(Vector(Shape.Curved(square, 0.3, None, 1.0, (0, 0, 0))), 0.0, 1.0)
    flat.width shouldBe 1.0 +- 1e-12 // zero bulge is the polygon itself
    bulged.width should be > flat.width // the control points push the box out

  behavior of "the palette"

  it should "convert every fill to the components the PDF and PostScript backends want" in:
    for p <- Vector(3, 4, 5, 6, 7, 8, 9, 10, 12) do
      val (r, g, b) = Palette.rgbOf(p)
      for c <- Vector(r, g, b) do c should (be >= 0.0 and be <= 1.0)
      val hex       = f"#${(r * 255).round}%02x${(g * 255).round}%02x${(b * 255).round}%02x"
      hex shouldBe Palette.fillOf(p)

  it should "fall back to grey for a polygon size it does not know" in:
    // the giant gons stay deliberately unmapped — each is the unique biggest polygon of its figure
    Palette.fillOf(42) shouldBe "#cccccc"
