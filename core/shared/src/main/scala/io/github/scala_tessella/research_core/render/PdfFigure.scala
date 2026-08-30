package io.github.scala_tessella.research_core.render

/** Emit developed faces as a self-contained VECTOR PDF (one page, path fill+stroke operators only) — the
  * format the papers actually `\includegraphics`. Colours are shared with the SVG atlas ([[Palette]]), the
  * container with every other PDF here ([[PdfDocument]]). PDF's origin is bottom-left with y up, matching the
  * tiling coordinates directly (the SVG path needs a y-flip; this does not).
  *
  * A SPECIAL CASE OF [[FigureCanvas]] in everything but its bytes: one page, one stroke width, filled
  * polygons and no text. It is kept separate on purpose — its output is the 20 published panels, byte-frozen,
  * and `FigureCanvas` emits a `/Font` resource these do not have. Re-expressing them through it would change
  * every published file for no gain.
  *
  * IO-free, like everything in `core`: [[toPdf]] returns the document as a string and writing it is the
  * caller's business. It used to write the file itself, which is why it could not live here.
  *
  * `tint` (default 0, byte-frozen output) lightens the IRREGULAR faces towards white — same hue as the
  * regular polygon of that size, distinguishably lighter ([[Palette.rgbTinted]],
  * [[FigurePolicy.IrregularTint]]).
  */
object PdfFigure:

  private def num(v: Double): String = String.format(java.util.Locale.ROOT, "%.2f", v)

  def toPdf(faces: Vector[(Int, Vector[Pt])], tint: Double = 0.0): String =
    import FigurePolicy.{PtPerUnit, StrokeUnits}
    val all      = faces.flatMap(_._2)
    val (x0, y0) = (all.map(_.x).min - 0.25, all.map(_.y).min - 0.25)
    val (x1, y1) = (all.map(_.x).max + 0.25, all.map(_.y).max + 0.25)
    val (w, h)   = ((x1 - x0) * PtPerUnit, (y1 - y0) * PtPerUnit)
    val content  = StringBuilder()
    content ++= s"${num(StrokeUnits * PtPerUnit)} w 0.13 0.13 0.13 RG 1 j 1 J\n"
    for (p, pts) <- faces do
      val (r, g, b) =
        if tint > 0 && !FaceShape.isRegular(pts) then Palette.rgbTinted(p, tint) else Palette.rgbOf(p)
      content ++= s"${num(r)} ${num(g)} ${num(b)} rg\n"
      content ++=
        pts.zipWithIndex
          .map((pt, i) =>
            s"${num((pt.x - x0) * PtPerUnit)} ${num((pt.y - y0) * PtPerUnit)} ${
                if i == 0 then "m" else "l"
              }"
          )
          .mkString(" ") + " h B\n"
    val stream   = content.toString
    PdfDocument(
      PdfDocument.preamble ++
        Vector(PdfDocument.page(w, h, "<< >>"), PdfDocument.contents(stream))
    )
