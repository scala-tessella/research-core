package io.github.scala_tessella.research_core.render

/** Emit developed faces as a self-contained SVG — the atlas format, read on screen and in a browser. Takes
  * faces, not a symbol: WHICH tiling these are and how they were developed is the caller's business (the
  * barycentric [[io.github.scala_tessella.research_core.SymbolRenderer.develop]], or a moduli-point developer
  * that lives with its own symbol type), and this file is only the format.
  *
  * IO-free, like everything in `core`: [[toSvg]] returns the document as a string. `tint` (default 0)
  * lightens the irregular faces as [[PdfFigure.toPdf]] does.
  */
object SvgFigure:

  // Locale.ROOT: the default locale may use comma decimals (e.g. it_IT), which breaks SVG points
  private def fmt(v: Double): String = String.format(java.util.Locale.ROOT, "%.5f", v)

  /** Render developed faces as a self-contained SVG with a banner line (types, level) baked in. The viewBox
    * is in tiling units; explicit pixel `width`/`height` (~80 px per unit edge) make viewers open it at a
    * sensible size instead of unit-scale (~13 px).
    */
  def toSvg(faces: Vector[(Int, Vector[Pt])], banner: String, tint: Double = 0.0): String =
    val all              = faces.flatMap(_._2)
    val (xs, ys)         = (all.map(_.x), all.map(_.y))
    val (x0, y0, x1, y1) = (xs.min - 0.2, ys.min - 0.2, xs.max + 0.2, ys.max + 0.2)
    val bannerH          = (y1 - y0) * 0.06
    val pxPerUnit        = 80.0
    val (wPx, hPx)       = ((x1 - x0) * pxPerUnit, (y1 - y0 + bannerH) * pxPerUnit)
    val sb               = StringBuilder()
    sb ++=
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="${wPx.round}" height="${hPx.round}" viewBox="$x0 ${-y1 -
          bannerH} ${x1 - x0} ${y1 - y0 + bannerH}">\n"""
    sb ++= s"""<text x="${x0 + 0.1}" y="${-y1 - bannerH * 0.25}" font-size="${bannerH *
        0.6}" font-family="monospace">$banner</text>\n"""
    for (p, pts) <- faces do
      val d = pts.map((x, y) => s"${fmt(x)},${fmt(-y)}").mkString(" ")
      // the viewBox is in tiling units, so the stroke is too: FigurePolicy.StrokeUnits, not a literal
      sb ++=
        s"""<polygon points="$d" fill="${
            if tint > 0 && !FaceShape.isRegular(pts) then Palette.fillTinted(p, tint) else Palette.fillOf(p)
          }" stroke="#222" stroke-width="${FigurePolicy.StrokeUnits}"/>\n"""
    sb ++= "</svg>\n"
    sb.toString
