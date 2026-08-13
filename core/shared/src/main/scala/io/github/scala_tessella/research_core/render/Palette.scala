package io.github.scala_tessella.research_core.render

/** The fill colour of a face, by polygon size — shared by every emitter so the SVG atlas, the PDF panels and
  * the journal composites cannot disagree about what a hexagon looks like.
  *
  * Greyscale was measured on 2026-08-12 and accepted as is: the pairs that share a figure separate by ΔL* =
  * 33 (3 vs 12), 24 (3 vs 4) and 18 (3 vs 6), but 4 vs 6 is only 5.8 and 4 vs 8 is 8.8 — near-identical greys
  * in a black-and-white printing. Accepted because the tiles are told apart by SHAPE, not fill (polygon shape
  * is the subject), no caption names a colour, and these values feed the published panels. Revisit only if a
  * figure ever distinguishes tiles by fill alone.
  */
object Palette:

  val fillOf: Map[Int, String] = Map(
    3  -> "#f4d35e",
    4  -> "#ee6c4d",
    6  -> "#7fb069",
    8  -> "#9b5de5",
    12 -> "#3d84a8"
  ).withDefaultValue("#cccccc")

  /** The same colour as PDF/PostScript wants it: three components in `[0, 1]`. */
  def rgbOf(p: Int): (Double, Double, Double) =
    val hex        = fillOf(p).drop(1)
    def ch(i: Int) = Integer.parseInt(hex.substring(2 * i, 2 * i + 2), 16) / 255.0
    (ch(0), ch(1), ch(2))
