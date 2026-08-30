package io.github.scala_tessella.research_core.render

/** The fill colour of a face, by polygon size — shared by every emitter so the SVG atlas, the PDF panels and
  * the journal composites cannot disagree about what a hexagon looks like.
  *
  * Greyscale was measured on 2026-08-12 and accepted as is: the pairs that share a figure separate by ΔL* =
  * 33 (3 vs 12), 24 (3 vs 4) and 18 (3 vs 6), but 4 vs 6 is only 5.8 and 4 vs 8 is 8.8 — near-identical greys
  * in a black-and-white printing. Accepted because the tiles are told apart by SHAPE, not fill (polygon shape
  * is the subject), no caption names a colour, and these values feed the published panels. Revisit only if a
  * figure ever distinguishes tiles by fill alone.
  *
  * The revisit clause fired on 2026-08-19: the minimal-uniformity figures for (5².10) and (4.5.20) pair
  * PENTAGONS with 10- and 20-gons, and all of them fell to the default grey — indistinguishable by fill in
  * figures where fill is the only cue between two convex near-round shapes. Pentagons therefore join the
  * canonical map as deep rose; the large gons stay default grey (each is the unique biggest polygon of its
  * figure, told apart by size alone). Greyscale: 5 vs the default grey separates by ΔL* ≈ 35, 5 vs 4 by ≈ 14.
  *
  * Extended the same day to the remaining regular sizes the minimal-uniformity species use: 7 (teal, beside
  * yellow triangles and default-grey 42-gons), 9 (violet-blue, beside triangles and grey 18-gons) and 10
  * (ochre, beside rose pentagons, triangles and grey 15-gons). Greyscale: each separates from its figure's
  * grey companion by ΔL* ≥ 23; 10 vs 5 is ≈ 7 — accepted on the shape-carries-the-signal ground of 4 vs 6 (a
  * decagon and a pentagon cannot be confused by outline). The giant gons (15, 18, 20, 24, 42) stay default
  * grey: each is the unique biggest polygon of its figure.
  *
  * The 15-gon joined on 2026-08-30 (dark denim): in the $(3.10.15)$ figure it is NOT the biggest polygon — an
  * irregular 36-gon is — so it could not stay default grey once irregular tiles took a lighter tone of their
  * size's colour and the two greys met. Greyscale: 15 vs the yellow triangles ΔL* ≈ 45, vs the ochre decagons
  * ≈ 12, vs the lightened grey 36-gon ≈ 50. The 16-gon followed the same day (forest green): no regular
  * 16-gon is drawn anywhere, but the $(4.5.20)$ figure's reflex 16-gons are irregular tiles, and their
  * lightened tone — a soft green — separates from the orange squares, rose pentagons and grey 20-gons in a
  * way a lightened default grey does not.
  */
object Palette:

  val fillOf: Map[Int, String] = Map(
    3  -> "#f4d35e",
    4  -> "#ee6c4d",
    5  -> "#c9366f",
    6  -> "#7fb069",
    7  -> "#2a9d8f",
    8  -> "#9b5de5",
    9  -> "#5e60ce",
    10 -> "#bc6c25",
    12 -> "#3d84a8",
    15 -> "#3f5f8a",
    16 -> "#3a7d44"
  ).withDefaultValue("#cccccc")

  /** The same colour as PDF/PostScript wants it: three components in `[0, 1]`. */
  def rgbOf(p: Int): (Double, Double, Double) =
    val hex        = fillOf(p).drop(1)
    def ch(i: Int) = Integer.parseInt(hex.substring(2 * i, 2 * i + 2), 16) / 255.0
    (ch(0), ch(1), ch(2))

  /** The fill of an IRREGULAR `p`-gon: the regular `p`-gon's colour pushed towards white by `tint` (0 keeps
    * it, 1 is white). Same hue, distinguishably lighter — an irregular hexagon among regular ones, or a star
    * dodecagon among regular dodecagons, reads at a glance and still says what it is fused from.
    */
  def rgbTinted(p: Int, tint: Double): (Double, Double, Double) =
    val (r, g, b) = rgbOf(p)
    (r + (1 - r) * tint, g + (1 - g) * tint, b + (1 - b) * tint)

  /** [[rgbTinted]] as the hex string the SVG atlas writes. */
  def fillTinted(p: Int, tint: Double): String =
    val (r, g, b)     = rgbTinted(p, tint)
    def hx(v: Double) = f"${(v * 255).round.toInt.max(0).min(255)}%02x"
    s"#${hx(r)}${hx(g)}${hx(b)}"
