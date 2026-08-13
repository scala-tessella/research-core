package io.github.scala_tessella.research_core.render

/** THE stroke policy for every figure this programme emits — one place, so the next journal rule lands once.
  *
  * WHY IT IS A POLICY AND NOT A NUMBER PER EMITTER. On 2026-08-11 the paper figures were found to violate a
  * journal artwork guide requiring printed line weights of [[MinPt]]–[[MaxPt]]: they printed at ~0.31 pt. The
  * one-line fix had to be applied in two repositories and still missed two further emitters, because each had
  * grown its own constant. They now all read from here.
  *
  * TWO WAYS TO SPELL A STROKE, and the difference matters:
  *
  *   - [[StrokePt]] — printed points, for artwork emitted AT FINAL SIZE (the composites of
  *     `CompositeFigure`). This number IS the printed number; nothing scales it.
  *   - [[StrokeUnits]] — tiling units, for a standalone panel that LaTeX will place at some fraction of its
  *     intrinsic width. What prints is [[printedPt]], and the placement factor is not knowable here — so
  *     re-check with [[inBand]] whenever a figure is placed at much under half its intrinsic width.
  */
object FigurePolicy:

  /** Floor for printed line weight. Below it, lines drop out in production. */
  inline val MinPt = 0.35

  /** Ceiling for printed line weight. */
  inline val MaxPt = 1.5

  /** Printed stroke for artwork emitted at final size. 0.8 pt sits mid-band and, crucially, still clears
    * [[MinPt]] (at 0.4 pt) if the figure is reduced to half its supplied width — which is what would happen
    * if a full-page composite were set in a single 8.85 cm column.
    */
  inline val StrokePt = 0.8

  /** Points per tiling unit for a standalone panel. */
  inline val PtPerUnit = 22.0

  /** Stroke width in tiling units, for panels and for the SVG atlas (whose viewBox is in tiling units too).
    *
    * Raised 0.03 -> 0.05 on 2026-08-11. The arithmetic, for whoever revisits this: at [[PtPerUnit]] a panel
    * is ~447 pt wide intrinsically and is placed at 75 mm (212.6 pt) in a 2x2 composite, a factor of 0.475 —
    * so 0.03 gave 0.31 pt (under [[MinPt]]) while 0.05 gives 0.52 pt.
    *
    * The same 0.05 was re-derived independently for the honeycomb renderer on 2026-08-13, which draws at 26
    * pt per unit edge and is placed at ~0.60 of intrinsic width: its old 0.012 printed at ~0.19 pt, and 0.05
    * gives 0.78 pt. Two different scale systems, one constant — which is the point.
    */
  inline val StrokeUnits = 0.05

  /** What a `strokeUnits`-wide stroke actually prints at, for a drawing emitted at `ptPerUnit` and then
    * placed at `placed / intrinsic` of its width. Strokes scale with placement; this is the whole trap.
    */
  def printedPt(strokeUnits: Double, ptPerUnit: Double, placedOverIntrinsic: Double): Double =
    strokeUnits * ptPerUnit * placedOverIntrinsic

  /** Whether a PRINTED width satisfies the guide. */
  def inBand(printedPt: Double): Boolean =
    printedPt >= MinPt && printedPt <= MaxPt
