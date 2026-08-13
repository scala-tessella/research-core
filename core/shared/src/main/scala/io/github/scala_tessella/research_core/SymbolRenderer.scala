package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.render.SvgFigure

import scala.collection.mutable

/** Draw a euclidean regular-polygon tiling directly from its (minimal) Delaney–Dress symbol, by BARYCENTRIC
  * DEVELOPMENT: each chamber is the flag triangle (V = vertex, E = edge midpoint, F = face centre) — for a
  * p-gon face a right triangle with |VE| = 1/2, the right angle at E, angle π/p at F. Starting from one
  * placed chamber, BFS across σ₀/σ₁/σ₂ constructs the neighbour's triangle:
  *
  *   - σ₀ (other vertex, same edge+face): V reflected across line EF;
  *   - σ₁ (other edge, same vertex+face): E reflected across line VF;
  *   - σ₂ (other face, same vertex+edge): F re-placed on the far side of VE at the NEIGHBOUR's apothem (its
  *     face size may differ).
  *
  * Developing the QUOTIENT symbol unfolds the orbifold onto the plane, so v-values > 1 and chains (σ-fixed
  * chambers — crossing a mirror lands in the same chamber, mirrored) need no special handling; dedup is by
  * (chamber, rounded geometry). Pure doubles — no ℤ[ζ₁₂] restriction, so 4.8² renders like everything else.
  * Faces are reconstructed by grouping developed corners around each face centre.
  *
  * THE DEVELOPMENT LIVES HERE, THE FORMAT DOES NOT. Emitting is [[render.SvgFigure]]'s job, and takes faces
  * rather than a symbol, so a different developer — one typed on another repository's `DSymbol`, or on a
  * moduli point instead of the regular apothem — reaches the same emitters. [[toSvg]] is forwarded from here
  * for the callers that only ever wanted "develop this symbol and hand me the document".
  *
  * IO-free by design (the `core` module is Scala.js-clean): [[toSvg]] returns the document as a string and
  * writing it is the caller's business — a one-liner over `java.nio.file.Files.writeString`.
  */
object SymbolRenderer:

  type Pt = render.Pt

  def apothem(p: Int): Double      = 0.5 / math.tan(math.Pi / p)
  def circumradius(p: Int): Double = 0.5 / math.sin(math.Pi / p)

  /** Reflection of `pt` across the line through `a` and `b`. */
  def reflect(pt: Pt, a: Pt, b: Pt): Pt =
    val (dx, dy) = (b.x - a.x, b.y - a.y)
    val len2     = dx * dx + dy * dy
    val (vx, vy) = (pt.x - a.x, pt.y - a.y)
    val t        = (vx * dx + vy * dy) / len2
    val (px, py) = (t * dx, t * dy) // v∥
    (a.x + 2 * px - vx, a.y + 2 * py - vy)

  final private case class Cham(c: Int, v: Pt, e: Pt, f: Pt)

  private def round6(x: Double): Long   = math.round(x * 1e6)
  private def key(pt: Pt): (Long, Long) = (round6(pt.x), round6(pt.y))

  /** Develop the symbol out to face centres within `radius` of the origin. Returns the COMPLETE faces:
    * `(p, corners sorted around the centre)`.
    */
  def develop(ds: DSymbol, radius: Double): Vector[(Int, Vector[Pt])] =
    val p1                = ds.m(0, 1, 1)
    val start             = Cham(1, (0.0, 0.0), (0.5, 0.0), (0.5, apothem(p1)))
    val seen              = mutable.Set.empty[(Int, (Long, Long), (Long, Long), (Long, Long))]
    val queue             = mutable.Queue(start)
    // face centre -> (p, corner set)
    val corners           =
      mutable.Map.empty[(Long, Long), (Int, mutable.Set[(Long, Long)], mutable.Map[(Long, Long), Pt])]
    def chamKey(ch: Cham) = (ch.c, key(ch.v), key(ch.e), key(ch.f))
    def dist(pt: Pt)      = math.hypot(pt.x, pt.y)
    while queue.nonEmpty do
      val ch = queue.dequeue()
      if !seen(chamKey(ch)) then
        seen += chamKey(ch)
        val p             = ds.m(0, 1, ch.c)
        val (_, cs, reps) =
          corners.getOrElseUpdate(key(ch.f), (p, mutable.Set.empty, mutable.Map.empty))
        cs += key(ch.v)
        reps.getOrElseUpdate(key(ch.v), ch.v): Unit
        // σ₀: other vertex across the edge
        val n0            = Cham(ds.get(0, ch.c), reflect(ch.v, ch.e, ch.f), ch.e, ch.f)
        // σ₁: other edge at the vertex, same face
        val n1            = Cham(ds.get(1, ch.c), ch.v, reflect(ch.e, ch.v, ch.f), ch.f)
        // σ₂: other face across the edge — re-place F at the neighbour's apothem on the far side of VE
        val c2            = ds.get(2, ch.c)
        val a2            = apothem(ds.m(0, 1, c2))
        val aHere         = apothem(p)
        val f2            = (ch.e.x - a2 / aHere * (ch.f.x - ch.e.x), ch.e.y - a2 / aHere * (ch.f.y - ch.e.y))
        val n2            = Cham(c2, ch.v, ch.e, f2)
        for n <- List(n0, n1, n2) if dist(n.f) <= radius && !seen(chamKey(n)) do queue.enqueue(n)
    corners.toVector.collect:
      case (fk, (p, cs, reps)) if cs.size == p =>
        val centre: Pt = (fk._1 / 1e6, fk._2 / 1e6)
        val pts        = reps.values.toVector.sortBy(v => math.atan2(v.y - centre.y, v.x - centre.x))
        (p, pts)

  /** Render developed faces as a self-contained SVG. Forwards to [[render.SvgFigure.toSvg]], where the format
    * lives.
    */
  def toSvg(faces: Vector[(Int, Vector[Pt])], banner: String): String =
    SvgFigure.toSvg(faces, banner)

  /** Develop and emit one tiling's SVG document. */
  def toSvg(ds: DSymbol, banner: String, radius: Double = 6.0): String =
    SvgFigure.toSvg(develop(ds, radius), banner)
