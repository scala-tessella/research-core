package io.github.scala_tessella.research_core.render

/** What an emitter can tell about a face from its boundary alone: whether it is REGULAR. Unit edges are the
  * caller's business (every developer here emits them); on unit edges a polygon is regular iff its boundary
  * turns by the same angle at every corner.
  */
object FaceShape:

  /** Regular iff every corner turns by the same signed angle, to `tol` radians — loose enough for hand-typed
    * three-decimal fixtures, far below the 60° an irregular lattice corner differs by.
    */
  def isRegular(pts: Vector[Pt], tol: Double = 1e-3): Boolean =
    val m = pts.length
    if m < 3 then false
    else
      val turns = pts.indices.map { i =>
        val (a, b, c) = (pts(i), pts((i + 1) % m), pts((i + 2) % m))
        val (ux, uy)  = (b.x - a.x, b.y - a.y)
        val (vx, vy)  = (c.x - b.x, c.y - b.y)
        math.atan2(ux * vy - uy * vx, ux * vx + uy * vy)
      }
      turns.forall(t => math.abs(t - turns.head) < tol)
