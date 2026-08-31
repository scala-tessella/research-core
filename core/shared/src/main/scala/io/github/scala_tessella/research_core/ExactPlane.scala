package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.Cyc

/** The exact plane on the 2π/N direction lattice (N even): a unit edge IS a root of unity ζ_N^d, a point IS
  * the ℤ[ζ_N] sum of the unit steps that reach it, and a polygon IS its cyclic sequence of edge directions.
  * Non-unit edges, drifting vertices and inexact coincidences are unrepresentable — the guardrails the
  * numeric de-fusion scout lacked.
  *
  * Angle bookkeeping is in integer units of 2π/N throughout: an interior angle of u units is u·(2π/N)
  * radians; π is N/2 units. A corner's EXTERIOR TURN between consecutive edge directions is `d_i − d_{i−1}`
  * normalized to (−N/2, N/2), and interior = N/2 − turn, so convex corners have turn > 0, reflex corners turn
  * < 0, and the forbidden straight corner is turn = 0. A closed simple CCW polygon's turns sum to N.
  */
object ExactPlane:

  /** Orientation of the triangle (a, b, c): +1 CCW, −1 CW, 0 collinear — the exact sign of the cross product
    * Im(conj(b − a)·(c − a)).
    */
  def orientation(a: Cyc, b: Cyc, c: Cyc): Int = ((b - a).conj * (c - a)).imSign

  /** Is `u` on the CLOSED segment [a, b]? Exact: collinear with non-negative projections. */
  def onSegment(a: Cyc, b: Cyc, u: Cyc): Boolean =
    orientation(a, b, u) == 0 &&
      ((b - a).conj * (u - a)).reSign >= 0 &&
      ((a - b).conj * (u - b)).reSign >= 0

  /** Do the CLOSED segments [a, b] and [c, d] share any point? Exact, all degeneracies included: proper
    * crossings by strict opposite orientations, touches and collinear overlaps by endpoint containment.
    */
  def segmentsIntersect(a: Cyc, b: Cyc, c: Cyc, d: Cyc): Boolean =
    val o1 = orientation(a, b, c)
    val o2 = orientation(a, b, d)
    val o3 = orientation(c, d, a)
    val o4 = orientation(c, d, b)
    (o1 * o2 < 0 && o3 * o4 < 0) ||
    (o1 == 0 && onSegment(a, b, c)) ||
    (o2 == 0 && onSegment(a, b, d)) ||
    (o3 == 0 && onSegment(c, d, a)) ||
    (o4 == 0 && onSegment(c, d, b))

  /** EXACT area comparison of two closed polygons on the same lattice: the sign of area(a) − area(b) — ties
    * are algebraic facts, not numeric coincidences.
    */
  def areaCompare(a: UnitPolygon, b: UnitPolygon): Int = (a.doubleArea - b.doubleArea).imSign

  /** A unit-edge polygon as the cyclic sequence of its edge directions (each edge one unit step ζ_N^d),
    * traversed CCW. Corner i sits between edge i−1 and edge i.
    */
  final case class UnitPolygon(n: Int, dirs: Vector[Int]):
    require(n >= 2 && n % 2 == 0, s"even direction lattice required, got $n")
    require(dirs.length >= 3, "a polygon has at least three edges")

    /** The boundary closes iff the unit steps sum to zero — exactly. */
    def isClosed: Boolean = Cyc.sum(n, dirs.map(Cyc.root(n, _))).isZero

    /** Vertices as exact points, starting from `start`; vertex i is the tail of edge i. */
    def verticesFrom(start: Cyc): Vector[Cyc] =
      dirs.init.scanLeft(start)((p, d) => p + Cyc.root(n, d))

    /** Exterior turn at each corner, normalized to (−N/2, N/2); straight corners (turn 0) and reversals
      * (±N/2) are surfaced, not hidden — callers decide their fate.
      */
    def turns: Vector[Int] =
      dirs.indices.toVector.map: i =>
        val raw = (((dirs(i) - dirs((i - 1 + dirs.length) % dirs.length)) % n) + n) % n
        if raw > n / 2 then raw - n else raw

    /** Interior angles in units of 2π/N: N/2 − turn. */
    def interiorAngles: Vector[Int] = turns.map(n / 2 - _)

    /** No corner is straight (angle π) or a full reversal (angle 0 or 2π). */
    def cornersSane: Boolean = turns.forall(t => t != 0 && t.abs != n / 2)

    /** Some PROPER contiguous run of edges returns to its starting point — the boundary touches itself at a
      * vertex (the degeneracy class of the two-triangles-and-a-slit "octagon"). Exact, via vanishing sums of
      * roots of unity.
      */
    def selfTouches: Boolean =
      val m = dirs.length
      (0 until m).exists: from =>
        (1 until m).exists: len =>
          Cyc.sum(n, (0 until len).map(j => Cyc.root(n, dirs((from + j) % m)))).isZero

    /** Two NON-ADJACENT boundary edges share a point — proper crossings included, which the vertex-only
      * `selfTouches` cannot see (the unit pentagram passes it). Assumes a closed polygon; anchor-free.
      */
    def selfIntersects: Boolean =
      val m         = dirs.length
      val vs        = verticesFrom(Cyc.zero(n))
      def v(k: Int) = vs(k % m)
      (0 until m).exists: i =>
        (i + 2 until m).exists: j =>
          !(i == 0 && j == m - 1) && segmentsIntersect(vs(i), v(i + 1), vs(j), v(j + 1))

    /** The exact embedded-CCW-polygon certificate: closed, winding number 1 (turns sum to N), sane corners,
      * and no two non-adjacent edges sharing a point.
      */
    def isSimpleCertified: Boolean = isClosed && turns.sum == n && cornersSane && !selfIntersects

    /** The shoelace element over the zero-anchored vertices: its IMAGINARY part is twice the signed area,
      * translation-invariant (the element itself is not) — compare areas exactly via
      * [[ExactPlane.areaCompare]].
      */
    def doubleArea: Cyc =
      val vs = verticesFrom(Cyc.zero(n))
      Cyc.sum(n, vs.indices.map(i => vs(i).conj * vs((i + 1) % vs.length)))

    /** Numeric area — for reporting only, never for decisions. */
    def areaApprox: Double = doubleArea.approx._2 / 2
