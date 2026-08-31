package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon

/** DE-FUSION MOVES as exact direction-sequence surgery (`notes/saturation.md`): splitting a regular q-gon off
  * an irregular tile is a rewrite of the tile's cyclic direction word, and every validity condition is an
  * exact certificate — arc regularity is integer arithmetic on the 2π/N lattice, seam angles are integer
  * comparisons against the tile's own corners (never read back from the surgered word, where a poked-through
  * placement would masquerade as reflex), and the remainder must pass the full embedded-CCW certificate. The
  * shoelace identity of the surgery (the shared arc cancels) makes remainder-simplicity + winding 1 the
  * containment proof: area(tile) = area(q-gon) + area(remainder).
  *
  * SHAPE-legality only: whether a split's spliced vertex words stay legal in the tiling is the sampler's
  * concern, on top of these moves.
  */
object Defusion:

  /** Interior angle of the regular q-gon in 2π/N units — an integer exactly when q | N. */
  def regularUnits(n: Int, q: Int): Int =
    require(q >= 3 && n % q == 0, s"no regular $q-gon on the 2π/$n lattice")
    n / 2 - n / q

  /** The CCW regular q-gon whose j-th edge direction is `d0 + j·N/q`. */
  def regularGon(n: Int, q: Int, d0: Int): UnitPolygon =
    UnitPolygon(n, Vector.tabulate(q)(j => (((d0 + j * (n / q)) % n) + n) % n))

  /** One remainder component; `rel` is its anchor relative to the remainder walk's start (the arc's tail
    * seam), so callers can place pieces exactly.
    */
  final case class Piece(poly: UnitPolygon, rel: CycloRing.Cyc)

  /** A regular polygon split off a tile; `remainders` is empty iff the tile WAS the q-gon, one piece for a
    * plain flush split, several when the cut PINCHES the tile apart (a corner of the q-gon landing exactly on
    * a far boundary vertex — the fusion-28 square notch).
    */
  final case class Split(regular: UnitPolygon, remainders: Vector[Piece])

  /** Decompose a closed direction walk into embedded CCW polygons, splitting at PINCH vertices (exact
    * revisits). Leaves must pass the full embedding certificate — proper crossings, T-contacts (a vertex
    * inside an edge) and reversed loops (a placement not splitting the tile) are all illegal; only clean
    * pinches (two tiles meeting at a vertex) pass. In `loose` (decomposition-region) mode, straight leaf
    * corners are legal and zero-width two-edge loops (a cut edge coinciding with the boundary) are dropped
    * rather than rejected.
    */
  private def decompose(
      n: Int,
      dirs: Vector[Int],
      start: CycloRing.Cyc,
      loose: Boolean = false
  ): Option[Vector[Piece]] =
    if loose && dirs.length == 2 && (((dirs(1) - dirs(0) - n / 2) % n) + n) % n == 0 then
      Some(Vector.empty) // a doubled edge: zero width, nothing to place
    else if dirs.length < 3 then None
    else
      val vs    = dirs.init.scanLeft(start)((p, d) => p + CycloRing.Cyc.root(n, d))
      val keys  = vs.map(_.reducedKey)
      val pinch =
        (for
          i <- keys.indices.iterator
          j <- (i + 1 until keys.length).iterator
          if keys(i) == keys(j)
        yield (i, j)).nextOption()
      pinch match
        case Some((i, j)) =>
          for
            a <- decompose(n, dirs.slice(i, j), vs(i), loose)
            b <- decompose(n, dirs.drop(j) ++ dirs.take(i), vs(j), loose)
          yield a ++ b
        case None         =>
          val p = UnitPolygon(n, dirs)
          Option.when(
            p.isClosed && p.turns.sum == n && (loose || p.cornersSane) && !p.selfIntersects
          )(Vector(Piece(p, start)))

  /** Split a regular q-gon off `p`, flush along the arc of `l` boundary edges starting at edge `i` (the q-gon
    * inside `p`, sharing exactly those edges). None unless every certificate holds: the arc's directions
    * advance by N/q (so the shared corners are exactly regular), both seam leftovers `corner − (q−2)π/q` are
    * positive and not π, and the remainder is an embedded CCW unit polygon. `l = q` degenerates to
    * recognising `p` as the q-gon itself.
    */
  def splitFlush(p: UnitPolygon, i: Int, l: Int, q: Int): Option[Split] =
    splitFlushCore(p, i, l, q, loose = false)

  /** Decomposition-mode split: straight seams and straight remainder corners are legal — the remainder is a
    * REGION to fill further, not a tile of a tiling.
    */
  def splitFlushLoose(p: UnitPolygon, i: Int, l: Int, q: Int): Option[Split] =
    splitFlushCore(p, i, l, q, loose = true)

  private def splitFlushCore(p: UnitPolygon, i: Int, l: Int, q: Int, loose: Boolean): Option[Split] =
    val n = p.n
    val m = p.dirs.length
    if q < 3 || n % q != 0 || l < 1 || l > q || i < 0 || i >= m then None
    else
      val stepQ = n / q
      val arcOk =
        l <= m && (0 until l).forall(j => p.dirs((i + j) % m) == (((p.dirs(i) + j * stepQ) % n) + n) % n)
      if !arcOk then None
      else if l == q then
        if m == q then Some(Split(regularGon(n, q, p.dirs(i)), Vector.empty)) else None
      else if l >= m then None
      else
        val aq       = regularUnits(n, q)
        val seamHead = p.interiorAngles(i) - aq
        val seamTail = p.interiorAngles((i + l) % m) - aq
        if seamHead < 1 || seamTail < 1 then None
        else if !loose && (seamHead == n / 2 || seamTail == n / 2) then None
        else
          val keep = Vector.tabulate(m - l)(j => p.dirs((i + l + j) % m))
          val cut  = (l until q).reverse.toVector.map(j => (((p.dirs(i) + j * stepQ + n / 2) % n) + n) % n)
          decompose(n, keep ++ cut, CycloRing.Cyc.zero(n), loose).map: pieces =>
            Split(regularGon(n, q, p.dirs(i)), pieces)

  private def isRegularGon(p: UnitPolygon): Boolean =
    val m = p.dirs.length
    p.n % m == 0 && (1 until m).forall(j => p.dirs(j) == (((p.dirs(0) + j * (p.n / m)) % p.n) + p.n) % p.n)

  /** COMPLETE decision: is `p` an edge-to-edge union of unit-edge regular polygons with sizes from `qs`?
    * Exhaustive but pruned: every decomposition has EXACTLY ONE piece owning boundary edge 0, so the search
    * branches only over that piece's identity — the loose flush splits whose arc CONTAINS edge 0 (multi-arc
    * contacts included: coincident cut edges become droppable two-edge loops) — and recurses on the remainder
    * pieces, memoised by congruence key, after an exact area-feasibility gate on ζ₁₂ (the region's (rational,
    * √3) area parts must be a non-negative combination of the piece areas). Sound and complete RELATIVE TO
    * `qs`: the caller must justify the piece universe (Galois-sector and area-part arguments fix it per
    * lattice).
    */
  def regularUnion(p: UnitPolygon, qs: Seq[Int]): Boolean =
    val memo                                  = collection.mutable.Map.empty[Vector[Int], Boolean]
    def key(u: UnitPolygon): Vector[Int]      =
      // congruence key (shift × rotation × reflection), local copy to keep Defusion self-contained
      import scala.math.Ordering.Implicits.seqOrdering
      val n                                  = u.n
      def norm(ds: Vector[Int]): Vector[Int] =
        ds.indices
          .map { s =>
            val rot = ds.drop(s) ++ ds.take(s)
            rot.map(d => (((d - rot.head) % n) + n) % n)
          }
          .min
      val refl                               = u.dirs.map(d => (((n / 2 - d) % n) + n) % n).reverse
      List(norm(u.dirs), norm(refl)).min
    def areaFeasible(u: UnitPolygon): Boolean =
      // ζ₁₂ only: 4·area = c₁ + 2c₃ + c₂√3 must be Σ pieces with 4·areas: 3→√3, 4→4, 6→6√3, 12→24+12√3
      if u.n != 12 then true // gate only where the arithmetic is implemented; other lattices skip it
      else
        val c = u.doubleArea.reducedKey
        (4 until 12).forall(i => c(i) == BigInt(0)) && {
          val fourA = (c(1) + 2 * c(3)).toInt // 4·area rational part ×2? (2·area = Im ⇒ ×2 twice)
          val fourB = c(2).toInt
          // 2·(2·area): rational 2c₁/2·… — work with 4·area = 2·Im: rational = 2c₁/2… simplify:
          // Im = c₁/2 + c₃ + (c₂/2)√3 ⇒ 4·area = 2·Im = c₁ + 2c₃ + c₂√3
          (0 to fourA / 4).exists { s =>
            (0 to (fourA - 4 * s) / 24).exists { d =>
              fourA == 4 * s + 24 * d && {
                val rem = fourB - 12 * d
                rem >= 0 && (0 to rem / 6).exists(h => rem - 6 * h >= 0) // t = rem − 6h ≥ 0
              }
            }
          }
        }
    def go(u: UnitPolygon): Boolean           =
      memo.getOrElseUpdate(
        key(u),
        isRegularGon(u) ||
          (areaFeasible(u) && {
            val m = u.dirs.length
            (for
              q <- qs.distinct.sorted.iterator
              l <- (1 to math.min(q, m)).iterator
              j <- (0 until l).iterator // arc (i, l) contains edge 0 ⟺ i = −j mod m, j < l
              s <- splitFlushLoose(u, ((-j % m) + m) % m, l, q).iterator
            yield s).exists(s => s.remainders.forall(pc => go(pc.poly)))
          })
      )
    go(p)

  /** One admissible flush split: the arc's starting edge and length, the q, the outcome. */
  final case class FlushSplit(edge: Int, arcLen: Int, q: Int, split: Split)

  /** A VERTEX-TOUCH split: a regular q-gon inside `p` sharing NO edge with the boundary, only exact vertex
    * contacts (at least two — a single contact pinches an annulus, which the certificates reject). `anchor`
    * is the boundary vertex carrying the q-gon's vertex 0, `dir0` its first edge direction, `contacts` the
    * boundary vertex indices touched; remainder pieces are anchored relative to `p`'s vertex `anchor`.
    */
  final case class VertexTouch(
      anchor: Int,
      dir0: Int,
      q: Int,
      contacts: Vector[Int],
      remainders: Vector[Piece]
  )

  /** Every vertex-touch split of `p` by regular q-gons with sizes from `qs`: anchor a q-gon corner at each
    * boundary vertex with a sliver of at least one lattice unit on BOTH sides (a zero sliver is a flush
    * contact, enumerated elsewhere), then keep placements whose boundary contact set is vertex-only (no
    * T-contacts, no crossings, no collinear overlap) and whose remainder components all pass the strict tile
    * certificates.
    */
  def vertexTouchSplits(p: UnitPolygon, qs: Seq[Int]): Vector[VertexTouch] =
    val n  = p.n
    val m  = p.dirs.length
    val vs = p.verticesFrom(CycloRing.Cyc.zero(n))
    for
      q <- qs.toVector.distinct.sorted
      if n % q == 0
      aq  = regularUnits(n, q)
      k  <- (0 until m).toVector
      s  <- (p.dirs(k) + 1 to p.dirs(k) + p.interiorAngles(k) - aq - 1).toVector
      vt <- tryVertexTouch(p, vs, k, ((s % n) + n) % n, q)
    yield vt

  private def tryVertexTouch(
      p: UnitPolygon,
      vs: Vector[CycloRing.Cyc],
      k: Int,
      s: Int,
      q: Int
  ): Option[VertexTouch] =
    val n        = p.n
    val m        = p.dirs.length
    val gon      = regularGon(n, q, s)
    val gvs      = gon.verticesFrom(vs(k))
    val vKeys    = vs.map(_.reducedKey)
    val touchOfQ = gvs.map(gv => vKeys.indexOf(gv.reducedKey))
    val contacts = touchOfQ.zipWithIndex.collect { case (pi, t) if pi >= 0 => (t, pi) }
    if contacts.length < 2 then None
    else
      def onInterior(pt: CycloRing.Cyc, a: CycloRing.Cyc, b: CycloRing.Cyc): Boolean =
        ExactPlane.onSegment(a, b, pt) && !(pt === a) && !(pt === b)
      val tContact                                                                   = gvs.zipWithIndex.exists((gv, t) =>
        touchOfQ(t) < 0 && (0 until m).exists(i => onInterior(gv, vs(i), vs((i + 1) % m)))
      ) ||
        vs.zipWithIndex.exists((pv, i) =>
          !contacts.exists(_._2 == i) && (0 until q).exists(t => onInterior(pv, gvs(t), gvs((t + 1) % q)))
        )
      val badEdge                                                                    = (0 until q).exists { t =>
        val (ga, gb) = (gvs(t), gvs((t + 1) % q))
        (0 until m).exists { i =>
          val (pa, pb) = (vs(i), vs((i + 1) % m))
          ExactPlane.segmentsIntersect(ga, gb, pa, pb) && {
            // legal only as one shared endpoint with no collinear run through it
            val shared = List(ga, gb).filter(g => (g === pa) || (g === pb))
            shared.length != 1 || {
              val g2 = if shared.head === ga then gb else ga
              val p2 = if shared.head === pa then pb else pa
              ExactPlane.onSegment(pa, pb, g2) || ExactPlane.onSegment(ga, gb, p2)
            }
          }
        }
      }
      if tContact || badEdge then None
      else
        // remainder components between consecutive contacts along p; the q-gon path between
        // the same contacts runs one of two ways around Q — exactly one global choice covers
        // each Q edge once, and the strict certificates reject the wrong one
        val byP                                                   = contacts.sortBy(_._2)
        def components(reversedQ: Boolean): Option[Vector[Piece]] =
          val pieces = byP.indices.toVector.map { a =>
            val (tA, kA) = byP(a)
            val (tB, kB) = byP((a + 1) % byP.length)
            val pArc     = Vector.tabulate(((kB - kA) % m + m) % m)(j => p.dirs((kA + j) % m))
            val qPath    =
              if reversedQ then
                Vector.tabulate(((tB - tA) % q + q) % q)(j =>
                  (gon.dirs((((tB - 1 - j) % q) + q) % q) + n / 2) % n
                )
              else Vector.tabulate(((tA - tB) % q + q) % q)(j => gon.dirs((tB + j) % q))
            decompose(n, pArc ++ qPath, vs(kA) - vs(k))
          }
          if pieces.forall(_.isDefined) then Some(pieces.flatMap(_.get)) else None
        components(reversedQ = true).orElse(components(reversedQ = false)).map: pieces =>
          VertexTouch(k, s, q, byP.map(_._2), pieces)

  /** Every geometric flush split of `p` by regular q-gons with sizes from `qs`. */
  def flushSplits(p: UnitPolygon, qs: Seq[Int]): Vector[FlushSplit] =
    val m = p.dirs.length
    for
      q <- qs.toVector.distinct.sorted if p.n % q == 0
      i <- (0 until m).toVector
      l <- (1 to math.min(q, m)).toVector
      s <- splitFlush(p, i, l, q)
    yield FlushSplit(i, l, q, s)
