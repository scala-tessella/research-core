package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.research_core.TilePatch.{PlacedTile, State}

/** TRANSLATION LATTICE of a periodic patch and its quotient census — the chamber-count side of the
  * lexicographic endpoint rule.
  *
  * Numeric values appear only as GUIDANCE (candidate ordering, integer rounding); every acceptance is exact:
  * a translation is verified by exact tile matching wherever the patch can check it, lattice equivalence
  * solves the integer combination numerically then verifies it in ℤ[ζ_N], and the census certifies itself by
  * the exact identity Σ tile-class areas = cell area. Counts are per PRIMITIVE-CANDIDATE cell and may sit on
  * a sublattice of the true translation group when the patch is small — which is why the comparable quantity
  * is the DENSITY chambers/area ([[densityCompare]]), invariant under sublattice choice.
  */
object Periodicity:

  /** |v|² as a ring element (v·v̄, real); exact comparison via `reSign`. */
  def norm2(v: Cyc): Cyc = v * v.conj

  /** Is |a| < |b|, exactly? */
  def norm2Less(a: Cyc, b: Cyc): Boolean = (norm2(b) - norm2(a)).reSign > 0

  /** Sign of the cross product Im(ā·b) — 0 iff parallel. */
  def crossSign(a: Cyc, b: Cyc): Int = (a.conj * b).imSign

  /** The same placed tile with its direction word rotated to its least position — one canonical (word,
    * anchor) per oriented placement, so equal words mean equal orientation. (Words are aperiodic: a periodic
    * word would close a proper sub-walk, i.e. self-touch.)
    */
  def canonicalPlacement(pt: PlacedTile): PlacedTile =
    import scala.math.Ordering.Implicits.seqOrdering
    val ds = pt.poly.dirs
    val s  = ds.indices.minBy(s => ds.drop(s) ++ ds.take(s))
    PlacedTile(UnitPolygon(pt.poly.n, ds.drop(s) ++ ds.take(s)), pt.vertices(s))

  /** Does translation by `t` map the state onto itself wherever the patch can check it? Every tile whose
    * translated vertices are all interior patch vertices must map onto a tile of the same word, and EVERY
    * congruence class present must contribute such a witness — a patch that is only locally periodic in its
    * small tiles must not certify a translation the big irregulars never test (the cand1 |t| = 2 trap).
    */
  def verifiedTranslation(state: State, t: Cyc): Boolean =
    val canon     = state.tiles.map(canonicalPlacement)
    val tileSet   = canon.map(pt => (pt.anchor.reducedKey, pt.poly.dirs)).toSet
    val interior  = state.interiorWords.keySet
    val witnesses = collection.mutable.Set.empty[Vector[Int]]
    val ok        = canon.forall: pt =>
      if pt.vertices.forall(v => interior((v + t).reducedKey)) then
        val mapped = tileSet(((pt.anchor + t).reducedKey, pt.poly.dirs))
        if mapped then witnesses += TilePatch.shapeKey(pt.poly)
        mapped
      else true
    ok && canon.map(pt => TilePatch.shapeKey(pt.poly)).toSet.subsetOf(witnesses)

  /** Candidate translations, most informative first: anchor differences within same-word groups of the
    * LARGEST-AREA shape class (sparse lattice markers — the big irregular tiles sit one per orbit per cell,
    * so their same-orientation differences concentrate on the true lattice), then the most numerous same-word
    * group as a dense fallback. Deduped by exact value, each block shortest first.
    */
  def candidateTranslations(state: State): Vector[Cyc] =
    val canon = state.tiles.map(canonicalPlacement)
    if canon.isEmpty then Vector.empty
    else
      def diffs(g: Vector[PlacedTile]): Vector[Cyc] =
        g.groupBy(_.poly.dirs)
          .toVector
          .sortBy(_._1.mkString(","))
          .flatMap: (_, ws) =>
            for
              i <- ws.indices.toVector
              j <- ws.indices.toVector
              if i != j
            yield ws(j).anchor - ws(i).anchor
      def byLen(ds: Vector[Cyc]): Vector[Cyc]       =
        ds.sortBy { d =>
          val (x, y) = d.approx; x * x + y * y
        }
      val marker                                    = canon
        .groupBy(pt => TilePatch.shapeKey(pt.poly))
        .toVector
        .sortBy(_._1.mkString(","))
        .maxBy(_._2.head.poly.areaApprox)
        ._2
      val dense                                     = canon.groupBy(_.poly.dirs).values.maxBy(_.length)
      (byLen(diffs(marker)) ++ byLen(diffs(dense)))
        .distinctBy(_.reducedKey)
        .filterNot(_.isZero)

  /** Lagrange-reduce a rank-2 basis: numeric rounding proposes, exact norms accept. */
  @annotation.tailrec
  def reduceBasis(a0: Cyc, b0: Cyc): (Cyc, Cyc) =
    val (a, b)   = if norm2Less(b0, a0) then (b0, a0) else (a0, b0)
    val (ax, ay) = a.approx
    val (bx, by) = b.approx
    val k        = math.round((ax * bx + ay * by) / (ax * ax + ay * ay)).toInt
    val c        = b - a.scaled(k)
    if norm2Less(c, b) then reduceBasis(a, c) else (a, b)

  /** Two shortest independent VERIFIED translations, Lagrange-reduced; None if the patch does not reveal a
    * rank-2 lattice among the first `tryLimit` candidates.
    */
  def latticeBasis(state: State, tryLimit: Int = 48): Option[(Cyc, Cyc)] =
    val verified = candidateTranslations(state).take(tryLimit).filter(verifiedTranslation(state, _))
    for
      t1 <- verified.headOption
      t2 <- verified.find(crossSign(t1, _) != 0)
    yield reduceBasis(t1, t2)

  /** The certified quotient: iterate verified basis pairs (shortest first), pre-filter by the exact necessary
    * condition 2·cellArea ≥ Σ distinct-shape areas (each congruence class needs at least one translation
    * class per cell), and accept the first census whose area identity certifies. The certificate is the
    * soundness gate — verification alone can be fooled by local periodicity.
    */
  def certifiedCell(state: State, tryLimit: Int = 400, pairLimit: Int = 12): Option[(Cyc, Cyc, CellCensus)] =
    val verified   = candidateTranslations(state).take(tryLimit).filter(verifiedTranslation(state, _))
    val shapeAreas = state.tiles
      .map(pt => (TilePatch.shapeKey(pt.poly), pt.poly))
      .distinctBy(_._1)
      .map(_._2.doubleArea)
      .reduceOption(_ + _)
    val pairs      =
      for
        i <- verified.indices.iterator
        j <- (i + 1 until verified.length).iterator
        if crossSign(verified(i), verified(j)) != 0
      yield reduceBasis(verified(i), verified(j))
    pairs
      .take(pairLimit)
      .filter: (t1, t2) =>
        shapeAreas.forall(sa => (cellAreaElem(t1, t2).scaled(2) - sa).imSign >= 0)
      .map((t1, t2) => (t1, t2, cellCensus(state, t1, t2)))
      .find(_._3.areaCertified)

  /** The cell-area element Im-oriented positive: its imaginary part IS the cell area. */
  def cellAreaElem(t1: Cyc, t2: Cyc): Cyc =
    val e = t1.conj * t2
    if e.imSign >= 0 then e else -e

  /** Lattice equivalence of two points: the integer combination is solved numerically on the reduced basis
    * (±1 safety window) and then verified exactly.
    */
  def latticeEquiv(t1: Cyc, t2: Cyc)(a: Cyc, b: Cyc): Boolean =
    val d        = b - a
    val (dx, dy) = d.approx
    val (x1, y1) = t1.approx
    val (x2, y2) = t2.approx
    val det      = x1 * y2 - y1 * x2
    val m0       = math.round((dx * y2 - dy * x2) / det).toInt
    val n0       = math.round((x1 * dy - y1 * dx) / det).toInt
    (for m <- m0 - 1 to m0 + 1; n <- n0 - 1 to n0 + 1 yield (m, n))
      .exists((m, n) => (t1.scaled(m) + t2.scaled(n)) === d)

  /** Group by an equivalence given as a relation, not a key — the classes a census reads off (congruent
    * tiles, translation-equal points) come from exact predicates that no hashable key summarises. Quadratic
    * in the number of classes, which is what a per-cell census has.
    */
  def partitionBy[A](xs: Vector[A])(eq: (A, A) => Boolean): Vector[Vector[A]] =
    xs.foldLeft(Vector.empty[Vector[A]]): (acc, x) =>
      acc.indexWhere(g => eq(g.head, x)) match
        case -1 => acc :+ Vector(x)
        case i  => acc.updated(i, acc(i) :+ x)

  /** The per-cell census under the given lattice: tile classes, Delaney chambers (Σ 2p), vertex classes over
    * interior vertices, and the EXACT self-certificate that the tile classes tile the cell (Σ class areas =
    * cell area).
    */
  final case class CellCensus(
      tileClasses: Vector[UnitPolygon],
      chambers: Int,
      vertexClasses: Int,
      cellArea: Cyc,
      areaCertified: Boolean
  )

  def cellCensus(state: State, t1: Cyc, t2: Cyc): CellCensus =
    val equiv    = latticeEquiv(t1, t2)
    val classes  = state.tiles
      .map(canonicalPlacement)
      .groupBy(_.poly.dirs)
      .toVector
      .sortBy(_._1.mkString(","))
      .flatMap((_, ps) => partitionBy(ps)((a, b) => equiv(a.anchor, b.anchor)).map(_.head.poly))
    val interior = state.interiorWords.keySet
    val vreps    = state.tiles
      .flatMap(_.vertices)
      .filter(v => interior(v.reducedKey))
      .distinctBy(_.reducedKey)
    val vclasses = partitionBy(vreps)(equiv).length
    val cell     = cellAreaElem(t1, t2)
    val sumAreas = classes.map(_.doubleArea).reduce(_ + _)
    CellCensus(
      classes,
      classes.map(2 * _.dirs.length).sum,
      vclasses,
      cell,
      (sumAreas - cell.scaled(2)).imSign == 0 // Σ 2·area(class) = 2·cellArea, exactly
    )

  /** Sign of density(a) − density(b), density = chambers / cell area — the sublattice- invariant chamber
    * tiebreak, compared exactly by cross-multiplication.
    */
  def densityCompare(a: (Int, Cyc), b: (Int, Cyc)): Int = (b._2.scaled(a._1) - a._2.scaled(b._1)).imSign
