package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.Defusion.FlushSplit
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon

/** TILING STATES for the de-fusion sampler: an exact patch of placed tiles, its vertex census, and class-wide
  * split application with FULL post-validation.
  *
  * The census is exact — vertices are ℤ[ζ_N] points keyed by canonical reduction, corners are ordered CCW by
  * their sector START ray (bisector ordering breaks on reflex corners — the scout's last bug), and a vertex
  * is checkable iff its corners sum to exactly 2π. Word legality follows the ratified class semantics: an
  * all-regular vertex must BE the species vertex z; a vertex with irregular corners must splice to a
  * contiguous cyclic arc of z ([[UClass.cyclicSubset]], rotation AND reflection). A move applies ONE split
  * variant to EVERY tile of a congruence class and validates the whole resulting state — shared-vertex
  * interference between instances (the pinwheel's symmetry-breaking mechanism) is caught here, never assumed
  * away per instance.
  *
  * SAMPLER SEMANTICS, not proof: vertices the finite patch leaves incomplete are unchecked, so endpoints are
  * evidence about the infinite tiling, to be banked only after symbol-level verification.
  */
object TilePatch:

  type VKey = Vector[BigInt]

  final case class PlacedTile(poly: UnitPolygon, anchor: Cyc):
    def vertices: Vector[Cyc] = poly.verticesFrom(anchor)

  /** Congruence key of a shape: the least direction word over both chiralities, all cyclic shifts,
    * offset-normalised — equal iff the shapes are congruent (unit edges pin scale).
    */
  def shapeKey(p: UnitPolygon): Vector[Int] =
    import scala.math.Ordering.Implicits.seqOrdering
    val n                                  = p.n
    def norm(ds: Vector[Int]): Vector[Int] =
      ds.indices
        .map: s =>
          val rot = ds.drop(s) ++ ds.take(s)
          rot.map(d => (((d - rot.head) % n) + n) % n)
        .min
    val refl                               = p.dirs.map(d => (((n / 2 - d) % n) + n) % n).reverse
    List(norm(p.dirs), norm(refl)).min

  /** Some(q) iff the shape IS the regular q-gon. */
  def regularSizeOf(p: UnitPolygon): Option[Int] =
    val m = p.dirs.length
    Option.when(p.n % m == 0 && shapeKey(p) == Vector.tabulate(m)(j => j * (p.n / m)))(m)

  /** One tile corner at a vertex: CCW sector start ray, angle units, regular letter or None. */
  final case class Corner(tile: Int, startRay: Int, angle: Int, letter: Option[Int])

  /** `strict` judges every vertex under the strict contiguous-arc reading ([[UClass.strictArcLegal]]) —
    * saturation is class-relative, and the strict class forbids moves the spliced class allows (and vice
    * versa, a strict-illegal state being spliced-legal never, the strict class being the smaller).
    */
  final case class State(
      n: Int,
      z: List[Int],
      tiles: Vector[PlacedTile],
      strict: Boolean = false,
      isolated: Boolean = false
  ):
    /** Corners grouped by exact vertex, each group CCW-ordered by sector start. */
    lazy val cornersByVertex: Map[VKey, Vector[Corner]] =
      val cs =
        for
          (t, ti) <- tiles.zipWithIndex
          letter   = regularSizeOf(t.poly)
          (v, k)  <- t.vertices.zipWithIndex
        yield (v.reducedKey, Corner(ti, ((t.poly.dirs(k) % n) + n) % n, t.poly.interiorAngles(k), letter))
      cs.groupMap(_._1)(_._2).view.mapValues(_.sortBy(_.startRay)).toMap

    /** The checkable vertices: corner angles summing to exactly 2π. */
    def interiorWords: Map[VKey, Vector[Option[Int]]] =
      cornersByVertex.collect:
        case (v, cs) if cs.map(_.angle).sum == n => v -> cs.map(_.letter)

  /** Legality of one CCW vertex word (None = irregular corner): all-regular must BE the species vertex;
    * otherwise the spliced regular letters must form a contiguous cyclic arc of z (an empty splice is
    * vacuously legal).
    */
  def vertexLegal(
      letters: Vector[Option[Int]],
      z: List[Int],
      strict: Boolean = false,
      isolated: Boolean = false
  ): Boolean =
    val spliced = letters.toList.flatten
    if isolated then spliced.isEmpty || UClass.isolatedLegal(letters, z)
    else if strict then spliced.isEmpty || UClass.strictArcLegal(letters, z)
    else if letters.forall(_.isDefined) then spliced.length == z.length && UClass.cyclicSubset(spliced, z)
    else spliced.isEmpty || UClass.cyclicSubset(spliced, z)

  /** The CCW-sorted corners at a vertex CHAIN: each sector's end ray is the next sector's start ray,
    * cyclically — with the 2π sum this makes the corners an exact partition of the neighbourhood (sum alone
    * would permit overlapping sectors), the local-homeomorphism condition of the constructive realization
    * certificate.
    */
  def vertexChained(cs: Vector[Corner], n: Int): Boolean =
    cs.indices.forall(i => (cs(i).startRay + cs(i).angle) % n == cs((i + 1) % cs.length).startRay)

  /** No vertex over-wound (angle sum > 2π) and every checkable vertex word legal. */
  def valid(state: State): Boolean =
    state.cornersByVertex.values.forall: cs =>
      val sum = cs.map(_.angle).sum
      sum < state.n ||
      (sum == state.n && vertexLegal(cs.map(_.letter), state.z, state.strict, state.isolated))

  /** A class-wide move: one split variant, identified congruence-invariantly. `kind` selects the family:
    * "flush" (a = arc length, b = 0) or "vt" — vertex-touch (a = the anchor vertex's index in the canonical
    * word, b = the sliver offset of the q-gon's first edge from the anchor's outgoing edge).
    * `remainderShapes` are the SORTED remainder shape keys.
    */
  final case class Move(
      shape: Vector[Int],
      kind: String,
      q: Int,
      a: Int,
      b: Int,
      remainderShapes: Vector[Vector[Int]]
  )

  private def remainderShapesOf(f: FlushSplit): Vector[Vector[Int]] =
    f.split.remainders.map(p => shapeKey(p.poly)).sortBy(_.mkString(","))

  private def anchoredSplit(t: PlacedTile, f: FlushSplit): Vector[PlacedTile] =
    val vs      = t.vertices
    val remTail = vs((f.edge + f.arcLen) % vs.length) // the remainder walk's start seam
    PlacedTile(f.split.regular, vs(f.edge)) +:
      f.split.remainders.map(p => PlacedTile(p.poly, remTail + p.rel))

  private def anchoredVt(t: PlacedTile, vt: Defusion.VertexTouch): Vector[PlacedTile] =
    val anchor = t.vertices(vt.anchor)
    PlacedTile(Defusion.regularGon(t.poly.n, vt.q, vt.dir0), anchor) +:
      vt.remainders.map(p => PlacedTile(p.poly, anchor + p.rel))

  private val vtCache = scala.collection.concurrent.TrieMap
    .empty[(Vector[Int], Vector[Int]), Vector[Defusion.VertexTouch]]

  private def vtSplits(poly: UnitPolygon, qs: Seq[Int]): Vector[Defusion.VertexTouch] =
    vtCache.getOrElseUpdate(
      (poly.n +: poly.dirs, qs.distinct.sorted.toVector),
      Defusion.vertexTouchSplits(poly, qs)
    )

  /** Candidate moves: every (irregular class × geometric split variant) across BOTH families, enumerated once
    * per congruence class, deterministic order; only q-gons named by z are ever split off. For "vt" moves the
    * (a, b) fields carry the representative's canonical anchor and sliver as a variant DISCRIMINATOR;
    * application re-selects canonically per instance (mirrored instances have mirrored parameters).
    */
  def moves(state: State): Vector[Move] =
    val reps  = state.tiles
      .map(_.poly)
      .filter(regularSizeOf(_).isEmpty)
      .groupBy(shapeKey)
      .toVector
      .sortBy(_._1.mkString(","))
      .map((k, ps) => (k, ps.head))
    val flush =
      for
        (key, p) <- reps
        f        <- Defusion.flushSplits(p, state.z.distinct)
      yield Move(key, "flush", f.q, f.arcLen, 0, remainderShapesOf(f))
    val vt    =
      for
        (key, p) <- reps
        v        <- vtSplits(p, state.z.distinct)
        m         = p.dirs.length
        sh        = canonicalShift(p.dirs)
      yield Move(
        key,
        "vt",
        v.q,
        ((v.anchor - sh)              % m + m)      % m,
        (((v.dir0 - p.dirs(v.anchor)) % p.n) + p.n) % p.n,
        v.remainders.map(pc => shapeKey(pc.poly)).sortBy(_.mkString(","))
      )
    (flush ++ vt).distinct.sortBy(m =>
      (m.shape.mkString(","), m.kind, m.q, m.a, m.b, m.remainderShapes.map(_.mkString(",")).mkString(";"))
    )

  /** The pre-state vertices an instance's split touches: both seams and the arc's interior corners. (The
    * cut's new vertices are always complete and are caught by post-validation.)
    */
  private def affectedVertices(t: PlacedTile, f: FlushSplit): Vector[VKey] =
    val vs = t.vertices
    (0 to f.arcLen).map(j => vs((f.edge + j) % vs.length).reducedKey).toVector

  private def canonicalShift(dirs: Vector[Int]): Int =
    import scala.math.Ordering.Implicits.seqOrdering
    dirs.indices.minBy(s => dirs.drop(s) ++ dirs.take(s))

  /** Apply `mv` to EVERY tile of its class, each instance splitting at the arc that is CANONICAL IN ITS WORD
    * (least index after rotating the word to its canonical position) — lattice translates share the word, so
    * the choice is translation-EQUIVARIANT and the resulting state stays periodic; then validate the whole
    * state. Admissible only with a WITNESS: some instance whose canonical arc is fully checkable — a
    * vacuously-legal move on boundary-blind seams is rejected, not sampled (the radius-6 P2 lesson).
    */
  def applyMove(state: State, mv: Move): Option[State] =
    val interior  = state.interiorWords.keySet
    var witnessed = false
    val next      = state.tiles.flatMap: t =>
      if regularSizeOf(t.poly).isDefined || shapeKey(t.poly) != mv.shape then Vector(t)
      else
        val m     = t.poly.dirs.length
        val shift = canonicalShift(t.poly.dirs)
        mv.kind match
          case "flush" =>
            val chosen = Defusion
              .flushSplits(t.poly, List(mv.q))
              .filter(f => f.arcLen == mv.a && remainderShapesOf(f) == mv.remainderShapes)
              .minByOption(f => ((f.edge - shift) % m + m) % m)
              .getOrElse(throw IllegalStateException("congruent instance without the split"))
            if affectedVertices(t, chosen).forall(interior) then witnessed = true
            anchoredSplit(t, chosen)
          case _       =>
            val chosen = vtSplits(t.poly, state.z.distinct)
              .filter(v =>
                v.q == mv.q &&
                  v.remainders.map(pc => shapeKey(pc.poly)).sortBy(_.mkString(",")) == mv.remainderShapes
              )
              .minByOption(v => (((v.anchor - shift) % m + m) % m, v.dir0))
              .getOrElse(throw IllegalStateException("congruent instance without the vt split"))
            val vs     = t.vertices
            if chosen.contacts.forall(ci => interior(vs(ci).reducedKey)) then witnessed = true
            anchoredVt(t, chosen)
    Option.when(witnessed)(state.copy(tiles = next)).filter(valid)

  /** Moves surviving class-wide application + full validation. */
  def admissible(state: State): Vector[Move] = moves(state).filter(m => applyMove(state, m).isDefined)

  /** Greedy exhaustion to a saturated endpoint: repeatedly apply the admissible move `pick` selects (the
    * strategy — endpoints from different strategies probe confluence). Terminates by area descent; `maxMoves`
    * is a hard backstop.
    */
  def exhaust(
      state: State,
      maxMoves: Int = 64,
      pick: Vector[Move] => Move = _.head
  ): (State, List[Move]) =
    @annotation.tailrec
    def go(s: State, done: List[Move], left: Int): (State, List[Move]) =
      if left == 0 then (s, done.reverse)
      else
        val adm = admissible(s)
        if adm.isEmpty then (s, done.reverse)
        else
          val mv = pick(adm)
          go(applyMove(s, mv).get, mv :: done, left - 1)
    go(state, Nil, maxMoves)

  /** Seed a state from an exact development. */
  def seed(
      patch: ExactDeveloper.ExactPatch,
      z: List[Int],
      strict: Boolean = false,
      isolated: Boolean = false
  ): State =
    State(patch.n, z, patch.faces.map(f => PlacedTile(f.poly, f.anchor)), strict, isolated)
