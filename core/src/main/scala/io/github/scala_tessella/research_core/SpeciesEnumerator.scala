package io.github.scala_tessella.research_core

import HoneycombAlphabet.*
import HoneycombAlphabet.CellType
import CertifiedDihedrals.{Iv, V3}

/** The species table — ALL edge-to-edge tilings of the sphere of directions by the rigid
  * corner figures of the 13 core cells. A vertex species (the star of a honeycomb vertex up to congruence,
  * reflections included) IS such a tiling: cells become spherical polygons with arc sides of length equal to
  * the face interior angles (60/90/120/135/150), shared faces become shared arcs, and honeycomb edges become
  * tiling vertices whose surrounding angles — exact dihedrals in the (15°, α) lattice — must close to exactly
  * 360°, i.e. be one of the edge figures catalogued by [[HoneycombAlphabet]].
  *
  * Assembly is a geometric DFS: one seed corner per cell type (fixed placement; the alphabet is restricted to
  * ordinal ≥ seed so each species is found from its least cell), growth always fills a boundary arc at a
  * most-filled tiling vertex, and placements are interval rotations built from orthonormal frames (both
  * mirror gluings tried). Pruning is exact where it matters: running excess ≤ (720, 0) and the cell multiset
  * dominated by one of the [[SpeciesSupports]] multisets; every vertex angle sum ≤ (360, 0) with each gap
  * either 0 or ≥ 60°; every partial vertex fan a contiguous subsequence of a catalogued edge figure.
  * Acceptance — empty boundary ∧ excess = (720, 0) ∧ all vertices exactly (360, 0) — certifies a genuine
  * degree-1 tiling (a closed complex with flat vertices is a spherical cover; area 720 forces degree 1).
  * Dedup is by the canonical key of the labeled combinatorial map (min BFS code over all starting flags and
  * both orientations). Any geometric decision with an ambiguous interval margin is flagged (expect none).
  */
object SpeciesEnumerator:

  import scala.math.Ordering.Implicits.seqOrdering

  // ---------- interval vector algebra ----------

  private def dot(a: V3, b: V3): Iv                                                  = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def vAdd(a: V3, b: V3): V3                                                 = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private def vSub(a: V3, b: V3): V3                                                 = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def vScale(a: V3, s: Iv): V3                                               = (a._1 * s, a._2 * s, a._3 * s)
  private def vCross(a: V3, b: V3): V3                                               =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)
  private def vNormalize(a: V3): V3                                                  = vScale(a, Iv.point(1.0) / dot(a, a).sqrtIv)
  private def midOf(a: V3): (Double, Double, Double)                                 = (a._1.mid, a._2.mid, a._3.mid)
  private def dist(a: (Double, Double, Double), b: (Double, Double, Double)): Double =
    val (dx, dy, dz) = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
    math.sqrt(dx * dx + dy * dy + dz * dz)

  private def frame(a: V3, b: V3): (V3, V3, V3) =
    val e1 = vNormalize(a)
    val e2 = vNormalize(vSub(b, vScale(e1, dot(e1, b))))
    (e1, e2, vCross(e1, e2))

  /** The rotation taking the source frame to the destination frame, applied to v. */
  private def mapped(src: (V3, V3, V3), dst: (V3, V3, V3))(v: V3): V3 =
    vAdd(vAdd(vScale(dst._1, dot(src._1, v)), vScale(dst._2, dot(src._2, v))), vScale(dst._3, dot(src._3, v)))

  private def reflect(nHat: V3)(v: V3): V3 = vSub(v, vScale(nHat, dot(v, nHat) * Iv.point(2.0)))

  final class Flags:
    val items: collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer.empty
    def add(s: String): Unit                         = items += s

  /** Certified sign of det(a, b, c): +1/−1, or 0 (flagged) if the interval straddles zero. */
  private def orientationSign(a: V3, b: V3, c: V3, flags: Flags, what: String): Int =
    val d = dot(vCross(a, b), c)
    if d.lo > 0 then 1
    else if d.hi < 0 then -1
    else
      flags.add(s"ambiguous orientation sign ($what): $d")
      0

  // ---------- corner data ----------

  /** Canonical corner figures of the 13 core cells: unit neighbor directions, cyclic. */
  val cornerFigures: Map[CellType, Vector[V3]] =
    SpeciesSupports.cornerConfigs.map((c, ps) => c -> CertifiedDihedrals.cornerFigure(ps).map(vNormalize))

  private val cfgOf: Map[CellType, Vector[Int]] =
    SpeciesSupports.cornerConfigs.view.mapValues(_.toVector).toMap

  /** Exact dihedral of a cell corner between consecutive faces (p, q). */
  private val dihedral: Map[(CellType, Int, Int), CoreAngle] =
    edgeTypes.flatMap { et =>
      List((et.cell, et.faces._1, et.faces._2) -> et.angle, (et.cell, et.faces._2, et.faces._1) -> et.angle)
    }.toMap

  // ---------- the 69-figure prefix filter ----------

  private val catalogueKeySet: Set[CanonKey] = catalogue.map(_.key).toSet

  /** Every contiguous subsequence (both directions) of every catalogued edge figure. */
  private val figureChains: Set[Vector[(Int, Int, Int)]] =
    val all =
      for
        fig   <- catalogue
        fwd    = fig.tokens.map(o => (o.et.cell.ordinal, o.left, o.right))
        base  <- Vector(fwd, fwd.reverse.map((c, l, r) => (c, r, l)))
        start <- base.indices
        len   <- 1 to base.size
      yield (0 until len).toVector.map(j => base((start + j) % base.size))
    all.toSet

  private def canonicalCycle(ts: Vector[(Int, Int, Int)]): CanonKey =
    val bwd = ts.reverse.map((c, l, r) => (c, r, l))
    (for
      base <- Vector(ts, bwd)
      k    <- ts.indices
    yield base.drop(k) ++ base.take(k)).min

  // ---------- assembly state ----------

  final case class Placed(cell: CellType, verts: Vector[V3], vids: Vector[Int])

  /** An arc of the spherical complex: the face size glued there and its owning (corner, side) pairs. */
  final case class Arc(face: Int, owners: List[(Int, Int)])

  private type ArcKey = (Int, Int)
  private def arcKey(a: Int, b: Int): ArcKey = if a < b then (a, b) else (b, a)

  final case class State(
      corners: Vector[Placed],
      positions: Vector[V3],
      posMid: Vector[(Double, Double, Double)],
      arcs: Map[ArcKey, Arc],
      sums: Map[Int, CoreAngle],
      excess: CoreAngle,
      counts: Map[CellType, Int]
  )

  private val full360  = CoreAngle(360, 0)
  private val full720  = CoreAngle(720, 0)
  private val matchTol = 1e-6 // vertex identification (true separations are 0 or ≫ 1e-3)
  private val grayTol  = 1e-3 // flag zone above matchTol
  private val angTol   = 1e-4 // geometric angle comparisons, degrees (true gaps are 0 or ≥ 60)

  // ---------- vertex fans ----------

  /** A corner's angular wedge at a tiling vertex, with its two bounding arcs ordered counterclockwise. */
  final private case class Wedge(
      corner: Int,
      cellOrd: Int,
      startFace: Int,
      endFace: Int,
      startKey: ArcKey,
      endKey: ArcKey,
      startDeg: Double,
      extentDeg: Double
  ):
    def token: (Int, Int, Int) = (cellOrd, startFace, endFace)

  /** The wedges around vertex `vid` sorted counterclockwise, with the gap following each (≈0 = adjacent).
    * Returns None (flagged) on geometric inconsistency.
    */
  private def fanAt(st: State, vid: Int, flags: Flags): Option[(Vector[Wedge], Vector[Double])] =
    val pm                    = st.posMid(vid)
    val refAxis               = if math.abs(pm._3) < 0.9 then (0.0, 0.0, 1.0) else (1.0, 0.0, 0.0)
    val f1                    =
      val c = (
        refAxis._2 * pm._3 - refAxis._3 * pm._2,
        refAxis._3 * pm._1 - refAxis._1 * pm._3,
        refAxis._1 * pm._2 - refAxis._2 * pm._1
      )
      val n = math.sqrt(c._1 * c._1 + c._2 * c._2 + c._3 * c._3)
      (c._1 / n, c._2 / n, c._3 / n)
    val f2                    = (pm._2 * f1._3 - pm._3 * f1._2, pm._3 * f1._1 - pm._1 * f1._3, pm._1 * f1._2 - pm._2 * f1._1)
    def theta(w: Int): Double =
      val q = st.posMid(w)
      val t = (
        q._1 - pm._1 * (pm._1 * q._1 + pm._2 * q._2 + pm._3 * q._3),
        q._2 - pm._2 * (pm._1 * q._1 + pm._2 * q._2 + pm._3 * q._3),
        q._3 - pm._3 * (pm._1 * q._1 + pm._2 * q._2 + pm._3 * q._3)
      )
      val x = t._1 * f1._1 + t._2 * f1._2 + t._3 * f1._3
      val y = t._1 * f2._1 + t._2 * f2._2 + t._3 * f2._3
      val a = math.toDegrees(math.atan2(y, x))
      if a < 0 then a + 360 else a
    val raw                   =
      for
        (c, ci) <- st.corners.zipWithIndex
        i       <- c.vids.indices
        if c.vids(i) == vid
      yield
        val k        = c.vids.size
        val cfg      = cfgOf(c.cell)
        val prevV    = c.vids((i + k - 1) % k)
        val nextV    = c.vids((i + 1) % k)
        val prevFace = cfg((i + k - 1) % k)
        val nextFace = cfg(i)
        val ang      = dihedral(c.cell, prevFace, nextFace).degrees
        val thPrev   = theta(prevV)
        val thNext   = theta(nextV)
        val fwd      = ((thNext - thPrev) % 360 + 360) % 360
        val bwd      = ((thPrev - thNext) % 360 + 360) % 360
        if math.abs(fwd - ang) < angTol then
          Some(Wedge(
            ci,
            c.cell.ordinal,
            prevFace,
            nextFace,
            arcKey(vid, prevV),
            arcKey(vid, nextV),
            thPrev,
            ang
          ))
        else if math.abs(bwd - ang) < angTol then
          Some(Wedge(
            ci,
            c.cell.ordinal,
            nextFace,
            prevFace,
            arcKey(vid, nextV),
            arcKey(vid, prevV),
            thNext,
            ang
          ))
        else
          flags.add(
            f"wedge extent mismatch at vid=$vid cell=${c.cell.label}: fwd=$fwd%.6f bwd=$bwd%.6f ang=$ang%.6f"
          )
          None
    if raw.exists(_.isEmpty) then None
    else
      val ws   = raw.flatten.toVector.sortBy(_.startDeg)
      val n    = ws.size
      val gaps = (0 until n).toVector.map { j =>
        val cur  = ws(j)
        val next = if j == n - 1 then ws.head.startDeg + 360 else ws(j + 1).startDeg
        next - (cur.startDeg + cur.extentDeg)
      }
      Some((ws, gaps))

  /** All local checks at vertex `vid`: exact sum ≤ 360, geometric gaps 0 or ≥ 60 with 0-gaps zipped, every
    * maximal chain a catalogued-figure subsequence, and exact closure ⇒ a catalogued figure.
    */
  private def vertexOk(st: State, vid: Int, flags: Flags): Boolean =
    val sum = st.sums.getOrElse(vid, CoreAngle.zero)
    if sum.degrees > 360.0 + angTol then false
    else
      fanAt(st, vid, flags) match
        case None             => false
        case Some((ws, gaps)) =>
          val n      = ws.size
          var ok     = true
          var nGaps  = 0
          val linked = Array.fill(n)(false) // wedge j immediately followed by wedge (j+1) % n
          for j <- 0 until n if ok do
            val gap = gaps(j)
            // margin tripwire: true gaps are 0 or exact lattice values (≥ 60, pairwise ≥ ~0.79° apart)
            if (math.abs(gap) > angTol && math.abs(gap) < 0.5) ||
              (math.abs(gap - 60.0) > angTol && math.abs(gap - 60.0) < 0.5)
            then flags.add(f"suspicious gap $gap%.6f at vid=$vid — decided by a thin margin")
            if gap < -angTol then ok = false // overlapping wedges
            else if gap <= angTol then
              if n > 1 && ws(j).endKey == ws((j + 1) % n).startKey then linked(j) = true
              else ok = false                // coincident directions without a shared arc: overlap
            else
              nGaps += 1
              if gap < 60.0 - angTol then ok = false // unfillable gap
          if !ok then false
          else if nGaps == 0 then
            // combinatorially closed fan: must be exactly flat and a catalogued edge figure
            sum == full360 && catalogueKeySet.contains(canonicalCycle(ws.map(_.token)))
          else if sum == full360 then false // exact closure but geometric gaps: immersed overlap
          else if sum.degrees + 60.0 * nGaps > 360.0 + angTol then false
          else
            // maximal chains = runs between gaps; each must extend to a catalogued figure
            val starts = (0 until n).filter(j => !linked((j + n - 1) % n))
            starts.forall { s =>
              var chain = Vector(ws(s).token)
              var j     = s
              while linked(j) do
                j = (j + 1) % n
                chain :+= ws(j).token
              figureChains.contains(chain)
            }

  // ---------- placement ----------

  private def round7(t: (Double, Double, Double)): (Long, Long, Long) =
    (math.round(t._1 * 1e7), math.round(t._2 * 1e7), math.round(t._3 * 1e7))

  /** All corners gluable to the boundary arc `key` on its free side: (cell, placed vertices), geometric
    * duplicates (corner self-symmetries) removed.
    */
  private def candidatePlacements(
      st: State,
      key: ArcKey,
      allowed: Set[CellType],
      flags: Flags
  ): Vector[(CellType, Vector[V3])] =
    val arc      = st.arcs(key)
    val a        = st.positions(key._1)
    val b        = st.positions(key._2)
    val (oc, os) = arc.owners.head
    val owner    = st.corners(oc)
    val occupied = orientationSign(a, b, owner.verts((os + 2) % owner.verts.size), flags, "occupied side")
    if occupied == 0 then Vector.empty
    else
      val free = -occupied
      val nHat = vNormalize(vCross(a, b))
      val dst  = frame(a, b)
      val out  = Vector.newBuilder[(CellType, Vector[V3])]
      val seen = collection.mutable.Set.empty[(Int, Vector[((Long, Long, Long), (Long, Long, Long), Int)])]
      for
        cell <- CellType.values
        if allowed(cell)
        cfg   = cfgOf(cell)
        s    <- cfg.indices
        if cfg(s) == arc.face
        swap <- List(false, true)
      do
        val fig      = cornerFigures(cell)
        val k        = fig.size
        val (sa, sb) = if swap then (fig((s + 1) % k), fig(s)) else (fig(s), fig((s + 1) % k))
        val rotated  = fig.map(mapped(frame(sa, sb), dst))
        val side     = orientationSign(a, b, rotated((s + 2) % k), flags, s"placement side ${cell.label}")
        if side != 0 then
          val placed = (if side == free then rotated else rotated.map(reflect(nHat))).map(vNormalize)
          val fp     = (
            cell.ordinal,
            placed.indices.toVector.map { i =>
              val e1       = round7(midOf(placed(i)))
              val e2       = round7(midOf(placed((i + 1) % k)))
              val (lo, hi) = if Ordering[(Long, Long, Long)].lteq(e1, e2) then (e1, e2) else (e2, e1)
              (lo, hi, cfg(i))
            }.sorted
          )
          if !seen(fp) then
            seen += fp
            out += cell -> placed
      out.result()

  /** Place a corner into the state: identify its vertices against the complex (flagging gray-zone matches),
    * zip coinciding arcs (face sizes must agree, at most two owners), update the exact vertex sums and the
    * running excess, and run every local check. None = pruned.
    */
  private def tryPlace(
      st: State,
      cell: CellType,
      verts: Vector[V3],
      supports: Vector[Map[CellType, Int]],
      flags: Flags
  ): Option[State] =
    val counts = st.counts.updatedWith(cell)(m => Some(m.getOrElse(0) + 1))
    val excess = st.excess + SpeciesSupports.cornerExcess(cell)
    if !supports.exists(sup => counts.forall((c, m) => sup.getOrElse(c, 0) >= m)) then None
    else if excess.degrees > 720.0 + angTol then None
    else
      var positions = st.positions
      var posMid    = st.posMid
      val vids      = verts.map { w =>
        val wm    = midOf(w)
        var best  = -1
        var bestD = Double.MaxValue
        for i <- posMid.indices do
          val d = dist(posMid(i), wm)
          if d < bestD then
            bestD = d
            best = i
        if bestD < matchTol then best
        else
          if bestD < grayTol then flags.add(f"gray-zone vertex match d=$bestD%.2e (cell ${cell.label})")
          positions :+= w
          posMid :+= wm
          positions.size - 1
      }
      if vids.distinct.size != vids.size then None
      else
        val k         = vids.size
        val cfg       = cfgOf(cell)
        val cornerIdx = st.corners.size
        var arcs      = st.arcs
        var ok        = true
        for i <- 0 until k if ok do
          val ak = arcKey(vids(i), vids((i + 1) % k))
          arcs.get(ak) match
            case Some(Arc(f, owners)) =>
              if f != cfg(i) || owners.size >= 2 then ok = false
              else arcs = arcs.updated(ak, Arc(f, (cornerIdx, i) :: owners))
            case None                 =>
              arcs = arcs.updated(ak, Arc(cfg(i), List((cornerIdx, i))))
        if !ok then None
        else
          var sums = st.sums
          for i <- 0 until k do
            val angle = dihedral(cell, cfg((i + k - 1) % k), cfg(i))
            sums = sums.updatedWith(vids(i))(s => Some(s.getOrElse(CoreAngle.zero) + angle))
          val next =
            State(st.corners :+ Placed(cell, verts, vids), positions, posMid, arcs, sums, excess, counts)
          if vids.forall(v => vertexOk(next, v, flags)) then Some(next) else None

  // ---------- canonical key of the labeled combinatorial map ----------

  private def canonicalMapKey(st: State, flags: Flags): Vector[Int] =
    val nC                                                        = st.corners.size
    val sideArc                                                   =
      st.corners.map(c => c.vids.indices.toVector.map(i => arcKey(c.vids(i), c.vids((i + 1) % c.vids.size))))
    val orient                                                    =
      st.corners.map(c => orientationSign(c.verts(0), c.verts(1), c.verts(2), flags, "corner orientation"))
    def encode(start: Int, startSide: Int, eps: Int): Vector[Int] =
      val idx   = Array.fill(nC)(-1)
      val entry = Array.fill(nC)(0)
      val order = collection.mutable.ArrayBuffer(start)
      idx(start) = 0
      entry(start) = startSide
      val out   = collection.mutable.ArrayBuffer.empty[Int]
      var qi    = 0
      var nid   = 1
      while qi < order.size do
        val c   = order(qi)
        qi += 1
        val ks  = sideArc(c).size
        val dir = if orient(c) == eps then 1 else -1
        out += st.corners(c).cell.ordinal
        for j <- 0 until ks do
          val s            = ((entry(c) + dir * j) % ks + ks) % ks
          val ak           = sideArc(c)(s)
          val (nb, nbSide) = st.arcs(ak).owners.find(_ != (c, s)).get
          out += st.arcs(ak).face
          if idx(nb) < 0 then
            idx(nb) = nid
            nid += 1
            entry(nb) = nbSide
            order += nb
          out += idx(nb)
      out.toVector
    (for
      c0  <- 0 until nC
      s0  <- sideArc(c0).indices
      eps <- List(1, -1)
    yield encode(c0, s0, eps)).min

  // ---------- species records ----------

  /** A realized vertex species: its cell multiset, canonical map key, and vertex-figure census. `vertices` is
    * the number of tiling vertices = honeycomb edges at the vertex; `faces` the number of arcs = faces.
    */
  final case class Species(
      counts: Map[CellType, Int],
      mapKey: Vector[Int],
      figures: Vector[(CanonKey, Int)],
      vertices: Int,
      faces: Int,
      state: State
  ):
    def cells: Int          = counts.values.sum
    def showSupport: String =
      counts.toList.sortBy(_._1.ordinal).map((c, m) => s"${c.label}:$m").mkString("{", " ", "}")

  private def speciesOf(st: State, flags: Flags): Species =
    val figKeys = st.positions.indices.toVector.map { vid =>
      val (ws, _) = fanAt(st, vid, flags).get
      val key     = canonicalCycle(ws.map(_.token))
      if !catalogueKeySet.contains(key) then
        flags.add(s"accepted species with uncatalogued figure at vid=$vid")
      key
    }
    Species(
      st.counts,
      canonicalMapKey(st, flags),
      figKeys.groupMapReduce(identity)(_ => 1)(_ + _).toVector.sortBy(_._1),
      st.positions.size,
      st.arcs.size,
      st
    )

  // ---------- the DFS ----------

  private def seedState(cell: CellType): State =
    val verts = cornerFigures(cell)
    val k     = verts.size
    val cfg   = cfgOf(cell)
    val vids  = (0 until k).toVector
    State(
      Vector(Placed(cell, verts, vids)),
      verts,
      verts.map(midOf),
      (0 until k).map(i => arcKey(i, (i + 1) % k) -> Arc(cfg(i), List((0, i)))).toMap,
      (0 until k).map(i => i -> dihedral(cell, cfg((i + k - 1) % k), cfg(i))).toMap,
      SpeciesSupports.cornerExcess(cell),
      Map(cell -> 1)
    )

  private def expand(
      st: State,
      allowed: Set[CellType],
      supports: Vector[Map[CellType, Int]],
      flags: Flags,
      out: collection.mutable.LinkedHashMap[Vector[Int], Species]
  ): Unit =
    val boundary = st.arcs.toVector.filter(_._2.owners.size == 1).map(_._1)
    if boundary.isEmpty then
      if st.excess == full720 && st.sums.values.forall(_ == full360) then
        val sp = speciesOf(st, flags)
        out.getOrElseUpdate(sp.mapKey, sp): Unit
      else flags.add(s"closed complex failed acceptance (excess=${st.excess}) — assembly bug")
    else
      def sumDeg(v: Int) = st.sums.getOrElse(v, CoreAngle.zero).degrees
      val target         = boundary.minBy(k => (-math.max(sumDeg(k._1), sumDeg(k._2)), k._1, k._2))
      for (cell, verts) <- candidatePlacements(st, target, allowed, flags) do
        tryPlace(st, cell, verts, supports, flags) match
          case Some(next) => expand(next, allowed, supports, flags, out)
          case None       => ()

  /** The complete species enumeration (all seeds, deduplicated) and the ambiguity flags (expect none). */
  lazy val enumerated: (Vector[Species], Vector[String]) =
    val flags = Flags()
    val out   = collection.mutable.LinkedHashMap.empty[Vector[Int], Species]
    for seed <- CellType.values do
      val allowed  = CellType.values.filter(_.ordinal >= seed.ordinal).toSet
      val supports = SpeciesSupports.supports
        .map(_.counts)
        .filter(sup => sup.getOrElse(seed, 0) >= 1 && sup.keys.forall(allowed))
      if supports.nonEmpty then expand(seedState(seed), allowed, supports, flags, out)
    (
      out.values.toVector.sortBy(s => (s.cells, s.showSupport, s.mapKey)),
      flags.items.distinct.toVector
    )

  def species: Vector[Species] = enumerated._1

  /** The species table: every [[SpeciesSupports]] support with its realized species (empty = unrealized). */
  def table: Vector[(SpeciesSupports.Support, Vector[Species])] =
    val byCounts = species.groupBy(_.counts)
    SpeciesSupports.supports.map(sup => sup -> byCounts.getOrElse(sup.counts, Vector.empty))
