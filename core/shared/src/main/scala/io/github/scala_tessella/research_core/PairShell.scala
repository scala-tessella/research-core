package io.github.scala_tessella.research_core

import SpeciesEnumerator.species
import MonoShell.{
  add, cross, descriptorsMatch, dist, geomOf, matchTol, neg, ringDescriptors, scale, sub,
  unit, dot, Flags, Glu, Rot, StarGeom
}

/** The FAIR PAIRS — the k = 2 Krötenheerdt substrate. A 2-uniform Krötenheerdt honeycomb carries two orbits
  * with distinct species S, T; connectivity forces an S–T edge, and since all S-vertices are one orbit, THE
  * decorated S-shell (neighbor species + gluings, up to symmetry) contains at least one T-star — and
  * symmetrically. Two necessary layers, both machine-checked here:
  *
  *   1. SUBSTRATE (the shared-figure graph): an S–T edge shows one edge figure to both ends, so S and T share
  *      a figure — the candidate pairs are the edges of the certified shared-figure adjacency graph (the 9
  *      unique-star species have no non-self neighbor and drop out for free).
  *   2. THE MIXED SHELL (the `MonoShell` single-species shell generalized to two species): around a seed
  *      S-star, every tiling-vertex must carry an S- or T-star — placements from the CROSS-GLUING atlas
  *      (frame-aligned candidates verified by transported ring descriptors, exactly as `MonoShell` but with
  *      the placed star's own geometry) — pairwise-compatible (mutual edges, agreeing rings), with at least
  *      one T among the neighbors. Both directions must be satisfiable: a mixed S-shell and a mixed T-shell.
  *
  * The output — the fair pairs — is the assembly substrate for `PairPatterns` (pair patterns + rigid
  * development). Anchors: the pairs of every KNOWN k = 2 Krötenheerdt honeycomb must survive (Barlow octet
  * pair, the four mixed slab pairs, the prismatic-lift pairs of the 2D 2-uniform tilings).
  */
object PairShell:

  /** A placement at a tiling-vertex of the seed star: a star of species `sp` glued via `glu`. */
  final case class Pl(sp: Int, glu: Glu)

  private val geomCache      = collection.mutable.Map.empty[Int, StarGeom]
  def geom(i: Int): StarGeom = geomCache.getOrElseUpdate(i, geomOf(species(i)))

  /** All verified gluings of a `gB`-star at tiling-vertex x of the seed geometry `gA`: frame-aligned
    * candidates kept iff the transported ring descriptors of B at y coincide with the seed's ring at x. With
    * gB = gA this is exactly `MonoShell`'s same-species gluing atlas (asserted in the spec).
    */
  def crossGluings(gA: StarGeom, x: Int, gB: StarGeom, flags: Flags): Vector[Glu] =
    val descX                                               = ringDescriptors(gA, x, identity)
    def inPlane(g: StarGeom)(v: Int, w: Int): MonoShell.Vec =
      unit(sub(g.u(w), scale(g.u(v), dot(g.u(w), g.u(v)))))
    val found                                               = collection.mutable.ArrayBuffer.empty[Glu]
    for
      y <- gB.u.indices
      if gB.rings(y).size == gA.rings(x).size
    do
      val bx = neg(gA.u(x))
      for
        (_, aInX, _) <- gA.rings(x)
        (_, aInY, _) <- gB.rings(y)
        det          <- Vector(1.0, -1.0)
      do
        val wx  = aInX._1 + aInX._2 - x
        val wy  = aInY._1 + aInY._2 - y
        val p   = inPlane(gA)(x, wx)
        val q   = inPlane(gB)(y, wy)
        val rot = Rot(gB.u(y), q, scale(cross(gB.u(y), q), det), bx, p, cross(bx, p))
        if dist(rot(gB.u(y)), bx) < matchTol &&
          descriptorsMatch(ringDescriptors(gB, y, rot.apply), descX, flags) &&
          !found.exists(_.rot.sameAs(rot))
        then found += Glu(y, rot)
    found.toVector

  private val crossCache                                                        = collection.mutable.Map.empty[(Int, Int, Int), Vector[Glu]]
  private def crossAt(seed: Int, x: Int, other: Int, flags: Flags): Vector[Glu] =
    crossCache.getOrElseUpdate((seed, x, other), crossGluings(geom(seed), x, geom(other), flags))

  /** Position in space of the placed star's tiling-vertex z (placed at seed vertex x via glu). */
  private def shellVertexPos(gSeed: StarGeom, x: Int, gB: StarGeom, glu: Glu, z: Int): MonoShell.Vec =
    add(gSeed.u(x), glu.rot(gB.u(z)))

  /** Two-geometry compatibility: the edge between the shell stars at x and x2 (if any) must be mutual and
    * carry the same induced ring from both sides — `MonoShell`'s `compatible` with per-placement geometries.
    */
  private[research_core] def compatibleP(
      gSeed: StarGeom,
      x: Int,
      px: Pl,
      x2: Int,
      px2: Pl,
      flags: Flags
  ): Boolean =
    val gx                                                                              = geom(px.sp)
    val gx2                                                                             = geom(px2.sp)
    def vertexAt(gB: StarGeom, from: Int, glu: Glu, target: MonoShell.Vec): Option[Int] =
      val ds = gB.u.indices.map(z => z -> dist(shellVertexPos(gSeed, from, gB, glu, z), target))
      ds.filter(t => t._2 > matchTol && t._2 < MonoShell.grayTol)
        .foreach(t => flags.add(f"gray-zone pair-shell vertex match d=${t._2}%.2e"))
      ds.find(_._2 < matchTol).map(_._1)
    val z                                                                               = vertexAt(gx, x, px.glu, gSeed.u(x2))
    val z2                                                                              = vertexAt(gx2, x2, px2.glu, gSeed.u(x))
    (z, z2) match
      case (None, None)         => true
      case (Some(za), Some(zb)) =>
        descriptorsMatch(
          ringDescriptors(gx, za, px.glu.rot.apply),
          ringDescriptors(gx2, zb, px2.glu.rot.apply),
          flags
        )
      case _                    => false

  /** The mixed-shell test for ordered (seed, other): does a consistent full shell around a seed-star exist
    * with every neighbor an S- or T-star and AT LEAST ONE T? Returns the witness species assignment.
    */
  final case class MixedResult(
      seed: Int,
      other: Int,
      domainSizes: Vector[(Int, Int)], // per tiling-vertex: (same-species gluings, cross gluings)
      witness: Option[Vector[Pl]]
  ):
    def sat: Boolean = witness.isDefined

  def mixedShell(seed: Int, other: Int, flags: Flags): MixedResult =
    val gS                                   = geom(seed)
    val n                                    = gS.u.size
    val domains                              = (0 until n).toVector.map { x =>
      crossAt(seed, x, seed, flags).map(Pl(seed, _)) ++ crossAt(seed, x, other, flags).map(Pl(other, _))
    }
    val sizes                                = (0 until n).toVector.map { x =>
      val d = domains(x)
      (d.count(_.sp == seed), d.count(_.sp == other))
    }
    // visit smallest domains first; the forced cross position is tried at every slot in turn
    val order                                = (0 until n).sortBy(domains(_).size).toVector
    val chosen                               = Array.fill[Option[Pl]](n)(None)
    def bt(k: Int, forcedSlot: Int): Boolean =
      if k == n then true
      else
        val x  = order(k)
        val ds = if k == forcedSlot then domains(x).filter(_.sp == other) else domains(x)
        ds.exists { pl =>
          chosen(x) = Some(pl)
          val ok  = (0 until k).forall { k2 =>
            val x2 = order(k2)
            compatibleP(gS, x, pl, x2, chosen(x2).get, flags)
          }
          val res = ok && bt(k + 1, forcedSlot)
          if !res then chosen(x) = None
          res
        }
    val sat                                  = (0 until n).exists { slot =>
      for i <- chosen.indices do chosen(i) = None
      bt(0, slot)
    }
    MixedResult(seed, other, sizes, if sat then Some(chosen.toVector.map(_.get)) else None)

  /** Candidate pairs: edges of the certified shared-figure adjacency graph (unordered, no self-pairs). */
  lazy val candidatePairs: Vector[(Int, Int)] =
    val adj = SpeciesCorona.analysis.adjacency
    (for
      i <- species.indices
      j <- adj.getOrElse(i, Vector.empty)
      if j > i
    yield (i, j)).toVector.sorted

  final case class Verdict(i: Int, j: Int, iSide: MixedResult, jSide: MixedResult):
    def fair: Boolean = iSide.sat && jSide.sat

  /** The fair-pair table: mixed-shell verdict per candidate pair, plus the gray-zone flags (expect none). */
  lazy val results: (Vector[Verdict], Vector[String]) =
    val flags = Flags()
    val vs    = candidatePairs.map((i, j) => Verdict(i, j, mixedShell(i, j, flags), mixedShell(j, i, flags)))
    (vs, flags.items.distinct.toVector)

  /** The fair pairs: the assembly substrate for `PairPatterns`. */
  lazy val fairPairs: Vector[(Int, Int)] = results._1.filter(_.fair).map(v => (v.i, v.j))
