package io.github.scala_tessella.research_core

import SpeciesEnumerator.{species, Species, State}

/** The star-gluing atlas and the mono-species shell filter.
  *
  * A vertex-transitive (k = 1) honeycomb has all vertex stars congruent, so its species is one of the 34 and
  * every neighbor of a vertex carries the SAME species. This module decides, per species S, whether the full
  * first shell of a placed S-star can be equipped with S-stars at all neighbors consistently — a NECESSARY
  * condition for S to be the species of any mono-species honeycomb (uniform or not). The filter is sound: it
  * only kills species that cannot appear as the species of a uniform honeycomb.
  *
  * Machinery. A GLUING at tiling-vertex x of the placed star (honeycomb edge e = (0, uₓ)) is a rigid motion g
  * placing a second copy of S at uₓ whose ring around e coincides with the star's ring at x, as placed cells:
  * g(0) = uₓ, some tiling-vertex y of S maps onto the back-direction −uₓ, and the ring cells match. Since a
  * convex cell is determined by an edge, its two face germs there, and its type, ring agreement is decided by
  * CELL DESCRIPTORS at e — (type, the two face planes with their interior directions) — where a face F of the
  * star spanning tiling-vertices x, w has plane span(uₓ, u_w) through the vertex and interior direction the
  * component of u_w ⊥ uₓ (constant along e, so both ends compute the same germ). Candidate motions are built
  * from frame alignments over all (y, arc, arc′, ±det) and kept iff the descriptor multisets agree —
  * geometric verification is the filter, so construction needs no case analysis.
  *
  * Shell consistency. For neighbors uₓ, uₓ′: cells shared by their stars all contain the segment between the
  * closer pair of the three vertices involved, and the binding constraints are: (i) uₓ′ is a tiling vertex of
  * the star at uₓ iff uₓ is one of the star at uₓ′ (edges must be mutual — in particular around every
  * triangular face, where the neighbors are themselves adjacent); (ii) when the edge h = (uₓ, uₓ′) exists,
  * both stars induce the same ring around h (descriptor multisets again). The filter searches for a full
  * assignment by backtracking; UNSAT excludes the species from k = 1 (and from all mono-species honeycombs).
  * SAT is a candidate certificate only — realizability and transitivity are [[TransitivePatterns]]'s
  * development. All geometric decisions run on midpoints with 1e-6 tolerances; margins near the threshold are
  * flagged (expect none — true mismatches in this lattice geometry are ≥ 1e-3).
  */
object MonoShell:

  type Vec = (Double, Double, Double)

  private[research_core] def sub(a: Vec, b: Vec): Vec      = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private[research_core] def add(a: Vec, b: Vec): Vec      = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private[research_core] def dot(a: Vec, b: Vec): Double   = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private[research_core] def cross(a: Vec, b: Vec): Vec    =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)
  private[research_core] def scale(a: Vec, s: Double): Vec = (a._1 * s, a._2 * s, a._3 * s)
  private[research_core] def norm(a: Vec): Double          = math.sqrt(dot(a, a))
  private[research_core] def unit(a: Vec): Vec             = scale(a, 1.0 / norm(a))
  private[research_core] def dist(a: Vec, b: Vec): Double  = norm(sub(a, b))
  private[research_core] def neg(a: Vec): Vec              = (-a._1, -a._2, -a._3)

  /** Rotation (possibly improper) as an orthonormal frame map: source frame → target frame. */
  final case class Rot(a1: Vec, a2: Vec, a3: Vec, b1: Vec, b2: Vec, b3: Vec):
    def apply(v: Vec): Vec      =
      add(add(scale(b1, dot(a1, v)), scale(b2, dot(a2, v))), scale(b3, dot(a3, v)))
    def sameAs(o: Rot): Boolean =
      val probes = Vector((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0))
      probes.forall(p => dist(apply(p), o(p)) < 1e-9)

  final class Flags:
    val items: collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer.empty
    def add(s: String): Unit                         = items += s

  private[research_core] val matchTol = 1e-6
  private[research_core] val grayTol  = 1e-3

  // ---------- star geometry ----------

  final case class StarGeom(
      st: State,
      u: Vector[Vec],
      rings: Map[Int, Vector[(Int, (Int, Int), (Int, Int))]]
  ):
    def cellOrd(ci: Int): Int = st.corners(ci).cell.ordinal

  def geomOf(sp: Species): StarGeom =
    val u     = sp.state.positions.map(v => unit((v._1.mid, v._2.mid, v._3.mid)))
    val rings = sp.state.positions.indices.map(x => x -> SpeciesCorona.ringAt(sp.state, x)).toMap
    StarGeom(sp.state, u, rings)

  /** The cell descriptors of the ring at tiling-vertex x, transported by `rot` (identity for the star
    * itself): per ring cell, (cell type, its two face germs (normal-as-line, interior direction)).
    */
  private[research_core] def ringDescriptors(
      g: StarGeom,
      x: Int,
      rot: Vec => Vec
  ): Vector[(Int, Vector[(Vec, Vec)])] =
    g.rings(x).map { (ci, aIn, aOut) =>
      val germs = Vector(aIn, aOut).map { arc =>
        val w = arc._1 + arc._2 - x
        val n = unit(cross(g.u(x), g.u(w)))
        val p = unit(sub(g.u(w), scale(g.u(x), dot(g.u(w), g.u(x)))))
        (rot(n), rot(p))
      }
      (g.cellOrd(ci), germs)
    }

  /** Multiset equality of ring descriptors (normals compared as lines, interiors as vectors). */
  private[research_core] def descriptorsMatch(
      a: Vector[(Int, Vector[(Vec, Vec)])],
      b: Vector[(Int, Vector[(Vec, Vec)])],
      flags: Flags
  ): Boolean =
    def germEq(x: (Vec, Vec), y: (Vec, Vec)): Boolean                               =
      val dn = math.min(dist(x._1, y._1), dist(x._1, neg(y._1)))
      val dp = dist(x._2, y._2)
      if (dn > matchTol && dn < grayTol) || (dp > matchTol && dp < grayTol) then
        flags.add(f"gray-zone germ match dn=$dn%.2e dp=$dp%.2e")
      dn < matchTol && dp < matchTol
    def cellEq(x: (Int, Vector[(Vec, Vec)]), y: (Int, Vector[(Vec, Vec)])): Boolean =
      x._1 == y._1 && {
        val Vector(g1, g2) = x._2
        val Vector(h1, h2) = y._2
        (germEq(g1, h1) && germEq(g2, h2)) || (germEq(g1, h2) && germEq(g2, h1))
      }
    a.size == b.size && {
      val used = Array.fill(b.size)(false)
      a.forall { xa =>
        val j = b.indices.find(j => !used(j) && cellEq(xa, b(j)))
        j.foreach(used(_) = true)
        j.isDefined
      }
    }

  // ---------- gluings ----------

  /** A verified gluing at tiling-vertex x: the star copy at uₓ via rigid motion p ↦ uₓ + rot(p), with S's
    * tiling-vertex y on the back-direction.
    */
  final case class Glu(y: Int, rot: Rot):
    def vertexPos(g: StarGeom, x: Int, z: Int): Vec = add(g.u(x), rot(g.u(z)))

  /** All verified gluings at x: frame-aligned candidates kept iff the transported ring descriptors of S at y
    * coincide with the star's ring descriptors at x.
    */
  def gluings(g: StarGeom, x: Int, flags: Flags): Vector[Glu] =
    val descX                        = ringDescriptors(g, x, identity)
    def inPlane(v: Int, w: Int): Vec = unit(sub(g.u(w), scale(g.u(v), dot(g.u(w), g.u(v)))))
    val found                        = collection.mutable.ArrayBuffer.empty[Glu]
    for
      y <- g.u.indices
      if g.rings(y).size == g.rings(x).size
    do
      val bx = neg(g.u(x))
      for
        (_, aInX, _) <- g.rings(x)
        (_, aInY, _) <- g.rings(y)
        det          <- Vector(1.0, -1.0)
      do
        val wx  = aInX._1 + aInX._2 - x
        val wy  = aInY._1 + aInY._2 - y
        val p   = inPlane(x, wx)
        val q   = inPlane(y, wy)
        val rot = Rot(g.u(y), q, scale(cross(g.u(y), q), det), bx, p, cross(bx, p))
        if dist(rot(g.u(y)), bx) < matchTol &&
          descriptorsMatch(ringDescriptors(g, y, rot.apply), descX, flags) &&
          !found.exists(_.rot.sameAs(rot))
        then found += Glu(y, rot)
    found.toVector

  // ---------- shell consistency ----------

  /** The ring descriptors around the edge from uₓ to a shell vertex P, as induced by the star at uₓ with
    * gluing glu — where z is the tiling-vertex of that star pointing at P.
    */
  private def shellRing(g: StarGeom, x: Int, glu: Glu, z: Int, flags: Flags) =
    ringDescriptors(g, z, glu.rot.apply)

  /** Compatibility of the gluings at two neighbors x, x′: the edge between them must be mutual, and if
    * present both stars must induce the same ring around it.
    */
  private[research_core] def compatible(
      g: StarGeom,
      x: Int,
      gx: Glu,
      x2: Int,
      gx2: Glu,
      flags: Flags
  ): Boolean =
    def vertexAt(from: Int, glu: Glu, target: Vec): Option[Int] =
      val ds = g.u.indices.map(z => z -> dist(glu.vertexPos(g, from, z), target))
      ds.filter(t => t._2 > matchTol && t._2 < grayTol)
        .foreach(t => flags.add(f"gray-zone shell vertex match d=${t._2}%.2e"))
      ds.find(_._2 < matchTol).map(_._1)
    val z                                                       = vertexAt(x, gx, g.u(x2))
    val z2                                                      = vertexAt(x2, gx2, g.u(x))
    (z, z2) match
      case (None, None)         => true
      case (Some(za), Some(zb)) =>
        descriptorsMatch(shellRing(g, x, gx, za, flags), shellRing(g, x2, gx2, zb, flags), flags)
      case _                    => false

  /** The shell filter: can every neighbor simultaneously carry an S-star? Returns the per-vertex gluing
    * counts and, if satisfiable, one witness assignment.
    */
  final case class ShellResult(counts: Vector[Int], witness: Option[Vector[Int]]):
    def sat: Boolean = witness.isDefined

  def shell(sp: Species, flags: Flags): ShellResult =
    val g                   = geomOf(sp)
    val domains             = g.u.indices.toVector.map(x => gluings(g, x, flags))
    val n                   = domains.size
    val chosen              = Array.fill(n)(-1)
    def bt(x: Int): Boolean =
      if x == n then true
      else
        domains(x).indices.exists { i =>
          chosen(x) = i
          val ok  = (0 until x).forall(x2 =>
            compatible(g, x, domains(x)(i), x2, domains(x2)(chosen(x2)), flags)
          )
          val res = ok && bt(x + 1)
          if !res then chosen(x) = -1
          res
        }
    val sat                 = bt(0)
    ShellResult(domains.map(_.size), if sat then Some(chosen.toVector) else None)

  /** The shell table: verdict per species (index into [[SpeciesEnumerator.species]]). */
  lazy val results: (Vector[(Int, ShellResult)], Vector[String]) =
    val flags = Flags()
    val rs    = species.indices.toVector.map(i => i -> shell(species(i), flags))
    (rs, flags.items.distinct.toVector)
