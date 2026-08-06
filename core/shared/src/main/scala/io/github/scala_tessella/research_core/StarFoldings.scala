package io.github.scala_tessella.research_core

import MonoShell.geomOf
import SpeciesEnumerator.species
import StarChambers.{Chamber, StarComplex}
import TransitivePatterns.{cornerData, stabilizers}

/** FOLDINGS of the species star complexes — the three-dimensional transposition of the two-dimensional
  * star-folding layer. A vertex orbit of a k-uniform honeycomb may carry a nontrivial stabilizer in the
  * symmetry group; the minimal symbol's chambers at that orbit are the star's flags QUOTIENTED by that
  * stabilizer. Since a chamber spans a full 3-simplex of barycenters, an isometry fixing a flag is the
  * identity — every subgroup of the star's symmetry group acts FREELY on the flags, so every quotient is an
  * honest chamber complex (σ's descend because symmetries are automorphisms of the flag complex; in a
  * quotient the σ's may acquire fixed points, the D-symbol mirrors).
  *
  * Deliverables: the symmetry group AS CHAMBER PERMUTATIONS (from the orthogonal stabilizer matrices of
  * `TransitivePatterns.stabilizers`, via the induced tiling-vertex permutation and (cell type, vid-set)
  * corner matching), the FULL SUBGROUP LATTICE (closure-BFS: every subgroup is reached by adding its
  * generators one at a time), and the FOLDED COMPLEXES with the m₀₁ (face size) and m₂₃ (ring size) data that
  * σ₀-assembly consumes. Classical pins: the full folding of the cubic star is the 1-chamber regular {4,3,4}
  * symbol; of the octet-c star the 2-chamber quasiregular tet-oct symbol.
  */
object StarFoldings:

  type Perm = Vector[Int]

  final case class StarSymmetry(cx: StarComplex, perms: Vector[Perm], ringSize: Vector[Int])

  private def dist(a: (Double, Double, Double), b: (Double, Double, Double)): Double =
    val (dx, dy, dz) = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
    math.sqrt(dx * dx + dy * dy + dz * dz)

  private val symCache  = collection.mutable.Map.empty[Int, StarSymmetry]
  private val subsCache = collection.mutable.Map.empty[Int, Vector[Set[Perm]]]

  /** The full subgroup lattice of species i's chamber group, memoized (the driver sweeps it repeatedly). */
  def subgroupsOfSpecies(i: Int): Vector[Set[Perm]] =
    subsCache.getOrElseUpdate(i, subgroupsOf(symmetryOf(i).perms))

  /** The star's symmetry group acting on its chambers (memoized). */
  def symmetryOf(i: Int): StarSymmetry = symCache.getOrElseUpdate(i, computeSymmetry(i))

  private def computeSymmetry(i: Int): StarSymmetry =
    val sp        = species(i)
    val cx        = StarChambers.complexOf(sp)
    val g         = geomOf(sp)
    val stab      = stabilizers(g, cornerData(g))
    val cornerKey = sp.state.corners.zipWithIndex.map((c, ci) => (c.cell, c.vids.toSet) -> ci).toMap
    require(cornerKey.size == sp.state.corners.size, "corner (cell type, vid-set) keys must be unique")
    val perms     = stab.map { m =>
      val p = g.u.indices.toVector.map { x =>
        val im   = m(g.u(x))
        val hits = g.u.indices.filter(y => dist(g.u(y), im) < 1e-4)
        require(hits.size == 1, s"stab matrix must permute tiling-vertices (vertex $x hit ${hits.size})")
        hits.head
      }
      cx.chambers.map { ch =>
        val c0    = sp.state.corners(ch.corner)
        val sz    = c0.vids.size
        val arcA  = p(c0.vids(ch.side))
        val arcB  = p(c0.vids((ch.side + 1) % sz))
        val vEnd  = p(c0.vids((ch.side + ch.end) % sz))
        val c2i   = cornerKey((c0.cell, c0.vids.map(p).toSet))
        val c2    = sp.state.corners(c2i)
        val side2 = c2.vids.indices
          .find(j => Set(c2.vids(j), c2.vids((j + 1) % c2.vids.size)) == Set(arcA, arcB))
          .get
        val end2  = if c2.vids(side2) == vEnd then 0 else 1
        cx.index(Chamber(c2i, side2, end2))
      }
    }
    val ringSize  = sp.state.positions.indices.toVector.map(x => SpeciesCorona.ringAt(sp.state, x).size)
    StarSymmetry(cx, perms, ringSize)

  private def compose(a: Perm, b: Perm): Perm = b.map(a) // (a ∘ b)(x) = a(b(x))

  private def closure(gens: Set[Perm], id: Perm): Set[Perm] =
    var cur   = gens + id
    var fresh = cur
    while fresh.nonEmpty do
      val next = (for a <- fresh; b <- cur yield compose(a, b)) ++
        (for a <- cur; b <- fresh yield compose(a, b))
      fresh = next -- cur
      cur ++= fresh
    cur

  /** ALL subgroups of the chamber-permutation group: closure-BFS from the trivial subgroup, adding one
    * outside element at a time — every subgroup is reached through its own generators.
    */
  def subgroupsOf(perms: Vector[Perm]): Vector[Set[Perm]] =
    val id                   = Vector.tabulate(perms.head.size)(identity)
    val all                  = perms.toSet
    var subs: Set[Set[Perm]] = Set(Set(id))
    var frontier             = subs
    while frontier.nonEmpty do
      val next  = for H <- frontier; x <- all if !H(x) yield closure(H + x, id)
      val fresh = next -- subs
      subs ++= fresh
      frontier = fresh
    subs.toVector

  /** A folded chamber complex: the quotient by a subgroup, with the m-data σ₀-assembly consumes. */
  final case class Folded(
      size: Int,
      s1: Vector[Int],
      s2: Vector[Int],
      s3: Vector[Int],
      m01: Vector[Int],    // face size at the chamber (σ₀σ₁-order target)
      m23: Vector[Int],    // ring size at the chamber's edge (σ₂σ₃-order, realized in-star)
      cell: Vector[Int],   // cell type ordinal (constant on orbits: symmetries preserve cell types)
      orbitOf: Vector[Int] // original chamber -> folded chamber
  )

  def fold(sym: StarSymmetry, sub: Set[Perm]): Folded =
    val n       = sym.cx.chambers.size
    val orbitOf = Array.fill(n)(-1)
    val reps    = Vector.newBuilder[Int]
    var k       = 0
    for c <- 0 until n do
      if orbitOf(c) < 0 then
        for h <- sub do orbitOf(h(c)) = k
        reps += c
        k += 1
    val r       = reps.result()
    Folded(
      k,
      r.map(c => orbitOf(sym.cx.s1(c))),
      r.map(c => orbitOf(sym.cx.s2(c))),
      r.map(c => orbitOf(sym.cx.s3(c))),
      r.map(sym.cx.faceSize),
      r.map(c => sym.ringSize(sym.cx.endVertex(c))),
      r.map(sym.cx.cellOrd),
      orbitOf.toVector
    )
