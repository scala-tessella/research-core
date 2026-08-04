package io.github.scala_tessella.research_core

import SpeciesEnumerator.{species, Species, State}

/** The CHAMBER COMPLEX of a species star — the flags of a honeycomb at a vertex carrying the species, with
  * the three internal involutions. This is the "vertex star" side of the three-dimensional Delaney–Dress
  * symbol assembly: a k-orbit honeycomb's symbol is k such stars (quotiented by foldings) joined by ONE
  * unknown involution σ₀ (crossing the edge to the neighbor vertex), exactly as in the two-dimensional
  * assembly with one more internal involution.
  *
  * A flag (v, e, f, c) at the base vertex v is encoded from [[SpeciesEnumerator]]'s certified spherical
  * complex as (corner,
  * side, end): the CELL is the corner, the FACE is one of the corner's sides (arc i spans vids(i),
  * vids(i+1)), and the EDGE is one of that face's two edges at v, named by its end tiling-vertex vids(i) or
  * vids(i+1). The involutions:
  *
  *   - σ₁ (change edge within the face): flip the end;
  *   - σ₂ (change face within the cell): the corner's other side at the same end vertex;
  *   - σ₃ (change cell across the face): the arc's other owner, end matched by tiling-vertex.
  *
  * All three are fixed-point-free involutions (the star complex is closed with every arc two-owned — the
  * enumerator's
  * acceptance), σ₁ and σ₃ commute, the (σ₁σ₂)-composition has order p over a p-corner (the m₁₂ data) and the
  * (σ₂σ₃)-composition order r over an r-ring edge (the m₂₃ data), and the chamber count is 4·#arcs. The face
  * size at a chamber (the m₀₁ data σ₀-assembly needs) is the glued face size of its arc.
  */
object StarChambers:

  /** A chamber of the star: corner (cell at v), side (face of that cell at v), end (edge of that face at v,
    * as 0 = vids(side), 1 = vids(side + 1)).
    */
  final case class Chamber(corner: Int, side: Int, end: Int)

  final case class StarComplex(
      chambers: Vector[Chamber],
      index: Map[Chamber, Int],
      s1: Vector[Int],
      s2: Vector[Int],
      s3: Vector[Int],
      faceSize: Vector[Int],  // glued face size at the chamber's arc (the m01 data)
      endVertex: Vector[Int], // the tiling-vertex of the chamber's edge (σ0 will cross it)
      cellOrd: Vector[Int]    // the chamber's cell type ordinal (shared across an edge: σ0 must match it)
  )

  private def arcKey(a: Int, b: Int): (Int, Int) = if a < b then (a, b) else (b, a)

  def complexOf(sp: Species): StarComplex =
    val st                      = sp.state
    val chambers                =
      for
        c    <- st.corners.indices.toVector
        side <- st.corners(c).vids.indices
        end  <- 0 to 1
      yield Chamber(c, side, end)
    val index                   = chambers.zipWithIndex.toMap
    def vidOf(ch: Chamber): Int =
      val vids = st.corners(ch.corner).vids
      vids((ch.side + ch.end) % vids.size)
    val s1                      = chambers.map(ch => index(ch.copy(end = 1 - ch.end)))
    val s2                      = chambers.map { ch =>
      val sz = st.corners(ch.corner).vids.size
      if ch.end == 0 then index(Chamber(ch.corner, (ch.side + sz - 1) % sz, 1))
      else index(Chamber(ch.corner, (ch.side + 1) % sz, 0))
    }
    val s3                      = chambers.map { ch =>
      val vids        = st.corners(ch.corner).vids
      val key         = arcKey(vids(ch.side), vids((ch.side + 1) % vids.size))
      val owners      = st.arcs(key).owners
      val (c2, side2) = owners.find(_ != (ch.corner, ch.side)).getOrElse(
        // an arc glued twice to the same corner side cannot occur in a closed star; a self-paired corner
        // (same corner, different side) can — owners are (corner, side) pairs, so inequality is on both
        sys.error(s"arc $key is not two-owned")
      )
      val vids2       = st.corners(c2).vids
      val myVid       = vidOf(ch)
      val end2        = if vids2(side2) == myVid then 0 else 1
      index(Chamber(c2, side2, end2))
    }
    val faceSize                = chambers.map { ch =>
      val vids = st.corners(ch.corner).vids
      st.arcs(arcKey(vids(ch.side), vids((ch.side + 1) % vids.size))).face
    }
    val endVer                  = chambers.map(vidOf)
    val cellOrd                 = chambers.map(ch => st.corners(ch.corner).cell.ordinal)
    StarComplex(chambers, index, s1, s2, s3, faceSize, endVer, cellOrd)

  /** Orbit of `start` under the composition s(a) ∘ s(b) alternately — orbit sizes of (σᵢσⱼ). */
  def orbitSize(a: Vector[Int], b: Vector[Int], start: Int): Int =
    var cur = start
    var n   = 0
    while
      cur = a(b(cur))
      n += 1
      cur != start
    do ()
    n

  /** The complexes of all species, by species index. */
  lazy val all: Vector[StarComplex] = species.map(complexOf)
