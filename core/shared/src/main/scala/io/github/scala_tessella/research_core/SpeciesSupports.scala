package io.github.scala_tessella.research_core

import HoneycombAlphabet.*
import HoneycombAlphabet.CellType

/** The AREA equation for vertex figures. At a honeycomb vertex the cell corners tile the
  * sphere of directions, so their solid angles (spherical excesses) sum to exactly 720 excess-degrees. All
  * core corner excesses lie in the two-tier lattice (excess = Σ dihedrals − (k−2)·180 = r + n·α), so the area
  * equation SPLITS exactly like the edge equation: Σr = 720 and Σn = 0. Together with the per-face- size
  * parity condition (every arc of the spherical tiling is shared by two corners, so each face size must occur
  * an even number of times over the multiset), this bounds the cell multiset of EVERY possible vertex species
  * — the 3D analogue of the 21-species arithmetic table, at multiset level.
  */
object SpeciesSupports:

  /** Corner configurations of the 13 core cells (cyclic face sizes around a vertex). */
  val cornerConfigs: Map[CellType, List[Int]] = Map(
    CellType.Tet                 -> List(3, 3, 3),
    CellType.Cube                -> List(4, 4, 4),
    CellType.Oct                 -> List(3, 3, 3, 3),
    CellType.TruncTet            -> List(3, 6, 6),
    CellType.Cuboctahedron       -> List(3, 4, 3, 4),
    CellType.TruncOct            -> List(4, 6, 6),
    CellType.TruncCube           -> List(3, 8, 8),
    CellType.Rhombicuboctahedron -> List(3, 4, 4, 4),
    CellType.TruncCuboctahedron  -> List(4, 6, 8),
    CellType.P3                  -> List(3, 4, 4),
    CellType.P6                  -> List(6, 4, 4),
    CellType.P8                  -> List(8, 4, 4),
    CellType.P12                 -> List(12, 4, 4)
  )

  private val dihedralOf: Map[(CellType, (Int, Int)), CoreAngle] =
    edgeTypes.map(et => (et.cell, et.faces) -> et.angle).toMap ++
      edgeTypes.map(et => (et.cell, et.faces.swap) -> et.angle).toMap

  /** Exact corner excess (solid angle in excess-degrees) of each core cell: Σ dihedrals − (k−2)·180. */
  val cornerExcess: Map[CellType, CoreAngle] =
    cornerConfigs.map { (cell, cfg) =>
      val k      = cfg.size
      val angles =
        cfg.indices.map(i => dihedralOf(cell -> (cfg((i + k - 1) % k), cfg(i)))).toList
      cell -> angles.foldLeft(CoreAngle(-(k - 2) * 180, 0))(_ + _)
    }

  /** A candidate species support: cell type → corner count, satisfying the exact area equation and the
    * per-face-size parity condition.
    */
  final case class Support(counts: Map[CellType, Int]):
    def cells: Int   = counts.values.sum
    def show: String =
      counts.toList.sortBy(_._1.ordinal).map((c, m) => s"${c.label}:$m").mkString("{", " ", "}")

  /** All solutions of Σ m·excess = (720, 0) with the parity filter. Exact; complete. */
  lazy val supports: Vector[Support] =
    val cells                                                = CellType.values.toVector
    val excess                                               = cells.map(cornerExcess)
    val minDeg                                               = excess.map(_.degrees).min // tet, 31.58…
    val found                                                = Vector.newBuilder[Support]
    def rec(i: Int, counts: List[Int], sum: CoreAngle): Unit =
      if i == cells.size then
        if sum == CoreAngle(720, 0) then
          val cnt        = cells.zip(counts.reverse).filter(_._2 > 0).toMap
          // parity: every face size must appear an even number of times across the corners
          val sideCounts = cnt.toList
            .flatMap((c, m) => cornerConfigs(c).map(_ -> m))
            .groupMapReduce(_._1)(_._2)(_ + _)
          if sideCounts.values.forall(_ % 2 == 0) then found += Support(cnt)
      else
        val maxM = ((720.0 - sum.degrees) / excess(i).degrees + 1e-9).toInt.max(0)
        for m <- 0 to maxM do
          val s = CoreAngle(sum.r + m * excess(i).r, sum.n + m * excess(i).n)
          if s.degrees <= 720.0 + 1e-9 then rec(i + 1, m :: counts, s)
    rec(0, Nil, CoreAngle(0, 0))
    found.result().sortBy(s => (s.cells, s.show))
