package io.github.scala_tessella.research_core

/** The core cell alphabet of the three-dimensional programme — the 13 unit-edge uniform polyhedra occurring
  * among the 28 convex uniform honeycombs — with EXACT dihedral angles and the edge-figure enumerator.
  *
  * The two-tier arithmetic: with α = arctan √2 (irrational in degrees, since 2cos 2α = −2/3 is a rational
  * non-integer, Niven), every core dihedral is r + n·α with r a multiple of 15° and n ∈ {−2,…,2}. Around an
  * edge the angles sum to 360° exactly, and since α/π is irrational this SPLITS: Σn = 0 (the α-charges
  * cancel) and Σr = 360. Edge figures are therefore a finite linear-diophantine enumeration — no
  * transcendence theory inside the core alphabet.
  */
object HoneycombAlphabet:

  /** An exact core angle r + n·α in degrees; r a multiple of 15, |n| ≤ 2 per single dihedral. */
  final case class CoreAngle(r: Int, n: Int):
    def degrees: Double            = r + n * CoreAngle.alphaDeg
    def +(o: CoreAngle): CoreAngle = CoreAngle(r + o.r, n + o.n)
  object CoreAngle:
    val alphaDeg: Double = math.toDegrees(math.atan(math.sqrt(2.0))) // 54.7356…
    val zero: CoreAngle  = CoreAngle(0, 0)
    val full: CoreAngle  = CoreAngle(360, 0)

  enum CellType(val label: String):
    case Tet                 extends CellType("tet")
    case Cube                extends CellType("cube")
    case Oct                 extends CellType("oct")
    case TruncTet            extends CellType("truncTet")
    case Cuboctahedron       extends CellType("co")
    case TruncOct            extends CellType("truncOct")
    case TruncCube           extends CellType("truncCube")
    case Rhombicuboctahedron extends CellType("rco")
    case TruncCuboctahedron  extends CellType("tco")
    case P3                  extends CellType("p3")
    case P6                  extends CellType("p6")
    case P8                  extends CellType("p8")
    case P12                 extends CellType("p12")

  import CellType.*

  /** One edge type of a cell: the (unordered) pair of face sizes meeting there and the dihedral angle. */
  final case class EdgeType(cell: CellType, faces: (Int, Int), angle: CoreAngle)

  /** The complete exact dihedral table of the core alphabet. Charges: (180,−2) = arccos 1/3, (0,2) =
    * arccos(−1/3), (180,−1) = arccos(−√3/3), (90,1) = arccos(−√6/3); the rest are rational.
    */
  val edgeTypes: Vector[EdgeType] = Vector(
    EdgeType(Tet, (3, 3), CoreAngle(180, -2)),
    EdgeType(Cube, (4, 4), CoreAngle(90, 0)),
    EdgeType(Oct, (3, 3), CoreAngle(0, 2)),
    EdgeType(TruncTet, (6, 6), CoreAngle(180, -2)),
    EdgeType(TruncTet, (3, 6), CoreAngle(0, 2)),
    EdgeType(Cuboctahedron, (3, 4), CoreAngle(180, -1)),
    EdgeType(TruncOct, (4, 6), CoreAngle(180, -1)),
    EdgeType(TruncOct, (6, 6), CoreAngle(0, 2)),
    EdgeType(TruncCube, (3, 8), CoreAngle(180, -1)),
    EdgeType(TruncCube, (8, 8), CoreAngle(90, 0)),
    EdgeType(Rhombicuboctahedron, (3, 4), CoreAngle(90, 1)),
    EdgeType(Rhombicuboctahedron, (4, 4), CoreAngle(135, 0)),
    EdgeType(TruncCuboctahedron, (4, 6), CoreAngle(90, 1)),
    EdgeType(TruncCuboctahedron, (4, 8), CoreAngle(135, 0)),
    EdgeType(TruncCuboctahedron, (6, 8), CoreAngle(180, -1)),
    EdgeType(P3, (4, 4), CoreAngle(60, 0)),
    EdgeType(P3, (3, 4), CoreAngle(90, 0)),
    EdgeType(P6, (4, 4), CoreAngle(120, 0)),
    EdgeType(P6, (4, 6), CoreAngle(90, 0)),
    EdgeType(P8, (4, 4), CoreAngle(135, 0)),
    EdgeType(P8, (4, 8), CoreAngle(90, 0)),
    EdgeType(P12, (4, 4), CoreAngle(150, 0)),
    EdgeType(P12, (4, 12), CoreAngle(90, 0))
  )

  /** Face-type compatibility: which cells carry a p-gonal face (a p-face glues only cells in this list). */
  val cellsWithFace: Map[Int, Vector[CellType]] =
    edgeTypes
      .flatMap(et => Vector(et.faces._1 -> et.cell, et.faces._2 -> et.cell))
      .distinct
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.distinct.sortBy(_.ordinal))
      .toMap

  /** An edge type placed in a cyclic edge figure: `left`/`right` are the faces shared with the
    * predecessor/successor cell around the edge.
    */
  final case class Oriented(et: EdgeType, flipped: Boolean):
    def left: Int  = if flipped then et.faces._2 else et.faces._1
    def right: Int = if flipped then et.faces._1 else et.faces._2

  private val orientedTokens: Vector[Oriented]   =
    edgeTypes.flatMap { et =>
      if et.faces._1 == et.faces._2 then Vector(Oriented(et, false))
      else Vector(Oriented(et, false), Oriented(et, true))
    }
  private val byLeft: Map[Int, Vector[Oriented]] = orientedTokens.groupBy(_.left)

  type CanonKey = Vector[(Int, Int, Int)] // (cell ordinal, left face, right face) per position

  /** Canonical key of a cyclic edge figure: lexicographic minimum over rotations and the reflection (which
    * reverses the cyclic order and swaps each token's left/right).
    */
  def canonical(seq: Vector[Oriented]): CanonKey =
    import scala.math.Ordering.Implicits.seqOrdering
    val fwd  = seq.map(o => (o.et.cell.ordinal, o.left, o.right))
    val bwd  = seq.reverse.map(o => (o.et.cell.ordinal, o.right, o.left))
    val reps =
      for
        base <- Vector(fwd, bwd)
        k    <- base.indices
      yield base.drop(k) ++ base.take(k)
    reps.min

  /** A complete edge figure: cells around an edge, consecutive ones sharing a face of the stated size,
    * dihedral angles summing to exactly 360° (Σr = 360 AND Σn = 0).
    */
  final case class EdgeFigure(tokens: Vector[Oriented]):
    def key: CanonKey           = canonical(tokens)
    def size: Int               = tokens.size
    def cells: Vector[CellType] = tokens.map(_.et.cell)
    def show: String            =
      tokens.map(o => s"${o.et.cell.label}(${o.left}·${o.right})").mkString("[", " ", "]")

  /** The complete edge-figure catalogue of the core alphabet. Since every dihedral is ≥ 60° (P3) and < 180°,
    * a figure has 3 to 6 cells; DFS with face-matching chain and angle pruning, deduplicated by canonical
    * key.
    */
  lazy val catalogue: Vector[EdgeFigure] =
    import scala.math.Ordering.Implicits.seqOrdering
    val found                                          = collection.mutable.Map.empty[CanonKey, EdgeFigure]
    def rec(seq: List[Oriented], sum: CoreAngle): Unit =
      val deg = sum.degrees
      if sum == CoreAngle.full && seq.size >= 3 && seq.head.right == seq.last.left then
        val fig = EdgeFigure(seq.reverse.toVector)
        found.getOrElseUpdate(fig.key, fig): Unit
      else if seq.size < 6 && deg + 59.999 < 360.0 then
        for o <- byLeft.getOrElse(seq.head.right, Vector.empty) do rec(o :: seq, sum + o.et.angle)
    for o <- orientedTokens do rec(List(o), o.et.angle)
    found.values.toVector.sortBy(f => (f.size, f.key))

  /** Catalogue restricted to figures whose cells all lie in the given sub-alphabet. */
  def restrictedTo(cells: Set[CellType]): Vector[EdgeFigure] =
    catalogue.filter(_.cells.forall(cells.contains))
