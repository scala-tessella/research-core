package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon

/** EXACT development of a Delaney symbol at a rigid angle point — the ℤ[ζ_N] analogue of the numeric
  * [[DeformedRenderer]], and the patch source of the exact de-fusion engine (`notes/saturation.md`;
  * Scala-only per the 2026-08-17 directive).
  *
  * A developed FLAG is a symbol chamber with an exact vertex position, the direction of its edge away from
  * that vertex (integer units of 2π/N), and a chirality; the three σ-steps act on flags by pure arithmetic —
  * positions move by roots of unity, directions by integer turns — so unit edges and exact vertex coincidence
  * hold by construction. Faces are extracted by walking σ₁∘σ₀ cycles and land as [[ExactPlane.UnitPolygon]]s
  * with exact anchors.
  */
object ExactDeveloper:

  /** The direction lattice for an angle assignment: N = 2·lcm of the denominators, the least even N making
    * every angle aπ an integer count a·N/2 of 2π/N units.
    */
  def latticeOf(angles: Iterable[Frac]): Int =
    def lcm(a: Long, b: Long): Long =
      def gcd(x: Long, y: Long): Long = if y == 0 then x.abs else gcd(y, x % y)
      (a / gcd(a, b)) * b
    2 * angles.foldLeft(1L)((acc, a) => lcm(acc, a.den)).toInt

  /** The angle aπ in 2π/N units — exact by choice of N. */
  def angleUnits(a: Frac, n: Int): Int =
    require((a.num * n / 2) % a.den == 0, s"angle $a not on the 2π/$n lattice")
    (a.num * n / 2 / a.den).toInt

  /** A developed flag: chamber, exact vertex position, edge direction away from the vertex (2π/N units),
    * chirality ±1.
    */
  final case class Flag(chamber: Int, pos: Cyc, dir: Int, chir: Int):
    /** Identity key: chamber, reduced position, direction mod N, chirality. */
    def key(n: Int): (Int, Vector[BigInt], Int, Int) =
      (chamber, pos.reducedKey, ((dir % n) + n) % n, chir)

  /** One σ_i step. σ₀ walks the edge (position advances by ζ^dir, direction reverses); σ₁ rotates to the
    * corner's other edge by the corner's interior angle (sense = chirality); σ₂ crosses to the neighbouring
    * face (geometry unchanged). All flip chirality.
    */
  def step(ds: DSymbol, unitsAt: Int => Int, n: Int)(f: Flag, i: Int): Flag =
    i match
      case 0 => Flag(ds.get(0, f.chamber), f.pos + Cyc.root(n, f.dir), f.dir + n / 2, -f.chir)
      case 1 => Flag(ds.get(1, f.chamber), f.pos, f.dir + f.chir * unitsAt(f.chamber), -f.chir)
      case _ => Flag(ds.get(2, f.chamber), f.pos, f.dir, -f.chir)

  /** A face instance: the walk's starting chamber, its exact anchor vertex, its polygon. */
  final case class PlacedFace(startChamber: Int, anchor: Cyc, poly: UnitPolygon)

  final case class ExactPatch(n: Int, faces: Vector[PlacedFace]):
    /** Interior angle units accumulated at each exact vertex (keyed by reduced position). An interior vertex
      * of a consistent development sums to exactly N; a patch-boundary vertex to less. A sum above N means
      * overlapping faces — the audit for holonomy defects.
      */
    def vertexAngleSums: Map[Vector[BigInt], Int] =
      faces
        .flatMap(f => f.poly.verticesFrom(f.anchor).map(_.reducedKey).zip(f.poly.interiorAngles))
        .groupMapReduce(_._1)(_._2)(_ + _)

  /** Develop the symbol at the angle point `x` (Frac per corner variable, π units) out to flags within
    * `radius` of the origin (numeric bound only), then extract every face whose full boundary was developed.
    */
  def develop(ds: DSymbol, x: Array[Frac], radius: Double): ExactPatch =
    val sys                             = MetricLayer.angleSystem(ds)
    val corner                          = sys.corner
    val n                               = latticeOf((1 to ds.size).map(c => x(corner(c))))
    val unitsAt                         = (c: Int) => angleUnits(x(corner(c)), n)
    val stepper                         = step(ds, unitsAt, n)
    val start                           = Flag(1, Cyc.zero(n), 0, 1)
    val visited                         = collection.mutable.Map(start.key(n) -> start)
    val frontier                        = collection.mutable.Queue(start)
    inline def inRange(p: Cyc): Boolean =
      val (px, py) = p.approx
      px * px + py * py <= radius * radius
    while frontier.nonEmpty do
      val f = frontier.dequeue()
      for i <- 0 to 2 do
        val g = stepper(f, i)
        val k = g.key(n)
        if !visited.contains(k) && inRange(g.pos) then
          visited(k) = g
          frontier.enqueue(g)

    // extract faces: from each unconsumed chirality-(+1) flag, walk σ₁∘σ₀ collecting the edge
    // directions; keep the face only if the whole cycle stayed inside the developed set
    val consumed = collection.mutable.Set.empty[(Int, Vector[BigInt], Int, Int)]
    val faces    = Vector.newBuilder[PlacedFace]
    val ordered  = visited.values.toVector
      .sortBy(f => (f.chamber, f.pos.reducedKey.mkString(","), ((f.dir % n) + n) % n))
    for f <- ordered if f.chir == 1 && !consumed(f.key(n)) do
      val dirs     = Vector.newBuilder[Int]
      var cur      = f
      var complete = true
      var go       = true
      while go do
        consumed += cur.key(n)
        dirs += ((cur.dir % n) + n) % n
        val nxt = stepper(stepper(cur, 0), 1)
        if !visited.contains(nxt.key(n)) then { complete = false; go = false }
        else if nxt.key(n) == f.key(n) then go = false
        else cur = nxt
      val ds1      = dirs.result()
      if complete && ds1.length >= 3 then faces += PlacedFace(f.chamber, f.pos, UnitPolygon(n, ds1))
    ExactPatch(n, faces.result())
