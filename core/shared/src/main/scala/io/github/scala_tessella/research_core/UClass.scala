package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.Signatures.VertexSignature

/** G4 — the U class, formalized on Delaney–Dress symbols. A unit-edge tiling belongs to U(z), z one of the 21
  * arithmetic vertex types, iff
  *
  *   1. some vertex is ALL-REGULAR with configuration exactly z (as a cyclic sequence);
  *   2. every vertex's regular sub-configuration (RVS) is non-empty;
  *   3. every RVS is a CYCLIC SUBSET of z (embeds preserving cyclic order, up to rotation/reflection).
  *
  * The historical fourth constraint (maximality of each RVS) is DELIBERATELY dropped, and (1) is read
  * permissively (no exclusivity clause — anyway implied by (3): a full 360° type embeds in z only if it
  * equals z). Both readings only SHRINK the class relative to this one, so a refutation of "U(z) has a tiling
  * with k vertex orbits" under (1)–(3) is sound for the stricter class as well.
  *
  * A symbol is tested over all DESIGNATIONS (subsets of face orbits declared regular): a genuine U(z) tiling
  * with uniformity k yields its full-symmetry symbol (≤ 12k chambers, the G0 bound) plus the
  * truth-designation, which passes [[designations]] and whose PINNED linear system (regular corners at
  * (p−2)π/p on top of the angle sums) is exactly consistent — so scanning all symbols with ≤ k vertex orbits
  * × all designations, and refuting each, refutes uniformity ≤ k for the whole class.
  */
object UClass:

  /** `w` is a CONTIGUOUS cyclic subword of `z`, up to rotation and reflection — the manuscript-era semantics,
    * whose fixture is (3.3) ⊂ (3.3.6.6) but (3.3) ⊄ (3.6.3.6): adjacency matters, so a plain cyclic
    * subsequence is too permissive.
    */
  def cyclicSubset(w: List[Int], z: List[Int]): Boolean =
    w.length <= z.length && {
      val zs = z.indices.flatMap: r =>
        val rot = z.drop(r) ++ z.take(r)
        List(rot, rot.reverse)
      val ws = w.indices.map(r => w.drop(r) ++ w.take(r))
      ws.exists(wr => zs.exists(_.startsWith(wr)))
    }

  private def cyclicEq(a: List[Int], b: List[Int]): Boolean = a.length == b.length && cyclicSubset(a, b)

  /** All designations (sets of 01-orbit indices declared REGULAR) under which `ds` satisfies U(z)
    * combinatorially: some all-regular vertex orbit has configuration z; every vertex orbit's RVS is
    * non-empty and a cyclic subset of z.
    */
  def designations(ds: DSymbol, z: VertexSignature): List[Set[Int]] =
    val faceOrbits = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 => k }
    val configs    = ds.orbs
      .filter(o => o.i == 1)
      .map(o => DelaneySymbols.vertexConfigOrbits(ds, o.elements.head).get)
    (0 until (1 << faceOrbits.size)).iterator
      .map(mask => faceOrbits.zipWithIndex.collect { case (f, i) if (mask & (1 << i)) != 0 => f }.toSet)
      .filter: reg =>
        val rvss = configs.map(_.filter((f, _) => reg(f)))
        rvss.forall(_.nonEmpty) && {
          val sizes = rvss.map(_.map(_._2))
          val fulls = configs.zip(sizes).collect { case (cfg, w) if cfg.length == w.length => w }
          fulls.exists(cyclicEq(_, z)) && sizes.forall(cyclicSubset(_, z))
        }
      .toList

  /** Exact consistency of the PINNED linear layer: the angle system of `ds` plus `γ = (p−2)/p` for every
    * corner of a regular-designated face orbit. Inconsistency refutes the designation metrically; a
    * consistent designation is a SURVIVOR needing closure-level (affine, per the review repair) analysis.
    */
  private[research_core] def pinnedConsistent(ds: DSymbol, regular: Set[Int]): Boolean =
    pinnedConsistent(ds, MetricLayer.angleSystem(ds), regular)

  private def pinnedConsistent(ds: DSymbol, sys: MetricLayer.AngleSystem, regular: Set[Int]): Boolean =
    MetricLayer.linearConsistent(sys.rows ++ facePins(ds, sys, regular), sys.vars)

  /** The `γ = (p−2)/p` pin rows for every corner of the given face orbits. */
  private def facePins(
      ds: DSymbol,
      sys: MetricLayer.AngleSystem,
      faces: Set[Int]
  ): Vector[(Array[Frac], Frac)] = (1 to ds.size).iterator
    .filter(d => faces(ds.orbitIndex(1)(d)))
    .map: d =>
      val row = Array.fill(sys.vars)(Frac(0, 1))
      row(sys.corner(d)) = Frac(1, 1)
      (row, Frac.make(ds.m(0, 1, d) - 2L, ds.m(0, 1, d)))
    .toVector

  /** True iff the pinned affine system already forces face orbit `f` (designated irregular) to its regular
    * angles — then every realization of this designation has a LARGER truth-designation, which is scanned
    * separately, so the designation yields no genuine U(z) tiling. A surviving designation is GENUINE only if
    * no irregular-designated face is forced regular: then a generic point of the affine family keeps them all
    * irregular simultaneously (finite-union argument). Linear layer only — sufficient whenever it refutes.
    */
  def forcedRegular(ds: DSymbol, regular: Set[Int], f: Int): Boolean =
    val sys  = MetricLayer.angleSystem(ds)
    val base = sys.rows ++ facePins(ds, sys, regular)
    forcedWith(ds, sys, base, baseNullity(sys, base), f)

  /** [[forcedRegular]] negated over every face of `irregular`, with the f-invariant base system and its
    * nullity computed ONCE: true iff no irregular-designated face is forced regular — the K3 genuineness
    * filter (calling `forcedRegular` per face redoes the base RREF each time).
    */
  def noneForcedRegular(
      ds: DSymbol,
      regular: Set[Int],
      irregular: Seq[Int]
  ): Boolean =
    irregular.isEmpty || {
      val sys  = MetricLayer.angleSystem(ds)
      val base = sys.rows ++ facePins(ds, sys, regular)
      val nul  = baseNullity(sys, base)
      irregular.forall(f => !forcedWith(ds, sys, base, nul, f))
    }

  private def baseNullity(sys: MetricLayer.AngleSystem, base: Vector[(Array[Frac], Frac)]): Int =
    MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, base)).size

  private def forcedWith(
      ds: DSymbol,
      sys: MetricLayer.AngleSystem,
      base: Vector[(Array[Frac], Frac)],
      nullity: Int,
      f: Int
  ): Boolean =
    val extra = base ++ facePins(ds, sys, Set(f))
    MetricLayer.linearConsistent(extra, sys.vars) &&
    MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, extra)).size == nullity

  /** U(z) candidates of `ds`: designations passing the combinatorial check AND the pinned linear layer (the
    * angle system depends only on `ds` — built once, not per designation).
    */
  def candidates(ds: DSymbol, z: VertexSignature): List[Set[Int]] =
    val sys = MetricLayer.angleSystem(ds)
    designations(ds, z).filter(pinnedConsistent(ds, sys, _))

  /** The ten conjecture targets (raw cyclic sequences) with their claimed minimal uniformities. */
  val targets: List[(List[Int], Int)] = List(
    List(3, 7, 42)    -> 10,
    List(3, 8, 24)    -> 3,
    List(3, 9, 18)    -> 4,
    List(3, 10, 15)   -> 5,
    List(4, 5, 20)    -> 7,
    List(5, 5, 10)    -> 4,
    List(3, 3, 4, 12) -> 4,
    List(3, 3, 6, 6)  -> 7,
    List(3, 4, 3, 12) -> 3,
    List(3, 4, 4, 6)  -> 3
  )
