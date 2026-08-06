package io.github.scala_tessella.research_core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species
import StarChambers.complexOf
import StarFoldings.{fold, subgroupsOf, symmetryOf}
import Sigma0Assembly.{enumerateSigma0, unionOf, ChamberUnion}
import SymbolCatalog.{canonicalKey, isMinimal, valid, Sym}

/** Structural sanity of the honeycomb substrate — the engines only, on their own terms: no enumeration COUNT
  * is asserted here, because counts are results and results belong to the verification repository of the
  * paper that states them. What is asserted is that each engine satisfies the invariants its own contract
  * promises, checked independently of the code path that produces them:
  *
  *   - the assembled spherical complexes are closed (every arc owned twice) and flat at every tiling vertex,
  *     the acceptance criterion re-read off the finished state rather than off the search;
  *   - the chamber complex satisfies the flag laws (three fixed-point-free involutions, σ₁σ₃ = σ₃σ₁, 4
  *     chambers per arc);
  *   - subgroup enumeration agrees with brute force, and Lagrange holds on the order-48 cubic star group;
  *   - the propagating σ₀ enumerator agrees with a BRUTE-FORCE ORACLE (all involutions with fixed points,
  *     filtered by directly-written axiom formulas) on small foldings;
  *   - canonical keys are relabeling-invariant, and minimality behaves on hand fixtures.
  */
class HoneycombSubstrateSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val cubic                  = bySupport("{cube:8}").head
  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse

  // ---------- the spherical complexes ----------

  "every assembled species" should "be a closed complex, flat at every tiling vertex" in:
    species should not be empty
    for sp <- species do
      withClue(s"${sp.showSupport}: "):
        sp.state.arcs.values.foreach(_.owners.size shouldBe 2)
        sp.state.excess shouldBe HoneycombAlphabet.CoreAngle(720, 0)
        sp.state.positions.indices.foreach(vid =>
          sp.state.sums(vid) shouldBe HoneycombAlphabet.CoreAngle(360, 0)
        )

  // ---------- the chamber complexes ----------

  "every species' chamber complex" should "satisfy the flag laws" in:
    for sp <- species do
      val cx = complexOf(sp)
      withClue(s"${sp.showSupport}: "):
        cx.chambers.size shouldBe 4 * sp.state.arcs.size
        for s <- Vector(cx.s1, cx.s2, cx.s3) do
          s.indices.foreach { c =>
            s(s(c)) shouldBe c // involution
            s(c) should not be c // fixed-point-free
          }
        cx.s1.indices.foreach(c => cx.s1(cx.s3(c)) shouldBe cx.s3(cx.s1(c)))

  // ---------- the subgroup lattice ----------

  "the subgroup enumeration" should "agree with brute force on the small stabilizers" in:
    // brute force: every nonempty subset closed under composition (a finite closed subset is a subgroup)
    val small = species.indices.filter(j => symmetryOf(j).perms.size <= 8).take(6)
    small should not be empty
    for i <- small do
      val g     = symmetryOf(i).perms
      val brute = (0 until (1 << g.size))
        .map(mask => g.indices.filter(j => (mask & (1 << j)) != 0).map(g).toSet)
        .filter(h => h.nonEmpty && h.forall(a => h.forall(b => h.contains(b.map(a)))))
        .toSet
      withClue(s"species ${species(i).showSupport} (|G| = ${g.size}): "):
        subgroupsOf(g).toSet shouldBe brute

  it should "satisfy Lagrange and conjugation-closure on the cubic star's order-48 group" in:
    val sym                          = symmetryOf(cubic)
    sym.perms.size shouldBe 48
    val subs                         = subgroupsOf(sym.perms)
    subs.foreach(sub => 48 % sub.size shouldBe 0)
    subs.map(_.size).max shouldBe 48
    def invert(p: StarFoldings.Perm) =
      val inv = Array.fill(p.size)(0)
      for x <- p.indices do inv(p(x)) = x
      inv.toVector
    val set                          = subs.toSet
    for h <- subs; g <- sym.perms do set should contain(h.map(x => invert(g).map(x).map(g)))

  // ---------- the σ₀ enumerator against an independent oracle ----------

  /** All involutions on 0 until n, fixed points allowed. */
  private def involutions(n: Int): Vector[Vector[Int]] =
    def gen(pending: Vector[Int], acc: Map[Int, Int]): Vector[Map[Int, Int]] =
      pending match
        case c +: rest =>
          gen(rest, acc + (c -> c)) ++
            rest.flatMap(d => gen(rest.filterNot(_ == d), acc + (c -> d) + (d -> c)))
        case _         => Vector(acc)
    gen(Vector.range(0, n), Map.empty).map(m => Vector.tabulate(n)(m))

  /** The axioms, written directly — no propagation, no incrementality. */
  private def axiomsOk(u: ChamberUnion, s0: Vector[Int]): Boolean =
    val n                     = u.size
    val matching              = (0 until n).forall { c =>
      val d = s0(c)
      u.m01(c) == u.m01(d) && u.m23(c) == u.m23(d) && u.cell(c) == u.cell(d)
    }
    val commute               = (0 until n).forall(c => s0(u.s2(c)) == u.s2(s0(c)) && s0(u.s3(c)) == u.s3(s0(c)))
    def cycleLen(c: Int): Int =
      var a = u.s1(s0(c))
      var k = 1
      while a != c do
        a = u.s1(s0(a))
        k += 1
      k
    val faces                 = (0 until n).forall(c => u.m01(c) % cycleLen(c) == 0)
    val conn                  =
      val seen  = collection.mutable.Set(0)
      var front = List(0)
      while front.nonEmpty do
        front = front.flatMap(c => List(s0(c), u.s1(c), u.s2(c), u.s3(c)).filter(seen.add))
      seen.size == n
    matching && commute && faces && conn

  "the σ₀ enumerator" should "equal the brute-force oracle on small foldings" in:
    val symCubic                           = symmetryOf(cubic)
    val symOctetC                          = symmetryOf(octetC)
    val symOctetH                          = symmetryOf(octetH)
    def full(s: StarFoldings.StarSymmetry) = fold(s, s.perms.toSet)
    val cases                              = Vector(
      Vector(full(symCubic)),
      Vector(full(symOctetC)),
      Vector(full(symOctetH)),
      Vector(full(symCubic), full(symOctetC)) // two orbits, no cross-compatible chamber
    ) ++ subgroupsOf(symCubic.perms).filter(h => 48 / h.size <= 8).map(h => Vector(fold(symCubic, h)))
    for parts <- cases do
      val u             = unionOf(parts)
      val (found, capd) = enumerateSigma0(u)
      withClue(s"union of sizes ${parts.map(_.size).mkString("+")}: "):
        capd shouldBe false
        found.toSet shouldBe involutions(u.size).filter(axiomsOk(u, _)).toSet

  // ---------- canonical keys and minimality ----------

  "the canonical key" should "be invariant under chamber relabeling" in:
    val sym                                  = symmetryOf(octetH)
    val u                                    = unionOf(Vector(fold(sym, sym.perms.toSet)))
    val (sols, _)                            = enumerateSigma0(u)
    val syms                                 = sols.map(SymbolCatalog.symOf(u, Vector(octetH), _))
    syms should not be empty
    val rnd                                  = new scala.util.Random(7)
    def relabel(s: Sym, p: Vector[Int]): Sym =
      def conj(g: Vector[Int]) =
        val out = Array.fill(s.size)(0)
        for c <- 0 until s.size do out(p(c)) = p(g(c))
        out.toVector
      def data(v: Vector[Int]) =
        val out = Array.fill(s.size)(0)
        for c <- 0 until s.size do out(p(c)) = v(c)
        out.toVector
      Sym(
        conj(s.s0),
        conj(s.s1),
        conj(s.s2),
        conj(s.s3),
        data(s.m01),
        data(s.m23),
        data(s.cell),
        data(s.speciesOf)
      )
    for s <- syms; _ <- 1 to 5 do
      canonicalKey(relabel(s, rnd.shuffle(Vector.range(0, s.size)))) shouldBe canonicalKey(s)

  "minimality" should "hold for the 1-chamber symbol and fail for its hand-built double" in:
    val single = Sym(Vector(0), Vector(0), Vector(0), Vector(0), Vector(4), Vector(4), Vector(1), Vector(0))
    isMinimal(single) shouldBe true
    val swap   = Vector(1, 0)
    val double = Sym(swap, swap, swap, swap, Vector(4, 4), Vector(4, 4), Vector(1, 1), Vector(0, 0))
    valid(double) shouldBe true
    isMinimal(double) shouldBe false
