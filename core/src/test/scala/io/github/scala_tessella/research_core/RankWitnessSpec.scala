package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Cyclo24.Cyclo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Generic unit tests for the algebraic rank-witness machinery over ℚ(ζ₂₄) ([[RankWitness]]), on hand-built
  * integer matrices — independent of any tiling. (The tiling-specific track-D certification — witnessing the
  * moduli ranks of all 93 symbols and their quotients through [[MetricLayer]] — lives in the paper
  * verification repo.)
  */
class RankWitnessSpec extends AnyFlatSpec with Matchers:

  /** An integer as a cyclotomic scalar, representation-agnostic (repeated addition of one). */
  private def ci(n: Int): Cyclo =
    val a = Iterator.fill(math.abs(n))(Cyclo.one).foldLeft(Cyclo.zero)(_ + _)
    if n < 0 then -a else a

  private def mat(rows: List[List[Int]]): Array[Array[Cyclo]] = rows.map(_.map(ci).toArray).toArray

  private def cols(m: Array[Array[Cyclo]]): Int = if m.isEmpty then 0 else m(0).length

  behavior of "RankWitness.produce / verify on hand-built ℚ(ζ₂₄) matrices"

  it should "witness a full-rank square matrix (empty kernel, nonsingular minor)" in:
    val m = mat(List(List(1, 2), List(3, 4)))
    val w = RankWitness.produce(m)
    RankWitness.verify(m, w) shouldBe true
    w.rank shouldBe 2
    w.kernel shouldBe empty
    RankWitness.det(RankWitness.minor(m, w)) should not be ci(0)

  it should "witness the 3×3 identity (rank 3, empty kernel)" in:
    val m = mat(List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1)))
    val w = RankWitness.produce(m)
    RankWitness.verify(m, w) shouldBe true
    w.rank shouldBe 3
    w.kernel shouldBe empty

  it should "witness a rank-deficient matrix (kernel size = cols − rank, kernel annihilated)" in:
    val m = mat(List(List(1, 2), List(2, 4))) // second row = 2× first
    val w = RankWitness.produce(m)
    RankWitness.verify(m, w) shouldBe true
    w.rank shouldBe 1
    w.kernel.size shouldBe (cols(m) - w.rank)
    w.kernel.size shouldBe 1

  it should "witness the zero matrix (rank 0, full kernel)" in:
    val m = mat(List(List(0, 0), List(0, 0)))
    val w = RankWitness.produce(m)
    RankWitness.verify(m, w) shouldBe true
    w.rank shouldBe 0
    w.kernel.size shouldBe 2

  it should "witness a wide rectangular matrix (kernel size = cols − rank)" in:
    val m = mat(List(List(1, 0, 2), List(0, 1, 3)))
    val w = RankWitness.produce(m)
    RankWitness.verify(m, w) shouldBe true
    w.rank shouldBe 2
    w.kernel.size shouldBe 1

  behavior of "RankWitness.det"

  it should "compute 1×1 and 2×2 determinants exactly over ℚ(ζ₂₄)" in:
    RankWitness.det(Vector(Vector(ci(7)))) shouldBe ci(7)
    RankWitness.det(Vector(Vector(ci(1), ci(2)), Vector(ci(3), ci(4)))) shouldBe ci(-2)

  behavior of "the verifier's teeth"

  it should "reject a witness that overstates the rank (count mismatch)" in:
    val m   = mat(List(List(1, 2), List(2, 4)))
    val w   = RankWitness.produce(m)
    val bad = w.copy(rank = w.rank + 1) // rank no longer equals pivotRows.size
    RankWitness.verify(m, bad) shouldBe false

  it should "reject a corrupted kernel vector (no longer annihilated)" in:
    val m = mat(List(List(1, 2), List(2, 4)))
    val w = RankWitness.produce(m)
    w.kernel should not be empty
    val k0  = w.kernel.head
    val bad = w.copy(kernel = k0.updated(0, k0(0) + Cyclo.one) +: w.kernel.tail)
    RankWitness.verify(m, bad) shouldBe false
