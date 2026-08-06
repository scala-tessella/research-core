package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Cyclo24.{Cyclo, Rat}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The ℚ(ζ₂₄) arithmetic of [[Cyclo24]] as a FIELD, by property: `ExactRankSpec` pins the defining relation
  * and the trig identities at the 24 lattice points, but `Rat` arithmetic, general products and inverses were
  * previously asserted on single hand elements only. Random elements here have small coefficients (the axioms
  * are representation-level facts; BigInt makes magnitude irrelevant anyway).
  */
class Cyclo24Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  // 100 cases per property instead of ScalaTest's default 10 — the Cyclo generator is 8-dimensional
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val rats: Gen[Rat] =
    for
      n <- Gen.choose(-20, 20)
      d <- Gen.choose(1, 20)
    yield Rat.make(n, d)

  private val cyclos: Gen[Cyclo] =
    Gen.containerOfN[Vector, Rat](8, rats).map(Cyclo(_))

  private val nonZeroCyclos: Gen[Cyclo] = cyclos.filterNot(_.isZero)

  behavior of "Rat (normalized big-integer rationals)"

  it should "reject a zero denominator" in:
    an[IllegalArgumentException] should be thrownBy Rat.make(1, 0)

  it should "satisfy ring identities and division as multiplicative inverse" in:
    forAll(rats, rats, rats) { (a, b, c) =>
      a + b shouldBe b + a
      a * b shouldBe b * a
      (a + b) + c shouldBe a + (b + c)
      a * (b + c) shouldBe a * b + a * c
      (a + b) - b shouldBe a
      a + -a shouldBe Rat.zero
      if !b.isZero then (a / b) * b shouldBe a
    }

  behavior of "Cyclo (ℚ(ζ₂₄) in the power basis 1, ζ, …, ζ⁷)"

  it should "be a commutative ring" in:
    forAll(cyclos, cyclos, cyclos) { (a, b, c) =>
      a + b shouldBe b + a
      a * b shouldBe b * a
      (a + b) + c shouldBe a + (b + c)
      (a * b) * c shouldBe a * (b * c)
      a * (b + c) shouldBe a * b + a * c
      a + -a shouldBe Cyclo.zero
      a * Cyclo.one shouldBe a
      a * Cyclo.zero shouldBe Cyclo.zero
    }

  it should "invert every nonzero element (a · a⁻¹ = 1)" in:
    forAll(nonZeroCyclos) { a =>
      a * a.inverse shouldBe Cyclo.one
    }

  it should "reject inverting zero" in:
    an[IllegalArgumentException] should be thrownBy Cyclo.zero.inverse

  it should "agree with the numeric embedding at ζ = e^{iπ/12}" in:
    forAll(cyclos, cyclos) { (a, b) =>
      val (re, im) = (a * b).toComplex
      val (ra, ia) = a.toComplex
      val (rb, ib) = b.toComplex
      re shouldBe (ra * rb - ia * ib) +- 1e-6
      im shouldBe (ra * ib + ia * rb) +- 1e-6
    }

  behavior of "Cyclo24.rank"

  it should "return 0 on an empty matrix" in:
    Cyclo24.rank(Array.empty) shouldBe 0

  it should "compute textbook ranks over the field" in:
    val z    = Cyclo.zeta
    // rows 2 and 3 are ζ²·row1 and (1+ζ²)·row1: all proportional over the field
    val row  = Array(z(1), z(5), Cyclo.one)
    val m    = Array(row, row.map(_ * z(2)), row.map(x => x + x * z(2)))
    Cyclo24.rank(m) shouldBe 1
    val diag = Array.tabulate(3, 3)((i, j) => if i == j then z(i + 1) else Cyclo.zero)
    Cyclo24.rank(diag) shouldBe 3
