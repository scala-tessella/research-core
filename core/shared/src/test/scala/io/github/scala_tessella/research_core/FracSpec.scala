package io.github.scala_tessella.research_core

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** [[Frac]] underpins the whole exact linear layer (curvature accounting in [[DelaneySymbols]], the angle
  * systems and RREF of [[MetricLayer]], the pinned rows of [[UClass]]), so its arithmetic is pinned here
  * directly rather than only through those consumers. Denominator-zero values are unrepresentable by
  * construction: `make` requires a nonzero denominator and `/` a nonzero divisor — previously `make(n, 0)`
  * silently produced `Frac(±1, 0)`, which would corrupt the linear algebra without ever throwing.
  */
class FracSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  // 100 cases per property instead of ScalaTest's default 10 — milliseconds on these generators
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  // small operands: the field axioms are exercised well inside Long overflow territory
  private val fracs: Gen[Frac] =
    for
      n <- Gen.choose(-100L, 100L)
      d <- Gen.choose(1L, 100L)
    yield Frac.make(n, d)

  private val nonZeroFracs: Gen[Frac] = fracs.filterNot(_.isZero)

  behavior of "Frac.make"

  it should "reject a zero denominator" in:
    an[IllegalArgumentException] should be thrownBy Frac.make(1, 0)
    an[IllegalArgumentException] should be thrownBy Frac.make(-7, 0)
    an[IllegalArgumentException] should be thrownBy Frac.make(0, 0)

  it should "always produce a reduced form with positive denominator" in:
    forAll(Gen.choose(-1000L, 1000L), Gen.choose(-1000L, 1000L).filterNot(_ == 0)) { (n, d) =>
      val f = Frac.make(n, d)
      f.den should be > 0L
      f shouldBe Frac.make(f.num, f.den) // already reduced: renormalizing is the identity
    }

  behavior of "Frac./"

  it should "reject division by zero" in:
    an[IllegalArgumentException] should be thrownBy (Frac.make(1, 2) / Frac.make(0, 1))

  it should "invert multiplication" in:
    forAll(fracs, nonZeroFracs) { (a, b) =>
      (a / b) * b shouldBe a
    }

  behavior of "Frac arithmetic (field axioms on reduced representatives)"

  it should "be commutative and associative in + and *" in:
    forAll(fracs, fracs, fracs) { (a, b, c) =>
      a + b shouldBe b + a
      a * b shouldBe b * a
      (a + b) + c shouldBe a + (b + c)
      (a * b) * c shouldBe a * (b * c)
    }

  it should "distribute * over +" in:
    forAll(fracs, fracs, fracs) { (a, b, c) =>
      a * (b + c) shouldBe a * b + a * c
    }

  it should "agree with subtraction as inverse of addition" in:
    forAll(fracs, fracs) { (a, b) =>
      (a + b) - b shouldBe a
      (a - b).isZero shouldBe (a == b)
    }

  behavior of "Frac.signum and Frac.toDouble"

  it should "match the sign and value of the Double quotient" in:
    forAll(fracs) { a =>
      a.signum shouldBe math.signum(a.toDouble).toInt
      a.toDouble shouldBe (a.num.toDouble / a.den) +- 1e-15
    }
