package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-method teeth for [[CycloRing]], the exact ℤ[ζ_N] foundation of the de-fusion engine. */
class CycloRingSpec extends AnyFlatSpec with Matchers:

  "cyclotomic" should "produce the known small polynomials" in:
    cyclotomic(1) shouldBe Vector(BigInt(-1), BigInt(1))            // x − 1
    cyclotomic(2) shouldBe Vector(BigInt(1), BigInt(1))             // x + 1
    cyclotomic(3) shouldBe Vector(BigInt(1), BigInt(1), BigInt(1))  // x² + x + 1
    cyclotomic(4) shouldBe Vector(BigInt(1), BigInt(0), BigInt(1))  // x² + 1
    cyclotomic(6) shouldBe Vector(BigInt(1), BigInt(-1), BigInt(1)) // x² − x + 1
    cyclotomic(12) shouldBe Vector(1, 0, -1, 0, 1).map(BigInt(_)) // x⁴ − x² + 1

  it should "have degree φ(n)" in:
    def phi(n: Int) = (1 to n).count(k => BigInt(k).gcd(BigInt(n)) == 1)
    for n <- 1 to 42 do cyclotomic(n).length - 1 shouldBe phi(n)

  "Cyc.isZero" should "certify vanishing sums of roots of unity" in:
    for n <- List(3, 5, 6, 7, 12, 30, 42) do
      withClue(s"all $n-th roots: ")(Cyc.sum(n, (0 until n).map(Cyc.root(n, _))).isZero shouldBe true)
    // 1 + ζ₆² + ζ₆⁴ = 0 (the cube roots inside the 6-ring)
    Cyc.sum(6, List(0, 2, 4).map(Cyc.root(6, _))).isZero shouldBe true
    // opposite roots cancel
    (Cyc.root(12, 1) + Cyc.root(12, 7)).isZero shouldBe true

  it should "reject non-vanishing sums" in:
    (Cyc.root(5, 0) + Cyc.root(5, 1)).isZero shouldBe false
    Cyc.root(7, 3).isZero shouldBe false
    Cyc.sum(12, List(0, 2, 4).map(Cyc.root(12, _))).isZero shouldBe false // 1 + ζ₆ + ζ₃ ≠ 0

  "===" should "identify equal ring values across representations" in:
    // ζ₆³ = −1
    Cyc.root(6, 3) === -Cyc.root(6, 0) shouldBe true
    // ζ₆ − ζ₆² = 1 (since ζ₆² = ζ₆ − 1)
    (Cyc.root(6, 1) - Cyc.root(6, 2)) === Cyc.root(6, 0) shouldBe true
    // distinct roots are distinct values
    Cyc.root(12, 1) === Cyc.root(12, 2) shouldBe false

  "rotated" should "be multiplication by ζ^k" in:
    val a = Cyc.root(12, 3) + Cyc.root(12, 7)
    a.rotated(12) === a shouldBe true
    a.rotated(5) === (Cyc.root(12, 8) + Cyc.root(12, 0)) shouldBe true
    a.rotated(-3) === (Cyc.root(12, 0) + Cyc.root(12, 4)) shouldBe true

  "ring ops" should "commute and associate on samples" in:
    val (x, y, z) = (Cyc.root(30, 4), Cyc.root(30, 17) + Cyc.root(30, 2), -Cyc.root(30, 29))
    (x + y) === (y + x) shouldBe true
    ((x + y) + z) === (x + (y + z)) shouldBe true
    (x - x).isZero shouldBe true

  "*" should "multiply exactly on the group basis" in:
    (Cyc.root(12, 3) * Cyc.root(12, 4)) === Cyc.root(12, 7) shouldBe true
    (Cyc.root(12, 7) * Cyc.root(12, 8)) === Cyc.root(12, 3) shouldBe true // wraps mod N
    val a = Cyc.root(30, 2) + Cyc.root(30, 11)
    (a * Cyc.zero(30)).isZero shouldBe true
    (a * Cyc.root(30, 0)) === a shouldBe true

  "scaled" should "be the exact integer multiple" in:
    Cyc.root(12, 5).scaled(3) === (Cyc.root(12, 5) + Cyc.root(12, 5) + Cyc.root(12, 5)) shouldBe true
    Cyc.root(12, 5).scaled(0).isZero shouldBe true
    Cyc.root(12, 5).scaled(-1) === -Cyc.root(12, 5) shouldBe true

  "conj" should "invert the root exponents and pair with * as the norm" in:
    Cyc.root(12, 5).conj === Cyc.root(12, 7) shouldBe true
    (Cyc.root(42, 13) * Cyc.root(42, 13).conj) === Cyc.root(42, 0) shouldBe true // |ζ|² = 1
    Cyc.zero(6).conj.isZero shouldBe true

  "reSign and imSign" should "decide zero algebraically and nonzero with certification" in:
    Cyc.root(12, 3).reSign shouldBe 0 // cos 90° = 0, decided by x = −x̄, not numerically
    Cyc.root(12, 3).imSign shouldBe 1 // sin 90° = 1
    Cyc.root(12, 9).imSign shouldBe -1 // sin 270°
    Cyc.root(12, 5).reSign shouldBe -1 // cos 150°
    val zeroValue = Cyc.root(12, 0) + Cyc.root(12, 6) // 1 + (−1): zero VALUE, nonzero vector
    zeroValue.reSign shouldBe 0
    zeroValue.imSign shouldBe 0
    (Cyc.root(12, 1) - Cyc.root(12, 11)).reSign shouldBe 0 // ζ − ζ̄ = 2i·sin 30° is pure imaginary
    (Cyc.root(12, 1) - Cyc.root(12, 11)).imSign shouldBe 1
