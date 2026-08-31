package io.github.scala_tessella.research_core

import scala.collection.concurrent.TrieMap

/** Exact arithmetic in ℤ[ζ_N] — the foundation of the exact de-fusion engine (`notes/saturation.md`; replaces
  * the floating-point patch-surgery scout, whose five successive precision bugs motivated the move to typed
  * exact geometry).
  *
  * Elements are integer coefficient vectors on the REDUNDANT group basis ζ⁰…ζ^{N−1}: sums and rotations are
  * coordinate-wise and exact, and no reduction happens until a ZERO TEST, which reduces modulo the monic N-th
  * cyclotomic polynomial — the single place where the ring relations enter. Plane points ARE ring elements (a
  * position is a sum of unit steps ζ^{d₁} + ζ^{d₂} + …), so unit edges are unrepresentable as anything else,
  * and point coincidence is an exact decision — the two invariants the numeric scout kept violating.
  */
object CycloRing:

  private val phiCache = TrieMap.empty[Int, Vector[BigInt]]

  /** The N-th cyclotomic polynomial Φ_N, monic, low-degree-first coefficients — by exact division
    * `x^N − 1 = Π_{d|N} Φ_d(x)`.
    */
  def cyclotomic(n: Int): Vector[BigInt] =
    require(n >= 1, s"conductor $n")
    phiCache.getOrElseUpdate(
      n, {
        def divide(a: Vector[BigInt], b: Vector[BigInt]): Vector[BigInt] =
          val rem = a.toArray
          val q   = Array.fill(a.length - b.length + 1)(BigInt(0))
          for i <- (0 to a.length - b.length).reverse do
            val f = rem(i + b.length - 1) / b.last
            q(i) = f
            for j <- b.indices do rem(i + j) -= f * b(j)
          require(rem.forall(_ == BigInt(0)), s"cyclotomic division left a remainder at n=$n")
          q.toVector
        val xn1                                                          = BigInt(-1) +: Vector.fill(n - 1)(BigInt(0)) :+ BigInt(1) // x^n − 1
        (1 until n).filter(n % _ == 0).foldLeft(xn1)((acc, d) => divide(acc, cyclotomic(d)))
      }
    )

  /** An element Σ_k c(k)·ζ_N^k of ℤ[ζ_N] on the group basis. Immutable; equality of ring VALUES is `===`
    * (reduction-aware), not case-class equality of representations.
    */
  final case class Cyc(n: Int, c: Vector[BigInt]):
    require(n >= 1 && c.length == n, s"coefficient vector must have length $n")

    def +(that: Cyc): Cyc = { require(n == that.n); Cyc(n, c.lazyZip(that.c).map(_ + _)) }
    def -(that: Cyc): Cyc = { require(n == that.n); Cyc(n, c.lazyZip(that.c).map(_ - _)) }
    def unary_- : Cyc     = Cyc(n, c.map(-_))

    /** Multiplication by ζ_N^k — a cyclic shift of the coefficients. */
    def rotated(k: Int): Cyc =
      val s = ((k % n) + n) % n
      Cyc(n, Vector.tabulate(n)(i => c(((i - s) % n + n) % n)))

    /** Ring multiplication — cyclic convolution on the group basis, exact. */
    def *(that: Cyc): Cyc =
      require(n == that.n)
      val out = Array.fill(n)(BigInt(0))
      for i <- 0 until n if c(i) != BigInt(0); j <- 0 until n if that.c(j) != BigInt(0) do
        out((i + j) % n) += c(i) * that.c(j)
      Cyc(n, out.toVector)

    /** Complex conjugation ζ^k ↦ ζ^{−k} — exact. */
    def conj: Cyc = Cyc(n, Vector.tabulate(n)(i => c((n - i) % n)))

    /** Integer scalar multiple — exact. */
    def scaled(k: BigInt): Cyc = Cyc(n, c.map(_ * k))

    /** Sign of the real part, EXACT: zero is decided algebraically (Re x = 0 ⟺ x = −x̄); a nonzero sign is
      * read numerically only OUTSIDE a certified error bound, and the indeterminate window fails loudly
      * instead of guessing — the anti-scout guardrail.
      */
    def reSign: Int =
      if (this + conj).isZero then 0 else certifiedSign(approx._1)

    /** Sign of the imaginary part, EXACT (Im x = 0 ⟺ x = x̄); same contract as `reSign`. */
    def imSign: Int =
      if (this - conj).isZero then 0 else certifiedSign(approx._2)

    private def certifiedSign(v: Double): Int =
      val bound = c.iterator.map(_.abs).sum.toDouble * 1e-9
      require(math.abs(v) > bound, s"sign undecided at double precision: $v within ±$bound")
      if v > 0 then 1 else -1

    /** True iff this element is 0 in ℚ(ζ_N): the coefficient polynomial reduces to 0 mod Φ_N. */
    def isZero: Boolean =
      val phi = cyclotomic(n)
      val rem = c.toArray
      for i <- (0 to rem.length - phi.length).reverse do
        val f = rem(i + phi.length - 1)
        if f != BigInt(0) then for j <- phi.indices do rem(i + j) -= f * phi(j)
      rem.forall(_ == BigInt(0))

    /** Exact ring-value equality. */
    def ===(that: Cyc): Boolean = (this - that).isZero

    /** The canonical representative: coefficients reduced modulo Φ_N (degree < φ(N)), padded back to length N
      * — equal ring values reduce identically, so this is a HASHABLE KEY.
      */
    def reducedKey: Vector[BigInt] =
      val phi = cyclotomic(n)
      val rem = c.toArray
      for i <- (0 to rem.length - phi.length).reverse do
        val f = rem(i + phi.length - 1)
        if f != BigInt(0) then for j <- phi.indices do rem(i + j) -= f * phi(j)
      rem.toVector

    /** Numeric value — for RADIUS BOUNDS and rendering only, never for decisions. */
    def approx: (Double, Double) =
      var (x, y) = (0.0, 0.0)
      for k <- 0 until n if c(k) != BigInt(0) do
        val a = 2 * math.Pi * k / n
        x += c(k).toDouble * math.cos(a)
        y += c(k).toDouble * math.sin(a)
      (x, y)

  object Cyc:
    def zero(n: Int): Cyc = Cyc(n, Vector.fill(n)(BigInt(0)))

    /** ζ_N^k — in the plane reading, the unit step in direction 2πk/N. */
    def root(n: Int, k: Int): Cyc                  = zero(n).copy(c = zero(n).c.updated(((k % n) + n) % n, BigInt(1)))
    def sum(n: Int, elems: IterableOnce[Cyc]): Cyc = elems.iterator.foldLeft(zero(n))(_ + _)
