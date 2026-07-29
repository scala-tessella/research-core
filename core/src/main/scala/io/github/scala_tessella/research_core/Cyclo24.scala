package io.github.scala_tessella.research_core

/** ADR-0009 paper certification, track B — exact arithmetic in the cyclotomic field ℚ(ζ₂₄), ζ = e^{iπ/12}:
  * basis 1, ζ, …, ζ⁷ over ℚ with ζ⁸ = ζ⁴ − 1 (the 24th cyclotomic polynomial is x⁸ − x⁴ + 1). Coefficients
  * are BIG-INTEGER rationals, so Gaussian elimination can never overflow. The point: at the regular point
  * every closure-Jacobian entry is a sum of sines and cosines of integer multiples of π/12 (faces are
  * {3,4,6,8,12}-gons), i.e. lives in this field — `cos kπ/12 = (ζᵏ + ζ⁻ᵏ)/2`, `sin kπ/12 = (ζᵏ − ζ⁻ᵏ)(−ζ⁶)/2`
  * (since i = ζ⁶) — so closure ranks become EXACT field computations.
  */
object Cyclo24:

  /** Normalized big-integer rational. */
  final case class Rat(num: BigInt, den: BigInt):
    def +(o: Rat): Rat   = Rat.make(num * o.den + o.num * den, den * o.den)
    def -(o: Rat): Rat   = Rat.make(num * o.den - o.num * den, den * o.den)
    def *(o: Rat): Rat   = Rat.make(num * o.num, den * o.den)
    def /(o: Rat): Rat   = Rat.make(num * o.den, den * o.num)
    def unary_- : Rat    = Rat(-num, den)
    def isZero: Boolean  = num == 0
    def toDouble: Double = num.toDouble / den.toDouble

  object Rat:
    val zero: Rat                       = Rat(0, 1)
    val one: Rat                        = Rat(1, 1)
    def make(n: BigInt, d: BigInt): Rat =
      require(d != 0, "zero denominator")
      val s = if d < 0 then -1 else 1
      val g = n.gcd(d)
      if g == 0 then zero else Rat(s * n / g, s * d / g)
    def apply(f: Frac): Rat             = make(BigInt(f.num), BigInt(f.den))

  /** An element of ℚ(ζ₂₄) as coefficients of 1, ζ, …, ζ⁷. */
  final case class Cyclo(c: Vector[Rat]):
    def +(o: Cyclo): Cyclo   = Cyclo(c.lazyZip(o.c).map(_ + _))
    def -(o: Cyclo): Cyclo   = Cyclo(c.lazyZip(o.c).map(_ - _))
    def unary_- : Cyclo      = Cyclo(c.map(-_))
    def scale(r: Rat): Cyclo = Cyclo(c.map(_ * r))
    def isZero: Boolean      = c.forall(_.isZero)

    def *(o: Cyclo): Cyclo =
      val raw = Array.fill(15)(Rat.zero)
      for i <- 0 until 8; j <- 0 until 8 if !c(i).isZero && !o.c(j).isZero do
        raw(i + j) = raw(i + j) + c(i) * o.c(j)
      // reduce ζ^{8+t} = ζ^{4+t} − ζ^t, from the top down
      for k <- 14 to 8 by -1 do
        if !raw(k).isZero then
          raw(k - 4) = raw(k - 4) + raw(k)
          raw(k - 8) = raw(k - 8) - raw(k)
          raw(k) = Rat.zero
      Cyclo(Vector.tabulate(8)(raw(_)))

    /** Field inverse: solve the 8×8 rational system `this · x = 1` over the power basis. */
    def inverse: Cyclo =
      require(!isZero, "inverse of zero")
      // column t = coordinates of this · ζ^t
      val cols = Vector.tabulate(8)(t => (this * Cyclo.zeta(t)).c)
      val m    = Array.tabulate(8)(r =>
        Array.tabulate(9)(cc => if cc < 8 then cols(cc)(r) else if r == 0 then Rat.one else Rat.zero)
      )
      var row  = 0
      for col <- 0 until 8 do
        var pr  = row
        while pr < 8 && m(pr)(col).isZero do pr += 1
        require(pr < 8, "singular multiplication matrix — impossible in a field")
        val t   = m(pr); m(pr) = m(row); m(row) = t
        val inv = m(row)(col)
        for j <- col to 8 do m(row)(j) = m(row)(j) / inv
        for i <- 0 until 8 if i != row && !m(i)(col).isZero do
          val f = m(i)(col)
          for j <- col to 8 do m(i)(j) = m(i)(j) - f * m(row)(j)
        row += 1
      Cyclo(Vector.tabulate(8)(r => m(r)(8)))

    /** Numeric evaluation at ζ = e^{iπ/12} — sanity checks only. */
    def toComplex: (Double, Double) =
      var (re, im) = (0.0, 0.0)
      for t <- 0 until 8 do
        re += c(t).toDouble * math.cos(t * math.Pi / 12)
        im += c(t).toDouble * math.sin(t * math.Pi / 12)
      (re, im)

  object Cyclo:
    val zero: Cyclo = Cyclo(Vector.fill(8)(Rat.zero))
    val one: Cyclo  = zeta(0)

    /** ζ^k, k reduced mod 24 then through ζ⁸ = ζ⁴ − 1 (and ζ¹² = −1 follows). */
    def zeta(k: Int): Cyclo =
      val kk  = ((k % 24) + 24) % 24
      val raw = Array.fill(24)(Rat.zero)
      raw(kk) = Rat.one
      for t <- 23 to 8 by -1 do
        if !raw(t).isZero then
          raw(t - 4) = raw(t - 4) + raw(t)
          raw(t - 8) = raw(t - 8) - raw(t)
          raw(t) = Rat.zero
      Cyclo(Vector.tabulate(8)(raw(_)))

    private val half = Rat.make(1, 2)

    /** cos(kπ/12), exactly. */
    val cosPi12: Int => Cyclo = memo(k => (zeta(k) + zeta(-k)).scale(half))

    /** sin(kπ/12), exactly: (ζᵏ − ζ⁻ᵏ)/(2i) with 1/i = −i = −ζ⁶. */
    val sinPi12: Int => Cyclo                       = memo(k => ((zeta(k) - zeta(-k)) * (-zeta(6))).scale(half))
    private def memo(f: Int => Cyclo): Int => Cyclo =
      val table = Vector.tabulate(24)(f)
      k => table(((k % 24) + 24) % 24)

  /** Rank of a matrix over ℚ(ζ₂₄) — exact Gaussian elimination, exact zero tests. */
  def rank(m0: Array[Array[Cyclo]]): Int =
    if m0.isEmpty then return 0
    val m    = m0.map(_.clone)
    val rows = m.length
    val cols = m(0).length
    var rank = 0
    var col  = 0
    while col < cols && rank < rows do
      var pr = rank
      while pr < rows && m(pr)(col).isZero do pr += 1
      if pr < rows then
        val t   = m(pr); m(pr) = m(rank); m(rank) = t
        val inv = m(rank)(col).inverse
        for j <- col until cols do m(rank)(j) = m(rank)(j) * inv
        for i <- rank + 1 until rows if !m(i)(col).isZero do
          val f = m(i)(col)
          for j <- col until cols do m(i)(j) = m(i)(j) - f * m(rank)(j)
        rank += 1
      col += 1
    rank
