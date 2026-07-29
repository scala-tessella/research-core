package io.github.scala_tessella.uniform_tilings

/** Exact rationals for curvature / angle arithmetic (extracted from the Delaney machinery). */
final case class Frac(num: Long, den: Long):
  def +(o: Frac): Frac = Frac.make(num * o.den + o.num * den, den * o.den)
  def -(o: Frac): Frac = Frac.make(num * o.den - o.num * den, den * o.den)
  def *(o: Frac): Frac = Frac.make(num * o.num, den * o.den)
  def /(o: Frac): Frac =
    require(!o.isZero, s"Frac: division by zero ($this / $o)")
    Frac.make(num * o.den, den * o.num)
  def signum: Int      = java.lang.Long.signum(num) * java.lang.Long.signum(den)
  def isZero: Boolean  = num == 0
  def toDouble: Double = num.toDouble / den

object Frac:
  def make(n: Long, d: Long): Frac =
    require(d != 0, s"Frac: zero denominator ($n/$d)")
    val s = if d < 0 then -1 else 1
    val g = gcd(math.abs(n), math.abs(d))
    Frac(s * n / g, s * d / g)

  private def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
