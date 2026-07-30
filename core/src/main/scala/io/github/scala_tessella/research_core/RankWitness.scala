package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.Cyclo24.Cyclo

/** ADR-0009 paper certification, track D — algebraic RANK WITNESSES: a rank-r verdict is certified by
  *
  *   - an r×r PIVOT MINOR with nonzero determinant (rank ≥ r), and
  *   - an explicit KERNEL BASIS of size cols − r with unit-coordinate structure — each vector is 1 at its own
  *     free column and 0 at the others, so independence is visible by inspection (rank ≤ r).
  *
  * The producer is the usual elimination; the VERIFIER shares no algorithm with it: determinants by Laplace
  * cofactor expansion, kernel membership by direct matrix–vector products. Everything is over ℚ(ζ₂₄)
  * ([[Cyclo24]]), which contains ℚ — so one engine covers both the rational angle systems and the cyclotomic
  * closure Jacobians. Witnesses are small enough to export and re-verify with any CAS.
  */
object RankWitness:

  final case class Witness(
      rank: Int,
      pivotRows: Vector[Int],
      pivotCols: Vector[Int],
      freeCols: Vector[Int],
      kernel: Vector[Vector[Cyclo]]
  )

  /** Elimination WITH bookkeeping: original pivot row/column indices and the RREF kernel basis. */
  def produce(m0: Array[Array[Cyclo]]): Witness =
    val rows       = m0.length
    val cols       = if rows == 0 then 0 else m0(0).length
    val m          = m0.map(_.clone)
    val perm       = Array.tabulate(rows)(identity) // original index of each working row
    val pivotOfCol = Array.fill(cols)(-1)
    val pivotRows  = Vector.newBuilder[Int]
    val pivotCols  = Vector.newBuilder[Int]
    var r          = 0
    var c          = 0
    while r < rows && c < cols do
      var pr = r
      while pr < rows && m(pr)(c).isZero do pr += 1
      if pr < rows then
        val t   = m(pr); m(pr) = m(r); m(r) = t
        val tp  = perm(pr); perm(pr) = perm(r); perm(r) = tp
        val inv = m(r)(c).inverse
        for j <- c until cols do m(r)(j) = m(r)(j) * inv
        for i <- 0 until rows if i != r && !m(i)(c).isZero do
          val f = m(i)(c)
          for j <- c until cols do m(i)(j) = m(i)(j) - f * m(r)(j)
        pivotOfCol(c) = r
        pivotRows += perm(r)
        pivotCols += c
        r += 1
      c += 1
    val free       = (0 until cols).filter(pivotOfCol(_) < 0).toVector
    val kernel     = free.map: f =>
      Vector.tabulate(cols): j =>
        if j == f then Cyclo.one
        else if pivotOfCol(j) >= 0 then -m(pivotOfCol(j))(f)
        else Cyclo.zero
    Witness(r, pivotRows.result(), pivotCols.result(), free, kernel)

  /** Laplace cofactor expansion — deliberately NOT elimination; fine at witness sizes (r ≤ ~8). */
  def det(a: Vector[Vector[Cyclo]]): Cyclo =
    val n = a.length
    if n == 0 then Cyclo.one
    else if n == 1 then a(0)(0)
    else
      var acc = Cyclo.zero
      for j <- 0 until n if !a(0)(j).isZero do
        val minor = a.drop(1).map(row => row.take(j) ++ row.drop(j + 1))
        val term  = a(0)(j) * det(minor)
        acc = if j % 2 == 0 then acc + term else acc - term
      acc

  /** The witness's pivot minor of the ORIGINAL matrix. */
  def minor(m: Array[Array[Cyclo]], w: Witness): Vector[Vector[Cyclo]] =
    w.pivotRows.map(r => w.pivotCols.map(c => m(r)(c)))

  /** Independent verification: minor nonsingular (cofactor determinant, plus a numeric evaluation
    * double-check), kernel vectors annihilated by every row (direct products), unit-coordinate structure, and
    * the counts adding up. Shares no elimination with [[produce]].
    */
  def verify(m: Array[Array[Cyclo]], w: Witness): Boolean =
    val rows     = m.length
    val cols     = if rows == 0 then 0 else m(0).length
    val counts   =
      w.rank == w.pivotRows.size && w.rank == w.pivotCols.size &&
        w.freeCols.size == cols - w.rank && w.kernel.size == cols - w.rank &&
        (w.pivotCols ++ w.freeCols).sorted ==
        (0 until cols).toVector &&
        w.pivotRows.distinct.size == w.rank && w.pivotRows.forall(r => r >= 0 && r < rows)
    if !counts then return false
    val d        = det(minor(m, w))
    if d.isZero then return false
    val (re, im) = d.toComplex
    if math.hypot(re, im) < 1e-12 then return false // numeric double-check of nonzeroness
    w.kernel.zipWithIndex.forall { (k, ki) =>
      val unitStructure = w.freeCols.zipWithIndex.forall((f, fi) =>
        if fi == ki then (k(f) - Cyclo.one).isZero else k(f).isZero
      )
      unitStructure && m.forall { row =>
        var acc = Cyclo.zero
        for j <- 0 until cols do if !row(j).isZero && !k(j).isZero then acc = acc + row(j) * k(j)
        acc.isZero
      }
    }
