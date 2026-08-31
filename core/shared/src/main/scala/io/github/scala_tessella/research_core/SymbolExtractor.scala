package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.TilePatch.{PlacedTile, State}

/** PERIODIC PATCH → DELANEY SYMBOL: the bridge from sampled endpoints back to the campaign's symbol-level
  * machinery.
  *
  * [[torusSymbol]] builds the symbol of the TRANSLATION quotient: chambers are (tile class, corner, side)
  * triples of one fully-checkable representative per class, σ₀/σ₁ are index arithmetic within a tile, σ₂ is
  * an exact directed-edge lookup in the patch, and everything is sanity-checked (total involutions) before a
  * symbol is returned. [[minimalImage]] then descends through proper quotients — by Delaney–Dress covering
  * theory the result is the symbol of the tiling modulo its FULL symmetry group, so its size is the true
  * chamber count C and its 12-orbit count the true uniformity k. Isomorphism = equality of `canonicalKey`s of
  * minimal images.
  */
object SymbolExtractor:

  /** Translation classes of the canonicalised placements, with the instance → class map. */
  private def classesAndIndex(
      state: State,
      t1: Cyc,
      t2: Cyc
  ): (Vector[Vector[PlacedTile]], Array[Int]) =
    val eq    = Periodicity.latticeEquiv(t1, t2)
    val canon = state.tiles.map(Periodicity.canonicalPlacement)
    val parts = canon.zipWithIndex
      .groupBy(_._1.poly.dirs)
      .toVector
      .sortBy(_._1.mkString(","))
      .flatMap((_, ps) => Periodicity.partitionBy(ps)((a, b) => eq(a._1.anchor, b._1.anchor)))
    val cls   = Array.fill(canon.length)(-1)
    for (g, ci) <- parts.zipWithIndex; (_, i) <- g do cls(i) = ci
    (parts.map(_.map(_._1)), cls)

  /** The Delaney symbol of the translation quotient, or None if some class lacks a representative with every
    * vertex checkable, a neighbour lookup escapes the patch, or the assembled involutions fail their sanity
    * check.
    */
  def torusSymbol(state: State, t1: Cyc, t2: Cyc): Option[DSymbol] =
    val (classes, cls) = classesAndIndex(state, t1, t2)
    val interior       = state.interiorWords.keySet
    val chained        = state.cornersByVertex.collect {
      case (v, cs) if cs.map(_.angle).sum == state.n && TilePatch.vertexChained(cs, state.n) => v
    }.toSet
    val reps           = classes.map: g =>
      g.filter(_.vertices.forall(v => interior(v.reducedKey) && chained(v.reducedKey)))
        .sortBy { pt =>
          val (x, y) = pt.anchor.approx; x * x + y * y
        }
        .headOption
    if reps.exists(_.isEmpty) then None
    else
      val rep                                = reps.map(_.get)
      val canon                              = state.tiles.map(Periodicity.canonicalPlacement)
      val edgeMap                            = (for
        (pt, ti) <- canon.zipWithIndex
        (v, k)   <- pt.vertices.zipWithIndex
      yield (v.reducedKey, pt.poly.dirs(k)) -> (ti, k)).toMap
      val p                                  = rep.map(_.poly.dirs.length)
      val base                               = p.scanLeft(0)((acc, pc) => acc + 2 * pc)
      val total                              = base.last
      def ch(c: Int, k: Int, side: Int): Int = base(c) + 2 * k + side + 1
      val op                                 = Array.ofDim[Int](total + 1, 3)
      val m01                                = Array.ofDim[Int](total + 1)
      val m12                                = Array.ofDim[Int](total + 1)
      var ok                                 = true
      for c <- rep.indices; k <- 0 until p(c) if ok do
        val vs   = rep(c).vertices
        val head = vs((k + 1) % p(c))
        for side <- 0 to 1 do
          val d = ch(c, k, side)
          op(d)(0) = ch(c, k, 1 - side)
          op(d)(1) = if side == 0 then ch(c, (k - 1 + p(c)) % p(c), 1) else ch(c, (k + 1) % p(c), 0)
          m01(d) = p(c)
        m12(ch(c, k, 0)) = state.cornersByVertex(vs(k).reducedKey).length
        m12(ch(c, k, 1)) = state.cornersByVertex(head.reducedKey).length
        edgeMap.get((head.reducedKey, (rep(c).poly.dirs(k) + state.n / 2) % state.n)) match
          case None          => ok = false
          case Some((ti, j)) =>
            op(ch(c, k, 0))(2) = ch(cls(ti), j, 1)
            op(ch(c, k, 1))(2) = ch(cls(ti), j, 0)
      val sane                               =
        ok && (1 to total).forall(d => (0 to 2).forall(i => op(d)(i) >= 1 && op(op(d)(i))(i) == d))
      Option.when(sane)(assemble(op, m01, m12))

  /** Build a DSymbol from op + per-chamber m values (v = m / orbit period, checked). */
  private def assemble(op: Array[Array[Int]], m01: Array[Int], m12: Array[Int]): DSymbol =
    val dset          = new DSet(op)
    val (orbs, index) = DelaneySymbols.collectOrbits(dset)
    val vs            = Array.tabulate(orbs.length): k =>
      val o = orbs(k)
      val d = o.elements.head
      val m = if o.i == 0 then m01(d) else m12(d)
      require(m % o.r == 0, s"orbit period ${o.r} does not divide m=$m")
      m / o.r
    new DSymbol(dset, orbs, index, vs)

  /** The minimal image — descend through proper quotients ([[DelaneySymbols.properQuotients]], the finest
    * m-constant op-congruences) until none remain. Any nontrivial covering admits the minimal-covering
    * congruence as a proper quotient, so greedy descent terminates at the Delaney symbol of the tiling modulo
    * its FULL symmetry group: its size is the true C, its 12-orbit count the true k.
    */
  @annotation.tailrec
  def minimalImage(ds: DSymbol): DSymbol =
    DelaneySymbols.properQuotients(ds) match
      case q :: _ => minimalImage(q)
      case Nil    => ds

  /** Serialise a symbol in the campaign's entry-key format (`σ0,σ1,σ2,m01|m12` per chamber, `;`-separated) —
    * the inverse of the probes' `symbolFromKey`.
    */
  def entryKey(ds: DSymbol): String = (1 to ds.size)
    .map(d => s"${ds.get(0, d)},${ds.get(1, d)},${ds.get(2, d)},${ds.m(0, 1, d)}|${ds.m(1, 2, d)}")
    .mkString(";")

  /** The full-symmetry profile of a certified periodic state: (C, k, canonical key). */
  def profile(state: State, t1: Cyc, t2: Cyc): Option[(Int, Int, String)] =
    torusSymbol(state, t1, t2).map: torus =>
      import DelaneySymbols.canonicalKey
      val min = minimalImage(torus)
      (min.size, min.orbs.count(_.i == 1), min.canonicalKey)
