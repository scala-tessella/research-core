package io.github.scala_tessella.research_core

import MonoShell.{ringDescriptors, Vec}
import PairPatterns.{KCtx, KPattern}
import SpeciesEnumerator.species
import StarChambers.Chamber
import StarFoldings.{fold, symmetryOf}
import Sigma0Assembly.unionOf
import SymbolCatalog.{symOf, Sym}
import TransitivePatterns.{Iso, Mat}

/** The REALIZATION side of the k >= 2 symbol identification — the minimal symbol of a certified PAIR (and
  * k-ary) honeycomb, derived from its certified pattern, for key-for-key comparison with the combinatorial
  * census.
  *
  * The k = 1 half of this derivation is [[io.github.scala_tessella.research_core.SymbolRealization]]; what
  * stays here is everything typed on the local [[PairPatterns]] `KCtx` / `KPattern`, which the library has no
  * counterpart for: the pair ball's stabilizer, the per-role pullback tables, and the k-ary sigma0.
  */
object PairRealization:

  // germ comparison, as in the library's k = 1 derivation. Kept here rather than taken from
  // `research_core.SymbolRealization`, where it is private: an epsilon test on a pair of direction vectors is
  // an internal detail of that derivation, not surface a library should promise.
  private def vDist(a: Vec, b: Vec): Double =
    math.sqrt(
      (a._1 - b._1) *
        (a._1 - b._1) +
        (a._2 - b._2) *
        (a._2 - b._2) +
        (a._3 - b._3) *
        (a._3 - b._3)
    )

  private def germEq(a: (Vec, Vec), b: (Vec, Vec)): Boolean =
    val dn = math.min(vDist(a._1, b._1), vDist(a._1, (-b._1._1, -b._1._2, -b._1._3)))
    dn < 1e-4 && vDist(a._2, b._2) < 1e-4

  // ---------- the k = 2 side: the minimal symbol of a certified pair honeycomb ----------

  /** Does the star symmetry s of role r preserve the pair ball (positions, SPECIES ROLES, and Stab-compatible
    * frames)? The pair transposition of `preservesBall`.
    */
  def preservesPairBall(
      ctx: KCtx,
      r: Int,
      s: Mat,
      ball: Vector[(Int, Iso)]
  ): Boolean =
    val index = ball.map((br, t) => TransitivePatterns.round4(t.t) -> (br, t)).toMap
    ball.forall { (br, t) =>
      index.get(TransitivePatterns.round4(s(t.t))) match
        case None           => false
        case Some((r2, t2)) =>
          r2 == br && {
            val m = s * t.m
            t2.m.dist(m) < 1e-3 || ctx.stab(br).exists(h => (t2.m.t * m).dist(h) < 1e-3)
          }
    }

  /** Per-role complex/ring/descriptor tables for the k-role pullback, computed once per ctx. `offUn` gives
    * the unfolded-union offsets of the k parts (k + 1 entries, total last); `roleAt` inverts them.
    */
  final case class PairTables(
      cxs: Vector[StarChambers.StarComplex],
      sts: Vector[SpeciesEnumerator.Species],
      ringsR: Vector[Map[Int, Vector[(Int, (Int, Int), (Int, Int))]]],
      descsR: Vector[Map[Int, Vector[(Int, Vector[(Vec, Vec)])]]]
  ):
    val offUn: Vector[Int]  = cxs.map(_.chambers.size).scanLeft(0)(_ + _)
    def roleAt(j: Int): Int = offUn.lastIndexWhere(_ <= j, offUn.size - 2)

  private val tablesCache = collection.mutable.Map.empty[Vector[Int], PairTables]

  def tablesOf(ctx: KCtx): PairTables = tablesCache.synchronized:
    tablesCache.getOrElseUpdate(
      ctx.spIdx, {
        val cxs = ctx.spIdx.map(i => StarChambers.complexOf(species(i)))
        val sts = ctx.spIdx.map(i => species(i).state)
        PairTables(
          cxs,
          ctx.spIdx.map(species),
          ctx.roles.toVector.map(r =>
            sts(r).positions.indices.map(x => x -> SpeciesCorona.ringAt(sts(r), x)).toMap
          ),
          ctx.roles.toVector.map(r =>
            sts(r).positions.indices.map(x => x -> ringDescriptors(ctx.g(r), x, identity)).toMap
          )
        )
      }
    )

  /** The far flag of ONE chamber (role r, index ci, end vertex x) pulled back through the placement `pl` at x
    * — descriptor rigidity, Option semantics: None when the placement's transported descriptors cannot host
    * the pullback uniquely (so candidate placements can be FILTERED by their induced map, the reverse
    * direction the realization filter needs). Returns the unfolded union index of the far chamber.
    */
  private[research_core] def pullbackAt(
      ctx: KCtx,
      tb: PairTables,
      r: Int,
      ci: Int,
      pl: PairShell.Pl
  ): Option[Int] =
    pullbackAtFrames(ctx, tb, r, ci, ctx.roleOf(pl.sp), pl.glu.y, pl.glu.rot.apply)

  /** The descriptor-matching core of the pullback, FRAME-PARAMETERIZED: the target role r2, its local vertex
    * y at the shared site, and the r2-local -> r-local linear transport. `pullbackAt` supplies the pattern
    * placement's frame (the k = 5 lesson stands: per-vertex placement frames are ambiguous whenever a star's
    * descriptor symmetry exceeds its site symmetry, and the two sides of an edge then read DIFFERENT flags —
    * which is why `derivedPairSymbol` derives σ₀ by the stabilizer fixpoint, not by frame reading).
    */
  private def pullbackAtFrames(
      ctx: KCtx,
      tb: PairTables,
      r: Int,
      ci: Int,
      r2: Int,
      y: Int,
      rot: Vec => Vec
  ): Option[Int] =
    val cx                     = tb.cxs(r)
    val ch                     = cx.chambers(ci)
    val x                      = cx.endVertex(ci)
    val ringX                  = tb.ringsR(r)(x)
    val kx                     = ringX.indexWhere(_._1 == ch.corner)
    if kx < 0 then return None
    val (cellX, germsX)        = tb.descsR(r)(x)(kx)
    val descY                  = ringDescriptors(ctx.g(r2), y, rot)
    val ky                     = descY.indices.filter { k =>
      val (cellY, germsY) = descY(k)
      cellY == cellX && {
        (germEq(germsY(0), germsX(0)) && germEq(germsY(1), germsX(1))) ||
        (germEq(germsY(0), germsX(1)) && germEq(germsY(1), germsX(0)))
      }
    }
    if ky.size != 1 then return None
    val sts2                   = tb.sts(r2).state
    val (cornerY, aInY, aOutY) = tb.ringsR(r2)(y)(ky.head)
    val arcX                   =
      val vids = tb.sts(r).state.corners(ch.corner).vids
      Set(vids(ch.side), vids((ch.side + 1) % vids.size))
    val (aInX, aOutX)          = (ringX(kx)._2, ringX(kx)._3)
    val faceIsIn               = Set(aInX._1, aInX._2) == arcX
    if !faceIsIn && Set(aOutX._1, aOutX._2) != arcX then return None
    val germX                  = if faceIsIn then germsX(0) else germsX(1)
    val (_, germsY)            = descY(ky.head)
    val arcY                   =
      if germEq(germsY(0), germX) then aInY
      else if germEq(germsY(1), germX) then aOutY
      else return None
    val vidsY                  = sts2.corners(cornerY).vids
    vidsY.indices
      .find(j => Set(vidsY(j), vidsY((j + 1) % vidsY.size)) == Set(arcY._1, arcY._2))
      .map { sideY =>
        val endY = if vidsY(sideY) == y then 0 else 1
        tb.offUn(r2) + tb.cxs(r2).index(Chamber(cornerY, sideY, endY))
      }

  /** σ₀ on the disjoint union of the k UNFOLDED star complexes, read off the k-pattern: chamber -> the far
    * flag of its edge pulled back through the placement at its end vertex, crossing species where the pattern
    * does. Same descriptor-rigidity location as the k = 1 derivation, role-parameterized; every pullback must
    * succeed (the pattern is certified) — loud failure otherwise.
    */
  def sigma0UnfoldedPair(ctx: KCtx, pat: KPattern): Vector[Int] =
    val tb = tablesOf(ctx)
    (for
      r  <- ctx.roles.toVector
      ci <- tb.cxs(r).chambers.indices
    yield
      val x = tb.cxs(r).endVertex(ci)
      pullbackAt(ctx, tb, r, ci, pat.p(r)(x)).getOrElse(
        throw new IllegalStateException(s"pullback failed (role $r chamber $ci)")
      )
    ).toVector

  /** The minimal symbol of a certified k-uniform honeycomb, derived from its certified k-pattern — the
    * k-orbit analogue of `derivedSymbolsOf`. Per role: the vertex stabilizer as the ball-preserving star
    * symmetries (the ball developed FROM that role, so every orbit gets an honest 3.05 ball), the star
    * complex folded by the corresponding chamber permutations; σ₀ from the σ₀-refined stabilizer FIXPOINT
    * (BFS-closure orbits, bounded faceLen) and descended to the folding tuple on the k-orbit union. Asserted
    * valid, minimal, and descent-well-defined — a wrong stabilizer or pullback fails loudly.
    */
  def derivedPairSymbol(ctx: KCtx, pat: KPattern): Sym =
    val balls                                                               =
      ctx.roles.toVector.map(r =>
        PairPatterns
          .developBall(ctx, pat, 3.05, start = r)
          .getOrElse(throw new IllegalStateException(s"certified pattern fails to develop from role $r"))
      )
    val syms                                                                = ctx.spIdx.map(symmetryOf)
    for r <- ctx.roles do
      require(syms(r).perms.size == ctx.stab(r).size, "stabilizer orders must agree between machineries")
    val tb                                                                  = tablesOf(ctx)
    val offUn                                                               = tb.offUn
    val s0Un                                                                = sigma0UnfoldedPair(ctx, pat)
    // per-role ball stabilizers, REFINED TO THE σ₀-COMPATIBLE JOINT FIXPOINT: a 3.05 ball can
    // carry MORE symmetry than the honeycomb — the k = 5 lift fixture has a {p3:12}#2 ball with
    // |H_ball| = 4 over a true site group of order 2 — and σ₀ sees the real gluing, so it is the arbiter:
    // keep only elements mapping σ₀-images within target-role orbits, re-checking as orbits shrink. Where
    // descent already held (every previously green case) the filter removes nothing — bit-identical.
    var hSets                                                               = ctx.roles.toVector.map { r =>
      val hIdx = ctx.stab(r).indices.filter(k => preservesPairBall(ctx, r, ctx.stab(r)(k), balls(r)))
      hIdx.map(syms(r).perms).toSet
    }
    // orbits by BFS closure under the set AS GENERATORS: correct even when a mid-fixpoint survivor set
    // transiently loses subgroup closure (single-application labeling corrupted the partition there and
    // let an incompatible element survive -- an infinite faceLen on the resulting
    // non-injective descended sigma0)
    def orbitIdsOf(hs: Vector[Set[StarFoldings.Perm]]): Vector[Vector[Int]] =
      ctx.roles.toVector.map { r =>
        val n   = syms(r).cx.chambers.size
        val ids = Array.fill(n)(-1)
        var nid = 0
        for c <- 0 until n if ids(c) < 0 do
          var front = List(c)
          ids(c) = nid
          while front.nonEmpty do
            front = front.flatMap(x =>
              hs(r).toList.map(_(x)).filter { y =>
                if ids(y) < 0 then { ids(y) = nid; true }
                else false
              }
            )
          nid += 1
        ids.toVector
      }
    var stable                                                              = false
    while !stable do
      val orb  = orbitIdsOf(hSets)
      val next = ctx.roles.toVector.map { r =>
        val nr = syms(r).cx.chambers.size
        hSets(r).filter { h =>
          (0 until nr).forall { c =>
            val t1 = s0Un(offUn(r) + c)
            val t2 = s0Un(offUn(r) + h(c))
            val r1 = tb.roleAt(t1)
            r1 == tb.roleAt(t2) && orb(r1)(t1 - offUn(r1)) == orb(r1)(t2 - offUn(r1))
          }
        }
      }
      stable = next == hSets
      hSets = next
    val folds                                                               = ctx.roles.toVector.map { r =>
      val hSet = hSets(r)
      require(
        hSet.forall(a => hSet.forall(b => hSet.contains(b.map(a)))),
        s"sigma0-refined ball stabilizer is not a subgroup (role $r)"
      )
      fold(syms(r), hSet)
    }
    val off                                                                 = folds.map(_.size).scanLeft(0)(_ + _)
    val u                                                                   = unionOf(folds)

    def roleOfFolded(q: Int): Int = off.lastIndexWhere(_ <= q, off.size - 2)

    def toFolded(qUn: Int): Int =
      val r = tb.roleAt(qUn)
      off(r) + folds(r).orbitOf(qUn - offUn(r))

    val s0 = Vector.tabulate(u.size) { q =>
      val r   = roleOfFolded(q)
      val rep = folds(r).orbitOf.indexOf(q - off(r))
      toFolded(s0Un(offUn(r) + rep))
    }
    require(
      s0Un.indices.forall(cUn => toFolded(s0Un(cUn)) == s0(toFolded(cUn))),
      "pattern sigma0 does not descend to the stabilizer foldings"
    )
    val s  = symOf(u, ctx.spIdx, s0)
    require(SymbolCatalog.valid(s), "derived pair symbol violates the axioms")
    require(SymbolCatalog.isMinimal(s), "derived pair symbol is not minimal")
    s
