package io.github.scala_tessella.research_core

import CompletenessAudit.{inLattice, latticeBasis}
import MonoShell.{Flags, Vec}
import PairPatterns.{ctxOf, developBall, KCtx, KPattern}
import PairShell.Pl
import StarFoldings.{fold, symmetryOf, Folded}
import SymbolCatalog.{canonicalKey, K2Entry, Sym}
import PairRealization.{derivedPairSymbol, pullbackAt, tablesOf}
import TransitivePatterns.{idMat, matOf}

/** The STANDALONE REALIZATION FILTER — from a census symbol to a certified honeycomb (or a refutation), with
  * no pre-certified honeycomb assumed. Required at k = 2: the census may contain minimal symbols beyond the
  * known classes, and each must be geometrically decided.
  *
  * The pipeline reverses the derivation direction of `PairRealization`, on the same invariants:
  *
  *   - σ₀-FILTERED DOMAINS: for each role and tiling-vertex, the cross-gluing atlas placements whose
  *     descriptor-rigidity pullback (`PairRealization.pullbackAt`) DESCENDS to exactly the symbol's σ₀ on the
  *     chambers at that vertex — the symbol prescribes the star-to-star gluings up to the folding subgroup's
  *     gauge lifts (an empty slot refutes the assembly outright);
  *   - the PATTERN SEARCH over those domains (`PairPatterns`' enumerator, whose completeness is not in
  *     question — only its budget), accepting a pattern iff it develops collision-free FROM BOTH ROLES, its
  *     derived minimal symbol (`derivedPairSymbol`) has EXACTLY the target key, and it PERIODIZES;
  *   - PERIODIZATION (`CompletenessAudit`'s periodization certificate transposed to two orbits): translation
  *     candidates harvested from the developed ball's role-0 entries with identity rotation (each is
  *     word-realized by the development BFS), a lattice basis, ball periodicity under ±τᵢ with role equality
  *     and Stab-compatible frames, lattice invariance under every placement rotation, and the coverage
  *     arithmetic.
  *
  * A symbol is REALIZED by the first accepted pattern at any of its provenance assemblies (`K2Entry.folds`);
  * it is REFUTED only when every provenance assembly is exhausted — sound because a genuine honeycomb
  * carrying the symbol lives at its own stabilizer folding pair, which is among the provenance (the census'
  * completeness argument), and its own pattern passes every acceptance check. Caps are reported honestly.
  */
object SymbolRealizationFilter:

  private def vAdd(a: Vec, b: Vec): Vec = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private def vNorm(a: Vec): Double     = math.sqrt(a._1 * a._1 + a._2 * a._2 + a._3 * a._3)
  private def vScale(a: Vec, s: Double) = (a._1 * s, a._2 * s, a._3 * s)

  /** The σ₀-consistent placement domains of one assembly: per role, per tiling-vertex, the atlas placements
    * whose pullback descends to the symbol's σ₀ on every chamber at that vertex.
    */
  def sigma0Domains(
      ctx: KCtx,
      folds: Vector[Folded],
      sym: Sym,
      flags: Flags
  ): Vector[Vector[Vector[Pl]]] =
    val tb                  = tablesOf(ctx)
    val off                 = folds.map(_.size).scanLeft(0)(_ + _)
    require(
      ctx.roles.forall(r => folds(r).orbitOf.size == tb.cxs(r).chambers.size) &&
        sym.size == folds.map(_.size).sum,
      "folding tuple does not match the symbol's chamber structure"
    )
    def folded(j: Int): Int =
      val r = tb.roleAt(j)
      off(r) + folds(r).orbitOf(j - tb.offUn(r))
    ctx.roles.toVector.map { r =>
      val chambersAt = tb.cxs(r).chambers.indices.groupBy(ci => tb.cxs(r).endVertex(ci))
      ctx.g(r).u.indices.toVector.map { x =>
        val cis   = chambersAt(x).toVector
        val cands = ctx.roles.toVector.flatMap(r2 =>
          PairShell.crossGluings(ctx.g(r), x, ctx.g(r2), flags).map(glu => Pl(ctx.spIdx(r2), glu))
        )
        cands.filter { pl =>
          cis.forall { ci =>
            pullbackAt(ctx, tb, r, ci, pl).exists(j => sym.s0(folded(tb.offUn(r) + ci)) == folded(j))
          }
        }
      }
    }

  /** The two-orbit periodization certificate (`CompletenessAudit`'s, transposed): a lattice basis from
    * ball-harvested role-0 identity-rotation translations, ball periodicity under ±τᵢ (role + Stab-compatible
    * frames), lattice invariance, coverage arithmetic. LATTICE INVARIANCE is checked on the ROLE-0 BALL
    * FRAMES — the point parts of the developed same-role transports, i.e. of actual honeycomb symmetries —
    * NOT on the placement rotations as in the k = 1 audit: a cross-species placement is a germ correspondence
    * between the two orbits' stars, not a symmetry, and its rotation need not normalize Λ (the known-genuine
    * 4H pattern falsified the naive transposition). Every same-role generator composition appears among the
    * role-0 entries of the certificate ball, so the frame check subsumes the generator condition.
    */
  final case class PairCertificate(
      tau: (Vec, Vec, Vec),
      covBound: Double,
      rPer: Double,
      periodic: Boolean,
      latticeInvariant: Boolean,
      coverage: Boolean
  ):
    def ok: Boolean = periodic && latticeInvariant && coverage

  /** Is the ball invariant under ±τ (same role, Stab-compatible frame) wherever the shifted position stays
    * inside `radius`? The load-bearing periodicity predicate, shared by the harvest filter and the final
    * certificate.
    */
  private def periodicUnder(
      ctx: KCtx,
      entries: Vector[(Int, TransitivePatterns.Iso)],
      taus: Vector[Vec],
      radius: Double
  ): Boolean =
    val byPos = entries.map((r, t) => TransitivePatterns.round4(t.t) -> (r, t)).toMap
    entries.forall { (r, t) =>
      taus.flatMap(tau => Vector(tau, vScale(tau, -1.0))).forall { tau =>
        val q = vAdd(t.t, tau)
        if vNorm(q) > radius - 1e-6 then true
        else
          byPos.get(TransitivePatterns.round4(q)) match
            case None            => false
            case Some((r2, t2i)) =>
              r2 == r &&
              (t2i.m.dist(t.m) < 1e-4 ||
                ctx.stab(r).exists(s => (t2i.m.t * t.m).dist(s) < 1e-3))
      }
    }

  /** `certifyPair` with an ESCALATING harvest radius: k ≥ 3 classes carry longer translation vectors (longer
    * necklace words; a ℤ/3 position period can triple the vertical period), so the 6.5 harvest that covers
    * every k = 2 class can miss a basis — the k = 3 sizing run realized only 5/8 slab classes per triple,
    * each failure "no translation basis" AFTER passing derived-key equality. Escalation fires only when the
    * smaller harvest fails, so k = 2 behavior is bit-identical; every rung's certificate re-verifies at its
    * own full periodization radius, so soundness is untouched. The 15.5 rung is the k = 4 transposition of
    * the same lesson: the two fully-folded 58-chamber {c,e,h,p} slab classes pass derived-key equality on all
    * 6 candidates but find no basis at 12.5.
    */
  def certifyEscalating(ctx: KCtx, pat: KPattern): Option[PairCertificate] =
    Vector(6.5, 9.5, 12.5, 15.5).iterator
      .map(r => certifyPair(ctx, pat, r))
      .collectFirst { case Some(c) if c.ok => c }

  def certifyPair(ctx: KCtx, pat: KPattern, harvestRadius: Double = 6.5): Option[PairCertificate] =
    // translation CANDIDATES are harvested generously — any role-0 entry whose frame lies in the star
    // symmetry group (the BFS stores one path-composition frame per position, which for a genuine translate
    // need not be the identity: {e,p} falsified the exact-identity harvest) — then each candidate is
    // VERIFIED against the harvest ball's ±τ-periodicity before entering the basis (a Stab-frame position
    // need not be a translation at all: {c,h}'s cch symbol falsified the unverified generous harvest). The
    // final certificate re-verifies the chosen basis at the full periodization radius.
    for
      seed    <- developBall(ctx, pat, harvestRadius)
      cands    = seed
                   .collect {
                     case (0, t)
                         if vNorm(t.t) > 1e-4 && ctx.stab(0).exists(s => t.m.dist(s) < 1e-4) =>
                       t.t
                   }
                   .sortBy(vNorm)
      verified = cands.filter(tau => periodicUnder(ctx, seed, Vector(tau), harvestRadius))
      basis   <- latticeBasis(verified)
      cert    <- certifyWith(ctx, pat, basis)
    yield cert

  def certifyWith(
      ctx: KCtx,
      pat: KPattern,
      basis: (Vec, Vec, Vec)
  ): Option[PairCertificate] =
    val (t1, t2, t3) = basis
    val maxT         = Vector(t1, t2, t3).map(vNorm).max
    val covBound     =
      (for
        e1 <- Vector(1.0, -1.0)
        e2 <- Vector(1.0, -1.0)
        e3 <- Vector(1.0, -1.0)
      yield vNorm(vAdd(vAdd(vScale(t1, e1), vScale(t2, e2)), vScale(t3, e3)))).max / 2.0
    val rPer         = covBound + maxT + 1.6
    developBall(ctx, pat, rPer).map { entries =>
      val periodic = periodicUnder(ctx, entries, Vector(t1, t2, t3), rPer)
      val latInv   = entries.forall { (r, t) =>
        r != 0 || Vector(t1, t2, t3).forall(tau => inLattice(basis, t.m(tau)))
      }
      PairCertificate(basis, covBound, rPer, periodic, latInv, rPer >= covBound + maxT + 1.5)
    }

  // ---------- SYMBOL-DRIVEN RIGID DEVELOPMENT (the pattern constructor) ----------
  //
  // Pattern-space search for realization was measured to be hopeless: σ₀-filtered domains collapse
  // to the |H| gauge lifts per slot, but slot-INDEPENDENT gauge choices cannot be coordinated — R1+R2,
  // both-roles partial development and σ₀ descent all pass for wrong-class combinations (the coarse-folding
  // descent aliases other classes onto the same folded σ₀), and the genuine gauge sits beyond 800 emissions
  // on the 4H canary. Here gauge coherence is TRANSPORTED instead: stars are placed one at a time, each
  // carrying its CHAMBER LIFT (unfolded chambers -> the symbol's folded chambers, a deck translate of the
  // canonical descent); σ₀ prescribes the far folded chamber of every flag, so a placement is admissible at
  // a star only with a lift matching all of them, and a re-visited star must agree in frame AND lift
  // (Stab-conjugation-aware). Completeness of refutation: candidates per edge run over the FULL atlas × all
  // deck lifts, and every base deck is tried — a genuine honeycomb's own development appears as some branch.

  /** One placed star of the symbol development: role, frame, chamber lift, chosen placement per slot. */
  final private case class Placed(
      r: Int,
      t: TransitivePatterns.Iso,
      lift: Vector[Int],
      slots: Array[Option[Pl]]
  )

  /** All σ₀-admissible (placement, far lift) pairs at one slot of a placed star with lift `lift`. */
  private def admissibleAt(
      ctx: KCtx,
      tb: PairRealization.PairTables,
      folds: Vector[Folded],
      decks: Vector[Set[StarFoldings.Perm]],
      sym: Sym,
      r: Int,
      x: Int,
      lift: Vector[Int],
      flags: Flags
  ): Vector[(Pl, Vector[Int])] =
    val off = folds.map(_.size).scanLeft(0)(_ + _)
    val cis = tb.cxs(r).chambers.indices.filter(ci => tb.cxs(r).endVertex(ci) == x).toVector
    val out = Vector.newBuilder[(Pl, Vector[Int])]
    for
      r2  <- ctx.roles.toVector
      glu <- PairShell.crossGluings(ctx.g(r), x, ctx.g(r2), flags)
    do
      val pl    = Pl(ctx.spIdx(r2), glu)
      val pulls = cis.map(ci => ci -> pullbackAt(ctx, tb, r, ci, pl))
      if pulls.forall(_._2.isDefined) then
        val far = pulls.map((ci, j) => ci -> (j.get - tb.offUn(r2)))
        for h2 <- decks(r2) do
          val lift2 = Vector.tabulate(tb.cxs(r2).chambers.size)(c => off(r2) + folds(r2).orbitOf(h2(c)))
          if far.forall((ci, j2) => lift2(j2) == sym.s0(lift(ci))) then
            if !out.result().exists((p, l) => p == pl && l == lift2) then out += ((pl, lift2))
    out.result()

  /** Develop the symbol at one assembly from one base deck: place stars breadth-first within `radius` +
    * slack, σ₀ prescribing every step; branch on genuinely distinct admissible placements; emit the pattern
    * read off the completed base stars of both roles. Returns the candidate patterns (deduped) and whether
    * the branch budget was hit.
    */
  private def developSymbol(
      ctx: KCtx,
      folds: Vector[Folded],
      decks: Vector[Set[StarFoldings.Perm]],
      sym: Sym,
      baseDeck: StarFoldings.Perm,
      flags: Flags,
      radius: Double,
      branchBudget: Int,
      log: String => Unit
  ): (Vector[KPattern], Boolean) =
    // the DFS nests one frame per processed slot (~stars × slots ≈ thousands) — run it on a big stack
    var result: (Vector[KPattern], Boolean) = (Vector.empty, false)
    val th                                  = new Thread(
      null,
      () =>
        result =
          developSymbolImpl(ctx, folds, decks, sym, baseDeck, flags, radius, branchBudget, log),
      "symbol-development",
      256L * 1024 * 1024
    )
    th.start()
    th.join()
    result

  private def developSymbolImpl(
      ctx: KCtx,
      folds: Vector[Folded],
      decks: Vector[Set[StarFoldings.Perm]],
      sym: Sym,
      baseDeck: StarFoldings.Perm,
      flags: Flags,
      radius: Double,
      branchBudget: Int,
      log: String => Unit
  ): (Vector[KPattern], Boolean) =
    val tb        = tablesOf(ctx)
    val slack     = radius + 1.6
    val perms     = ctx.roles.toVector.map(r => symmetryOf(ctx.spIdx(r)).perms)
    val stabs     = ctx.roles.toVector.map(r => ctx.stab(r))
    val out       = collection.mutable.LinkedHashSet.empty[KPattern]
    var branches  = 0
    var capped    = false
    val candCache = collection.mutable.Map.empty[(Int, Int, Vector[Int]), Vector[(Pl, Vector[Int])]]

    def candidatesFor(r: Int, x: Int, lift: Vector[Int]): Vector[(Pl, Vector[Int])] =
      // admissibility depends on the lift only through the σ₀ targets at this vertex — cache on those
      val cis     = tb.cxs(r).chambers.indices.filter(ci => tb.cxs(r).endVertex(ci) == x).toVector
      val targets = cis.map(ci => sym.s0(lift(ci)))
      candCache.getOrElseUpdate(
        (r, x, targets),
        admissibleAt(ctx, tb, folds, decks, sym, r, x, lift, flags)
      )

    def rec(
        placed: Map[(Long, Long, Long), Placed],
        queue: List[((Long, Long, Long), Int)] // (position key, slot) to process, BFS order
    ): Unit =
      if capped then return
      queue match
        case Nil             =>
          // every reachable slot processed: read the pattern off one complete star per role (the base for
          // role 0, the nearest complete star for each other role)
          val base   = placed((0L, 0L, 0L))
          val others = (1 until ctx.roles.size).map { r =>
            placed.values.toVector
              .filter(p => p.r == r && p.slots.forall(_.isDefined))
              .sortBy(p => vNorm(p.t.t))
              .headOption
          }
          if others.forall(_.isDefined) then
            out += KPattern(
              base.slots.map(_.get).toVector +: others.toVector.map(_.get.slots.map(_.get).toVector)
            )
        case (pk, x) :: rest =>
          val star = placed(pk)
          if star.slots(x).isDefined then rec(placed, rest)
          else
            val cands = candidatesFor(star.r, x, star.lift)
            if cands.isEmpty then () // dead branch
            else
              for (pl, lift2) <- cands do
                branches += 1
                if branches > branchBudget then capped = true
                if !capped then
                  val r2 = ctx.roleOf(pl.sp)
                  val t2 = star.t.compose(
                    TransitivePatterns.Iso(matOf(pl.glu.rot), ctx.g(star.r).u(x))
                  )
                  val k2 = TransitivePatterns.round4(t2.t)
                  star.slots(x) = Some(pl)
                  placed.get(k2) match
                    case Some(ex) =>
                      // the revisited star must agree: same role, frame within Stab, lift conjugated
                      val okLift = ex.r == r2 && stabs(r2).indices.exists { k =>
                        (ex.t.m.t * t2.m).dist(stabs(r2)(k)) < 1e-3 &&
                        lift2.indices.forall(ci => lift2(ci) == ex.lift(perms(r2)(k)(ci)))
                      }
                      if okLift then rec(placed, rest)
                    case None     =>
                      if vNorm(t2.t) > slack + 1e-6 then rec(placed, rest) // beyond horizon: record slot only
                      else
                        val fresh = Placed(r2, t2, lift2, Array.fill(ctx.g(r2).u.size)(None))
                        val jobs  = ctx.g(r2).u.indices.map(y => (k2, y)).toList
                        rec(placed + (k2 -> fresh), rest ::: jobs)
                  star.slots(x) = None

    val baseLift = Vector.tabulate(tb.cxs(0).chambers.size)(c => folds(0).orbitOf(baseDeck(c)))
    val base     = Placed(
      0,
      TransitivePatterns.Iso(idMat, (0.0, 0.0, 0.0)),
      baseLift,
      Array.fill(ctx.g(0).u.size)(None)
    )
    rec(
      Map((0L, 0L, 0L) -> base),
      ctx.g(0).u.indices.map(y => ((0L, 0L, 0L), y)).toList
    )
    (out.toVector, capped)

  /** The realization verdict of one census symbol. `assembly` is the index into `entry.folds` that realized
    * it; `candidates` counts the extracted patterns tested across all assemblies; refutation is honest only
    * when nothing is capped.
    */
  final case class Verdict(
      pat: Option[KPattern],
      cert: Option[PairCertificate],
      assembly: Int,
      candidates: Int,
      capped: Boolean
  ):
    def realized: Boolean = pat.isDefined

  def realize(
      entry: K2Entry,
      flags: Flags,
      branchBudget: Int = 200000,
      log: String => Unit = _ => ()
  ): Verdict =
    realizeK(
      SymbolCatalog.KEntry(Vector(entry.spA, entry.spB), entry.sym, entry.folds.map((a, b) => Vector(a, b))),
      flags,
      branchBudget,
      log
    )

  /** The k-ary realization driver — `realize` on a folding-TUPLE provenance entry (the k = 2 form wraps this;
    * behavior there is unchanged).
    */
  def realizeK(
      entry: SymbolCatalog.KEntry,
      flags: Flags,
      branchBudget: Int = 200000,
      log: String => Unit = _ => ()
  ): Verdict =
    val ctx       = ctxOf(entry.sps)
    val targetKey = canonicalKey(entry.sym)
    val syms      = entry.sps.map(symmetryOf)
    for r <- ctx.roles do
      require(syms(r).perms.size == ctx.stab(r).size, "stabilizer orders must agree between machineries")
    var tested    = 0
    var capped    = false
    var verdict   = Verdict(None, None, -1, 0, false)
    var ai        = 0
    while ai < entry.folds.size && !verdict.realized do
      val subs     = entry.folds(ai)
      val folds    = ctx.roles.toVector.map(r => fold(syms(r), subs(r)))
      val decks    = subs
      log(s"  assembly ${ai + 1}/${entry.folds.size} |H|=${subs.map(_.size).mkString(",")}")
      val deckList = subs(0).toVector
      var di       = 0
      while di < deckList.size && !verdict.realized do
        val (pats, capd) =
          developSymbol(ctx, folds, decks, entry.sym, deckList(di), flags, 3.05, branchBudget, log)
        capped ||= capd
        if pats.nonEmpty then log(s"    deck ${di + 1}/${deckList.size}: ${pats.size} candidate patterns")
        for pat <- pats if !verdict.realized do
          tested += 1
          val stage =
            val clashRole = ctx.roles.drop(1).find(r => developBall(ctx, pat, 3.05, start = r).isEmpty)
            if clashRole.isDefined then s"role-${clashRole.get} development clash"
            else if developBall(ctx, pat, 3.05).isEmpty then "role-0 development clash"
            else
              scala.util.Try(canonicalKey(derivedPairSymbol(ctx, pat))) match
                case scala.util.Failure(e)                   => s"derivation failed: ${e.getMessage}"
                case scala.util.Success(k) if k != targetKey => "derived key differs from target"
                case _                                       =>
                  certifyEscalating(ctx, pat) match
                    case None    => "no certificate at any harvest radius"
                    case Some(_) => "ACCEPTED"
          if stage == "ACCEPTED" then
            log(s"  REALIZED at assembly ${ai + 1}, deck ${di + 1}, candidate $tested")
            verdict = Verdict(Some(pat), certifyEscalating(ctx, pat), ai, tested, capped)
          else log(s"    candidate $tested rejected: $stage")
        di += 1
      ai += 1
    if verdict.realized then verdict else Verdict(None, None, -1, tested, capped)
