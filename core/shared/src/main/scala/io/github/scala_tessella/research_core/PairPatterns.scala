package io.github.scala_tessella.research_core

import SpeciesEnumerator.species
import MonoShell.{geomOf, Flags, StarGeom, Vec}
import PairShell.Pl
import TransitivePatterns.{cornerData, idMat, inStab, matOf, round4, stabilizers, starSig, Iso, Mat}

/** PAIR PATTERNS + rigid development — the two-orbit generalization of `TransitivePatterns`, since
  * generalized to k ROLES (`KCtx`/`KPattern`; the pair names remain as aliases and every k = 2 call site is
  * unchanged — the k = 2 checks are the regression oracle for the generalization). A k-uniform Krötenheerdt
  * honeycomb on a fair k-set {S₁..S_k} yields a K-PATTERN — for each tiling-vertex of each base star, the
  * (species, gluing) of the neighbor star, from `PairShell`'s cross-gluing atlas — satisfying the groupoid
  * versions of `TransitivePatterns`' conditions:
  *
  *   - (R1) reverse pairs COUPLE the two patterns: if S's vertex x places a τ-star with back-vertex y, then
  *     τ's pattern at y must place an S-star and the rotation product must lie in Stab(S);
  *   - (R2) face-cycle words close: the walk around a face hops between the two stars as the pattern
  *     dictates, and the composed word must fix the base (translation 0, rotation in the base stabilizer,
  *     landing back on the base species).
  *
  * Both shells must be MIXED (`PairShell`: with two orbits the decorated shell is an invariant and
  * connectivity forces a cross edge) — all-same patterns are pruned as soon as the seed shell is fully
  * assigned; a cross in the S-shell forces one in the T-shell via R1. Acceptance is COLLISION-FREE
  * DEVELOPMENT exactly as in `TransitivePatterns` (R1+R2 do not imply global consistency), with the collision
  * check species-aware: a re-reached position must carry the same species AND a Stab-compatible orientation.
  * Classes are separated by canonical development fingerprints based at the S-star over Stab(S), with species
  * tags in the encoding. Since the two species are distinct (Krötenheerdt), the two vertex classes can never
  * merge — an accepted pattern's development is 2-uniform, not accidentally 1-uniform.
  *
  * ENUMERATION runs through `TransitivePatterns`' COSET-SKELETON decomposition, per role: each joint domain
  * is grouped by placed stars (same species, rotation quotient in Stab(placed species)), mixed skeletons —
  * one coset per tiling-vertex, all-same cut at the skeleton level since species is coset-invariant — are
  * enumerated with the `PairShell` pairwise shell compatibility on representatives, the two roles' skeletons
  * combine as a product (they are coupled only through R1/R2, which the per-skeleton search checks), and the
  * joint DFS runs within each skeleton under a small cap. When every vertex of both roles is single-coset,
  * the forcing theorem applies verbatim (neighbor stars are forced, the honeycomb is determined by the base
  * star) and one witness suffices. Caps are reported honestly (`capped`); the census-closing audit belongs to
  * the caller, as `CompletenessAudit` does for `TransitivePatterns`.
  */
object PairPatterns:

  /** The k roles of a k-pattern; role 0 is the fingerprint base. `nbrs` is the arc adjacency of each role's
    * star and `arcKeys` its sorted arc list, both precomputed (face walks consult them constantly).
    */
  final case class KCtx(
      spIdx: Vector[Int], // species index per role
      g: Vector[StarGeom],
      corners: Vector[Vector[(Int, Vector[Vec])]],
      stab: Vector[Vector[Mat]],
      nbrs: Vector[Map[Int, Vector[Int]]],
      arcKeys: Vector[Vector[(Int, Int)]]
  ):
    def roleOf(sp: Int): Int = spIdx.indexOf(sp)
    def roles: Range         = spIdx.indices

  /** Historical k = 2 names — the machinery is k-role; the aliases keep every pair call site unchanged. */
  type PairCtx  = KCtx
  type PPattern = KPattern
  val PPattern: KPattern.type = KPattern

  def ctxOf(sps: Vector[Int]): KCtx =
    require(sps.distinct.size == sps.size, "Krötenheerdt roles carry pairwise distinct species")
    val gs = sps.map(i => geomOf(species(i)))
    val cs = gs.map(cornerData)
    val nb = gs.map(g => g.u.indices.map(v => v -> arcsAt(g, v)).toMap)
    val ak = gs.map(g => g.st.arcs.keys.toVector.sorted)
    KCtx(sps, gs, cs, gs.lazyZip(cs).map(stabilizers), nb, ak)

  def ctxOf(i: Int, j: Int): KCtx = ctxOf(Vector(i, j))

  /** A k-pattern: one placement per tiling-vertex per role. */
  final case class KPattern(p: Vector[Vector[Pl]]):
    def iso(ctx: KCtx, r: Int, x: Int): Iso = Iso(matOf(p(r)(x).glu.rot), ctx.g(r).u(x))

  private def arcsAt(g: StarGeom, v: Int): Vector[Int] =
    g.st.arcs.keys.toVector.collect {
      case (a, b) if a == v => b
      case (a, b) if b == v => a
    }

  private def vAdd(a: Vec, b: Vec): Vec                 = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private def vSub(a: Vec, b: Vec): Vec                 = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def vDot(a: Vec, b: Vec): Double              = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def vScale(a: Vec, s: Double): Vec            = (a._1 * s, a._2 * s, a._3 * s)
  private def vNorm(a: Vec): Double                     = math.sqrt(vDot(a, a))
  private def vUnit(a: Vec): Vec                        = vScale(a, 1.0 / vNorm(a))
  private def inPlane(g: StarGeom, v: Int, w: Int): Vec =
    vUnit(vSub(g.u(w), vScale(g.u(v), vDot(g.u(w), g.u(v)))))

  /** Walk the face of arc `key` of the base star of role r0 under a partial joint assignment. Left(role,
    * vertex) = assignment needed; Right(None) = geometric inconsistency; Right(Some(word)) = closed.
    */
  def walkFace(
      ctx: KCtx,
      assigned: (Int, Int) => Option[Pl],
      r0: Int,
      key: (Int, Int)
  ): Either[(Int, Int), Option[(Iso, Int)]] =
    val p        = ctx.g(r0).st.arcs(key).face
    var role     = r0
    var exit     = key._1
    var interior = inPlane(ctx.g(r0), key._1, key._2)
    var word     = Iso(idMat, (0.0, 0.0, 0.0))
    var steps    = 0
    while steps < p do
      assigned(role, exit) match
        case None     => return Left((role, exit))
        case Some(pl) =>
          val m  = matOf(pl.glu.rot)
          word = word.compose(Iso(m, ctx.g(role).u(exit)))
          val r2 = ctx.roleOf(pl.sp)
          val mt = m.t
          val dI = mt(interior)
          val y  = pl.glu.y
          ctx.nbrs(r2)(y).find(z => vNorm(vSub(inPlane(ctx.g(r2), y, z), dI)) < 1e-4) match
            case None     => return Right(None)
            case Some(zz) =>
              role = r2
              exit = zz
              interior = inPlane(ctx.g(r2), zz, y)
      steps += 1
    Right(Some((word, role)))

  // ---------- the skeleton-level filters ----------
  //
  // Each filter below is SOUND — it can never remove a member that participates in any genuine pattern —
  // and package-private so the spec can exercise it in isolation against edge cases and certified `TransitivePatterns`
  // output. searchPairPatterns composes them before its DFS.

  /** SPECIES-COUNT SHELL FILTER. Orbit-transitivity makes the decorated shell an invariant: EVERY vertex of
    * species S carries role S's shell, so the species multiset around every vertex of the development is
    * fixed by the skeleton. Each placed star's already-visible neighbors — the base vertex and the shell
    * vertices that coincide with one of the placed star's OWN tiling-vertex positions (coset-invariant: a
    * coset is one placed star), all with skeleton-fixed species — are a subset of its own full shell, so
    * their species counts may not exceed its role's counts — PER SPECIES: the visible neighbor species
    * multiset must be a sub-multiset of the role's full shell species multiset (at k = 2 this is exactly the
    * original cross/same-count check, spec-pinned; at k ≥ 3 it is strictly sharper and equally sound — the
    * skeleton fixes the full multiset, of which the visible neighbors are a subset). This kills the
    * combinatorially wrong species distributions (e.g. a Barlow shell with a lone cross neighbor, impossible
    * because the trios above and below a vertex lie in one layer) that are consistent as bare first shells
    * but whose refutation through R1/R2 alone costs millions of DFS nodes. Applies only when every slot is
    * species-homogeneous (skeleton domains and the forced single-coset case); vacuously true otherwise.
    */
  def speciesShellOk(ctx: KCtx, doms: Vector[Vector[Vector[Pl]]]): Boolean =
    !doms.forall(_.forall(d => d.nonEmpty && d.forall(_.sp == d.head.sp))) || {
      val spSlot     = ctx.roles.toVector.map(r => doms(r).map(_.head.sp))
      // the full shell species multiset of each role, as its skeleton fixes it
      val shellCount = ctx.roles.toVector.map(r => spSlot(r).groupBy(identity).view.mapValues(_.size).toMap)
      ctx.roles.forall { r =>
        val u = ctx.g(r).u
        u.indices.forall { x =>
          val rep     = doms(r)(x).head
          val sp2     = rep.sp
          val r2      = ctx.roleOf(sp2)
          // the placed star's tiling-vertex positions; its back-vertex lands on the base (origin)
          val placed  = ctx.g(r2).u.map(z => vAdd(u(x), rep.glu.rot(z)))
          val visible = (ctx.spIdx(r), (0.0, 0.0, 0.0)) +:
            u.indices.toVector.map(x2 => (spSlot(r)(x2), u(x2)))
          val nbrSp   = visible.collect {
            case (sp, pos) if placed.exists(pp => vNorm(vSub(pp, pos)) < 1e-4) => sp
          }
          nbrSp.groupBy(identity).forall((sp, seen) => seen.size <= shellCount(r2).getOrElse(sp, 0))
        }
      }
    }

  /** SPECIES-ARRANGEMENT FILTER — the decorated-shell invariant one level deeper, member-wise. In the
    * development, the star placed by member `pl` at slot (r, x) is a vertex of species sp2 = pl.sp, so it
    * carries role r2 = roleOf(sp2)'s germ: its tiling-vertex in direction pl.rot(u_r2(z)) holds the species
    * role r2's skeleton assigns to z. Where that position coincides with the base (origin) or with a shell
    * vertex u(x2) — whose species role r's skeleton fixes — the two assignments must agree: spSlot(r2)(z) ==
    * spIdx(r), resp. spSlot(r2)(z) == spSlot(r)(x2). This couples the two roles' skeletons GEOMETRICALLY (a
    * Barlow h-trio must align with the partner skeleton's stacking axis — the joint product's axis-mismatched
    * pairings die here instead of after millions of DFS nodes) and forces layer coherence among same-species
    * placements. Single pass (the constraint is member-local given the skeleton); applies only when every
    * slot is species-homogeneous; None when a slot empties.
    */
  def speciesArrangementFilter(
      ctx: KCtx,
      doms: Vector[Vector[Vector[Pl]]]
  ): Option[Vector[Vector[Vector[Pl]]]] =
    if !doms.forall(_.forall(d => d.nonEmpty && d.forall(_.sp == d.head.sp))) then Some(doms)
    else
      val spSlot = ctx.roles.toVector.map(r => doms(r).map(_.head.sp))
      val next   = ctx.roles.toVector.map { r =>
        val u = ctx.g(r).u
        doms(r).indices.toVector.map { x =>
          doms(r)(x).filter { pl =>
            val r2 = ctx.roleOf(pl.sp)
            ctx.g(r2).u.indices.forall { z =>
              val p = vAdd(u(x), pl.glu.rot(ctx.g(r2).u(z)))
              if vNorm(p) < 1e-4 then spSlot(r2)(z) == ctx.spIdx(r)
              else
                u.indices.find(x2 => vNorm(vSub(p, u(x2))) < 1e-4) match
                  case Some(x2) => spSlot(r2)(z) == spSlot(r)(x2)
                  case None     => true
            }
          }
        }
      }
      if next.forall(_.forall(_.nonEmpty)) then Some(next) else None

  /** R1-VIABILITY FIXPOINT: a member survives only while its reverse slot holds a species-matched member
    * whose rotation product lies in the base stabilizer — in any full pattern r1ok demands exactly that of
    * the chosen partner, so members of genuine patterns are never removed (their partners survive by the same
    * induction). None when a slot empties: no pattern exists in these domains.
    */
  def r1ViabilityFixpoint(
      ctx: KCtx,
      domains: Vector[Vector[Vector[Pl]]]
  ): Option[Vector[Vector[Vector[Pl]]]] =
    var doms   = domains
    var shrunk = true
    while shrunk do
      shrunk = false
      val next = ctx.roles.toVector.map { r =>
        doms(r).indices.toVector.map { x =>
          doms(r)(x).filter { pl =>
            val r2 = ctx.roleOf(pl.sp)
            doms(r2)(pl.glu.y).exists(py =>
              py.sp == ctx.spIdx(r) && inStab(matOf(pl.glu.rot) * matOf(py.glu.rot), ctx.stab(r))
            )
          }
        }
      }
      if next != doms then
        doms = next
        shrunk = true
    if doms.forall(_.forall(_.nonEmpty)) then Some(doms) else None

  /** GAUGE PIN — UNSOUND as symmetry breaking; retained only so the spec can pin its behavior and the
    * search's `pin` flag can exercise it in experiments. The intended argument (conjugating by t ∈ Stab(T) at
    * a cross slot sweeps that slot's coset while mapping the other role's skeleton to another enumerated
    * skeleton) FAILS: the pin slot is chosen per skeleton, and the sub-orbit of a class that stays inside a
    * given skeleton sweeps only a subgroup's worth of members — the pinned member can miss every genuine
    * presentation of a class in every skeleton (on the Barlow pair it excluded 9R and 12R entirely). The
    * search runs unpinned; sound symmetry breaking would need a gauge-covariant slot choice, the analogue of
    * the 2D program's lex-leader discipline.
    */
  private[research_core] def gaugePin(
      ctx: KCtx,
      doms: Vector[Vector[Vector[Pl]]]
  ): Vector[Vector[Vector[Pl]]] =
    val pinned = (for
      r <- ctx.roles.toVector
      v <- doms(r).indices
      d  = doms(r)(v)
      if d.nonEmpty && d.head.sp != ctx.spIdx(r) && d.forall(_.sp == d.head.sp)
    yield (r, v, d.size)).maxByOption(_._3)
    pinned.fold(doms)((r, v, _) => doms.updated(r, doms(r).updated(v, Vector(doms(r)(v).head))))

  /** Closability of the walk of `key` (a face of role r0's base star) under the partial assignment
    * `assigned`, completing open slots from `doms`: some completion must close with translation 0, rotation
    * in Stab(r0), landing on the base role.
    */
  private[research_core] def walkClosable(
      ctx: KCtx,
      doms: Vector[Vector[Vector[Pl]]],
      r0: Int,
      key: (Int, Int),
      assigned: (Int, Int) => Option[Pl]
  ): Boolean =
    def go(extra: Map[(Int, Int), Pl]): Boolean =
      walkFace(ctx, (rr, vv) => assigned(rr, vv).orElse(extra.get((rr, vv))), r0, key) match
        case Left((rr, vv))         => doms(rr)(vv).exists(pl => go(extra.updated((rr, vv), pl)))
        case Right(Some((w, rEnd))) => vNorm(w.t) < 1e-5 && rEnd == r0 && inStab(w.m, ctx.stab(r0))
        case Right(None)            => false
    go(Map.empty)

  /** WALK-SUPPORT FIXPOINT: a member survives only while every face walk starting at its slot is closable
    * from the current domains — any full pattern closes all walks with domain members, so the cut is sound.
    * It moves deep R2 refutations into local filtering; interleaved with the R1 fixpoint until both are
    * stable. None when a slot empties.
    */
  def walkSupportFixpoint(
      ctx: KCtx,
      domains: Vector[Vector[Vector[Pl]]]
  ): Option[Vector[Vector[Vector[Pl]]]] =
    var doms   = domains
    var shrunk = true
    while shrunk do
      shrunk = false
      val next = ctx.roles.toVector.map { r =>
        doms(r).indices.toVector.map { v =>
          doms(r)(v).filter { pl =>
            ctx.arcKeys(r).forall(key =>
              key._1 != v ||
                walkClosable(ctx, doms, r, key, (rr, vv) => if rr == r && vv == v then Some(pl) else None)
            )
          }
        }
      }
      if next != doms then
        r1ViabilityFixpoint(ctx, next) match
          case None    => return None
          case Some(d) =>
            doms = d
            shrunk = true
    if doms.forall(_.forall(_.nonEmpty)) then Some(doms) else None

  /** Joint DFS over both roles' domains with R1 (cross-coupled, species-matched) and R2 pruning; all-same
    * seed shells are pruned when `requireMixed` (a k = 2 pattern has a mixed shell on both sides). The
    * domains are first cut by the sound skeleton-level filters above (`pin` controls the gauge pin — tests
    * disable it to compare pinned and unpinned class sets); the DFS then assigns walk-demanded slots first
    * (with a targeted closability lookahead) and otherwise the most constrained open slot under an r1-forward
    * check. Every consistent pattern is emitted; returns (patterns found, capped).
    */
  def searchPairPatterns(
      ctx: KCtx,
      domains: Vector[Vector[Vector[Pl]]],
      cap: Int,
      emit: PPattern => Unit,
      log: String => Unit = _ => (),
      requireMixed: Boolean = true,
      pin: Boolean = false, // the gauge pin is UNSOUND (see gaugePin) — true only for experiments

      nodeBudget: Long =
        Long.MaxValue,       // exceeded => capped (honest reporting), for exploratory runs and diagnostics
      devBound: Double = 2.6 // partial-development prune radius; realization passes its acceptance reach
  ): (Int, Boolean) =
    if domains.exists(_.exists(_.isEmpty)) then return (0, false)
    if !speciesShellOk(ctx, domains) then return (0, false)
    var doms = speciesArrangementFilter(ctx, domains) match
      case None    => return (0, false)
      case Some(d) => d
    doms = r1ViabilityFixpoint(ctx, doms) match
      case None    => return (0, false)
      case Some(d) => d
    if pin then
      doms = r1ViabilityFixpoint(ctx, gaugePin(ctx, doms)) match
        case None    => return (0, false)
        case Some(d) => d
    doms = walkSupportFixpoint(ctx, doms) match
      case None    => return (0, false)
      case Some(d) => d

    val arcKeys  = ctx.arcKeys
    val chosen   = ctx.roles.toVector.map(r => Array.fill[Option[Pl]](doms(r).size)(None))
    var found    = 0
    var capped   = false
    var nodes    = 0L // DFS nodes explored — heartbeat so a silent search is distinguishable from a hang
    var maxDepth = 0  // deepest assignment reached — a stalling search is distinguishable from a thrashing one

    def r1ok(r: Int, x: Int, pl: Pl): Boolean =
      val r2  = ctx.roleOf(pl.sp)
      // (i) x's own reverse pair, when its partner is already assigned
      val fwd = chosen(r2)(pl.glu.y) match
        case Some(py) =>
          py.sp == ctx.spIdx(r) && inStab(matOf(pl.glu.rot) * matOf(py.glu.rot), ctx.stab(r))
        case None     => true
      // (ii) earlier placements whose placed star is role r with back-vertex x must reverse through pl
      fwd && ctx.roles.forall { rr =>
        chosen(rr).indices.forall { x2 =>
          (rr == r && x2 == x) || {
            chosen(rr)(x2) match
              case Some(p2) if ctx.roleOf(p2.sp) == r && p2.glu.y == x =>
                pl.sp == ctx.spIdx(rr) && inStab(matOf(p2.glu.rot) * matOf(pl.glu.rot), ctx.stab(rr))
              case _                                                   => true
          }
        }
      }

    def consistent(): Option[(Int, Int)] =
      var need: Option[(Int, Int)] = None
      val ok                       = ctx.roles.forall { r =>
        arcKeys(r).forall { key =>
          walkFace(ctx, (rr, v) => chosen(rr)(v), r, key) match
            case Left(rv)               => if need.isEmpty then need = Some(rv); true
            case Right(Some((w, rEnd))) =>
              vNorm(w.t) < 1e-5 && rEnd == r && inStab(w.m, ctx.stab(r))
            case Right(None)            => false
        }
      }
      if !ok then Some((-1, -1)) else need

    def mixedOk(): Boolean = // once a role's shell is full, it must contain a cross placement
      !requireMixed || {
        val each = ctx.roles.forall { r =>
          chosen(r).exists(_.isEmpty) || chosen(r).exists(p => p.get.sp != ctx.spIdx(r))
        }
        // k ≥ 3 only (at k = 2 a cross edge in each shell already joins the two roles): once ALL shells
        // are full, the role-adjacency graph (r — roleOf(placed sp)) must be connected — a k-uniform
        // honeycomb is connected, and orbit-transitivity makes the decorated shells an invariant, so a
        // disconnected role graph can host no genuine pattern
        each &&
        (ctx.roles.size <= 2 || ctx.roles.exists(r => chosen(r).exists(_.isEmpty)) || {
          val adj     = ctx.roles.toVector.map(r => chosen(r).map(p => ctx.roleOf(p.get.sp)).toSet)
          val reached = collection.mutable.Set(0)
          var grew    = true
          while grew do
            grew = false
            for r <- ctx.roles if !reached(r) && (adj(r).exists(reached) || reached.exists(adj(_)(r))) do
              reached += r
              grew = true
          reached.size == ctx.roles.size
        })
      }

    /** PARTIAL-DEVELOPMENT PRUNE: develop the ball of `bound` using only the ASSIGNED slots (unassigned slots
      * simply do not expand). Any species- or orientation-incompatible re-visit among the placed part dooms
      * EVERY completion of this partial assignment — the colliding words use assigned slots only — so the
      * subtree is cut. This moves the emission-time development rejection into the search: without it,
      * skeletons whose R1+R2-consistent patterns are dominated by non-developable ones (the Barlow cross-3
      * skeletons hosting 9R and 12R) drown the genuine patterns beyond any cap. The prune runs FROM BOTH
      * ROLES' bases (same soundness argument, base-independent): a role-0-only prune lets patterns whose
      * role-1-based development clashes survive to emission, and those wrong-gauge fakes dominate exactly as
      * the unpruned fakes did — the realization filter found the both-roles-consistent gauges buried beyond
      * 5000 emissions on three {e,p} fixture classes and 600+ on the 4H realization.
      */
    def partialDevOk(bound: Double): Boolean =
      def okFrom(start: Int): Boolean =
        val seen     = collection.mutable.Map.empty[(Long, Long, Long), (Int, Mat)]
        var frontier = List((start, Iso(idMat, (0.0, 0.0, 0.0))))
        seen((0L, 0L, 0L)) = (start, idMat)
        var ok       = true
        var d        = 0
        val maxD     = math.max(8, (2.5 * bound).toInt)
        while frontier.nonEmpty && ok && d < maxD do
          frontier = frontier.flatMap { (r, t) =>
            ctx.g(r).u.indices.toList.flatMap { x =>
              chosen(r)(x) match
                case None     => Nil
                case Some(pl) =>
                  val r2 = ctx.roleOf(pl.sp)
                  val t2 = t.compose(Iso(matOf(pl.glu.rot), ctx.g(r).u(x)))
                  if vNorm(t2.t) <= bound + 1e-6 then
                    val k = round4(t2.t)
                    seen.get(k) match
                      case Some((rS, mS)) =>
                        if rS != r2 ||
                          (mS.dist(t2.m) > 1e-4 && !ctx.stab(r2).exists(s => (mS.t * t2.m).dist(s) < 1e-3))
                        then ok = false
                        Nil
                      case None           =>
                        seen(k) = (r2, t2.m)
                        List((r2, t2))
                  else Nil
            }
          }
          d += 1
        ok
      ctx.roles.forall(okFrom)

    def tryAll(r: Int, v: Int, members: Vector[Pl]): Unit = scala.util.boundary:
      for pl <- members do
        chosen(r)(v) = Some(pl)
        val depth = chosen.map(_.count(_.isDefined)).sum
        // the partial-development prune runs at EVERY depth past the first collision-capable one: a
        // collision is caught at the assignment that creates it, so the whole subtree below it vanishes
        // (checkpointing every few depths instead lets the branching between creation and detection
        // balloon by orders of magnitude)
        if mixedOk() && (depth < 5 || partialDevOk(devBound)) then bt()
        chosen(r)(v) = None
        if found >= cap then { capped = true; scala.util.boundary.break() }

    def bt(): Unit =
      if found >= cap then { capped = true; return }
      if nodes >= nodeBudget then { capped = true; return }
      nodes += 1
      val depth = chosen.map(_.count(_.isDefined)).sum
      if depth > maxDepth then maxDepth = depth
      if nodes % 200000 == 0 then
        log(s"    ... search: ${nodes / 1000}k nodes, $found patterns, depth $depth (max $maxDepth)")
      consistent() match
        case Some((-1, -1)) => ()
        case Some((r, v))   =>
          // targeted lookahead: keep only members under which every walk now demanding (r, v) stays
          // closable — contradictions that the plain DFS would discover several assignments deeper
          // (once a face's last slot is filled) prune here instead
          val demanding =
            for
              r0  <- ctx.roles.toVector
              key <- arcKeys(r0)
              if walkFace(ctx, (rr, vv) => chosen(rr)(vv), r0, key) == Left((r, v))
            yield (r0, key)
          val viable    = doms(r)(v).filter { pl =>
            r1ok(r, v, pl) && {
              chosen(r)(v) = Some(pl)
              val ok =
                demanding.forall((r0, key) => walkClosable(ctx, doms, r0, key, (rr, vv) => chosen(rr)(vv)))
              chosen(r)(v) = None
              ok
            }
          }
          tryAll(r, v, viable)
        case None           =>
          // forward check + most-constrained open slot
          var bestR              = -1
          var bestV              = -1
          var bestVi: Vector[Pl] = null
          var dead               = false
          for r <- ctx.roles if !dead; v <- chosen(r).indices if !dead && chosen(r)(v).isEmpty do
            val vi = doms(r)(v).filter(r1ok(r, v, _))
            if vi.isEmpty then dead = true
            else if bestVi == null || vi.size < bestVi.size then
              bestR = r
              bestV = v
              bestVi = vi
          if !dead then
            if bestVi == null then
              found += 1
              emit(KPattern(chosen.map(_.map(_.get).toVector)))
            else tryAll(bestR, bestV, bestVi)

    bt()
    (found, capped)

  /** Develop the pair pattern (species-tagged BFS) out to `radius` + slack; None on any species- or
    * orientation-incompatible re-visit.
    */
  def developBall(
      ctx: KCtx,
      pat: PPattern,
      radius: Double,
      start: Int = 0 // base role of the ball (role 1 for role-1 stabilizer/fingerprint work)
  ): Option[Vector[(Int, Iso)]] =
    val slack    = radius + 1.6
    val seen     = collection.mutable.Map.empty[(Long, Long, Long), (Int, Iso)]
    var frontier = Vector((start, Iso(idMat, (0.0, 0.0, 0.0))))
    seen((0L, 0L, 0L)) = frontier.head
    var depth    = 0
    var clash    = false
    val maxDepth = math.max(14, (3.0 * slack).toInt)
    while frontier.nonEmpty && depth < maxDepth && !clash do
      frontier = frontier.flatMap { (r, t) =>
        ctx.g(r).u.indices.flatMap { x =>
          val pl  = pat.p(r)(x)
          val r2  = ctx.roleOf(pl.sp)
          val t2  = t.compose(pat.iso(ctx, r, x))
          val pos = t2.t
          if vNorm(pos) <= slack + 1e-6 then
            val k = round4(pos)
            seen.get(k) match
              case Some((rSeen, tSeen)) =>
                if rSeen != r2 ||
                  (tSeen.m.dist(t2.m) > 1e-4 && !ctx.stab(r2).exists(s => (tSeen.m.t * t2.m).dist(s) < 1e-3))
                then clash = true
                None
              case None                 =>
                seen(k) = (r2, t2)
                Some((r2, t2))
          else None
        }
      }
      depth += 1
    if clash then None else Some(seen.values.toVector)

  /** Canonical fingerprint of the ball, based at the role-0 star, minimized over Stab(role 0). */
  def fingerprintOf(
      ctx: KCtx,
      ball: Vector[(Int, Iso)],
      radius: Double
  ): Vector[(Long, Long, Long)] =
    val entries                                    = ball.filter((_, t) => vNorm(t.t) <= radius + 1e-6)
    def encode(s: Mat): Vector[(Long, Long, Long)] =
      entries
        .flatMap { (r, t) =>
          val p = round4(s(t.t))
          val m = s * t.m
          ((-1L - r, 0L, 0L) +: starSig(ctx.corners(r), m).flatMap((c, ds) => (c.toLong, 0L, 0L) +: ds)) :+ p
        }
        .sorted
    ctx.stab(0).map(encode).min(using scala.math.Ordering.Implicits.seqOrdering)

  // ---------- the coset-skeleton layer (`TransitivePatterns`' decomposition, per role) ----------

  /** Group a joint domain by PLACED STARS: two placements are coset-equivalent iff they place the same
    * species with rotations differing by Stab(placed species) — then they place the same geometric star, so
    * shell compatibility and the skeleton structure depend only on the coset. This is `TransitivePatterns`'
    * coset structure with the species tag added; the flat joint DFS is impractical on symmetric pairs (the
    * Barlow pair has 24 variables with 72-way domains) precisely because it re-derives each class through
    * many Stab-equivalent presentations, each paying a full development.
    */
  def cosetsOf(ctx: KCtx, dom: Vector[Pl]): Vector[Vector[Pl]] =
    val reps = collection.mutable.ArrayBuffer.empty[(Pl, collection.mutable.ArrayBuffer[Pl])]
    for pl <- dom do
      reps.find((r, _) =>
        r.sp == pl.sp && inStab(matOf(r.glu.rot).t * matOf(pl.glu.rot), ctx.stab(ctx.roleOf(pl.sp)))
      ) match
        case Some((_, members)) => members += pl
        case None               => reps += ((pl, collection.mutable.ArrayBuffer(pl)))
    reps.toVector.map(_._2.toVector)

  /** MIXED skeletons of one role: one coset per tiling-vertex, pruned by the `PairShell` pairwise shell
    * compatibility on representatives (coset-invariant: same coset = same placed star) and by mixedness —
    * species is coset-invariant, so a k = 2 pattern's mixed shell is a SKELETON property and all-same
    * skeletons are cut here (at the last slot, only cross cosets remain when none was chosen).
    */
  def roleSkeletons(
      ctx: KCtx,
      r: Int,
      cosets: Vector[Vector[Vector[Pl]]],
      flags: Flags
  ): (Vector[Vector[Vector[Pl]]], Boolean) =
    val n                = cosets.size
    val out              = Vector.newBuilder[Vector[Vector[Pl]]]
    var count            = 0
    var capped           = false
    val chosen           = Array.fill(n)(-1)
    def bt(x: Int): Unit =
      if count >= skeletonCap then capped = true
      else if x == n then
        out += chosen.toVector.zipWithIndex.map((ci, v) => cosets(v)(ci))
        count += 1
      else
        val mustCross =
          x == n - 1 && (0 until x).forall(v => cosets(v)(chosen(v)).head.sp == ctx.spIdx(r))
        cosets(x).indices.foreach { ci =>
          if !mustCross || cosets(x)(ci).head.sp != ctx.spIdx(r) then
            chosen(x) = ci
            val ok = (0 until x).forall(x2 =>
              PairShell.compatibleP(ctx.g(r), x, cosets(x)(ci).head, x2, cosets(x2)(chosen(x2)).head, flags)
            )
            if ok then bt(x + 1)
            chosen(x) = -1
        }
    bt(0)
    (out.result(), capped)

  // ---------- the per-pair driver ----------

  final case class Report(
      i: Int,
      j: Int,
      cosetCounts: Vector[Vector[Int]], // per role, per tiling-vertex: Stab-coset count of the joint domain
      forced: Boolean,                  // single coset everywhere in both roles: the forcing theorem applies
      skeletons: Int,                   // mixed joint skeletons searched (1 when forced)
      honeycombs: Int,                  // distinct development fingerprints (radius 2.05)
      stableAtR3: Boolean,              // fingerprints still distinct at radius 3.05
      patternsFound: Int,
      patternsRejected: Int,
      capped: Boolean
  )

  /** Per-skeleton pattern cap, as in `TransitivePatterns`. A 100× cap experiment on the Barlow anchor found
    * exactly the same classes in exactly the same skeletons as cap 40 (144000 patterns, 43 min, no new class)
    * — the cap is not where completeness lives; capped stays reported honestly and the census-closing audit
    * belongs to the caller, as `CompletenessAudit` does for `TransitivePatterns`.
    */
  private val patternCap  = 40
  private val skeletonCap = 5000

  private val heartbeatEvery = 25

  /** The full joint domains of a k-set: per role, per tiling-vertex, every atlas placement of any of the k
    * species (own species first, then the other roles in role order — the k = 2 ordering preserved exactly).
    */
  def jointDomainsOf(ctx: KCtx, flags: Flags): Vector[Vector[Vector[Pl]]] =
    ctx.roles.toVector.map { r =>
      ctx.g(r).u.indices.toVector.map { x =>
        PairShell.crossGluings(ctx.g(r), x, ctx.g(r), flags).map(Pl(ctx.spIdx(r), _)) ++
          ctx.roles.toVector
            .filter(_ != r)
            .flatMap(r2 => PairShell.crossGluings(ctx.g(r), x, ctx.g(r2), flags).map(Pl(ctx.spIdx(r2), _)))
      }
    }

  /** `log` receives heartbeat lines (domain and coset sizes up front, skeleton counts, then progress every
    * `heartbeatEvery` emitted patterns and on every new class) so long searches are observable mid-run —
    * never fire-and-forget.
    */
  def analyze(i: Int, j: Int, flags: Flags, log: String => Unit = _ => ()): Report =
    val t0      = System.currentTimeMillis
    def dt      = (System.currentTimeMillis - t0) / 1000
    val ctx     = ctxOf(i, j)
    val domains = jointDomainsOf(ctx, flags)
    log(
      ctx.roles
        .map(r => s"domains role$r: ${domains(r).map(_.size).mkString("/")}")
        .mkString("  ", " | ", s" | stab ${ctx.roles.map(r => ctx.stab(r).size).mkString(", ")} [${dt}s]")
    )
    val cosets  = ctx.roles.toVector.map(r => domains(r).map(dom => cosetsOf(ctx, dom)))
    log(
      ctx.roles
        .map(r => s"cosets role$r: ${cosets(r).map(_.size).mkString("/")}")
        .mkString("  ", " | ", s" [${dt}s]")
    )
    val forced  = cosets.forall(_.forall(_.size == 1))
    var capped  = false
    val skels   =
      if forced then Vector(domains) // the one skeleton; mixedness is enforced in the search
      else
        val perRole = ctx.roles.toVector.map { r =>
          val (s, c) = roleSkeletons(ctx, r, cosets(r), flags)
          capped ||= c
          s
        }
        // roles are coupled only through R1/R2 (checked in the search), so joint skeletons are the product
        val joint   = perRole.foldLeft(Vector(Vector.empty[Vector[Vector[Pl]]])) { (acc, s) =>
          for a <- acc; b <- s yield a :+ b
        }
        log(
          s"  skeletons: ${perRole.map(_.size).mkString(" x ")} = ${joint.size}" +
            s"${if capped then " (CAPPED)" else ""} [${dt}s]"
        )
        if joint.size > skeletonCap then { capped = true; joint.take(skeletonCap) }
        else joint
    // classes are keyed by the PAIR of fingerprints (radius 2.05, radius 3.05): the Barlow stackings agree
    // on small balls (the long-period letter-word regime of the Platonic note), so keying by the 2.05
    // fingerprint alone silently MERGES classes that only radius 3.05 separates — with the pair key the
    // merge shows up honestly as stableAtR3 = false instead
    val fps = collection.mutable.LinkedHashSet
      .empty[(Vector[(Long, Long, Long)], Vector[(Long, Long, Long)])]
    var nPat      = 0
    var nRejected = 0
    var nSeen     = 0
    for (skel, si) <- skels.zipWithIndex do
      if skels.size > 1 && (si + 1) % 100 == 0 then
        log(s"  ... skeleton ${si + 1}/${skels.size}, $nSeen patterns, ${fps.size} classes [${dt}s]")
      val cap    = if forced then 1 else patternCap
      val (n, c) = searchPairPatterns(
        ctx,
        skel,
        cap,
        pat => {
          developBall(ctx, pat, 3.05) match
            case None       => nRejected += 1
            case Some(ball) =>
              val key = (fingerprintOf(ctx, ball, 2.05), fingerprintOf(ctx, ball, 3.05))
              if fps.add(key) then
                log(s"  class ${fps.size} found at pattern ${nSeen + 1} (skeleton ${si + 1}) [${dt}s]")
          nSeen += 1
          if nSeen % heartbeatEvery == 0 then
            log(s"  ... $nSeen patterns, ${fps.size} classes, $nRejected rejected [${dt}s]")
        },
        m => log(s"  [skel ${si + 1}/${skels.size}]$m [${dt}s]")
      )
      nPat += n
      capped ||= c
    val stable    = fps.map(_._1).size == fps.size // radius 2.05 already separates all classes
    Report(i, j, cosets.map(_.map(_.size)), forced, skels.size, fps.size, stable, nPat, nRejected, capped)
