package io.github.scala_tessella.research_core

import MonoShell.{Glu, Rot}
import PairPatterns.PairCtx
import PairShell.Pl
import TransitivePatterns.Mat

/** Independent KNOWN-ANSWER fixtures for the Barlow pair (fixture-first): the four k = 2 Barlow stackings are
  * built DIRECTLY from their junction words over {c, h} — triangular lattice layers at height i·√(2/3),
  * positions stepping on ℤ/3, steps flipping exactly at h-junctions — and the germs at a c-vertex and an
  * h-vertex are extracted geometrically as alignment candidates of the species star models onto the actual
  * stars. No pattern-search machinery is used in the construction, so the fixtures can adjudicate it: every
  * genuine placement must appear in the cross-gluing atlas, every sound filter must keep it, and the search
  * must find a consistent pattern inside the fixture domains.
  */
object BarlowFixtures:
  type V3 = (Double, Double, Double)

  /** The four k = 2 Barlow junction necklaces: (c-cross, h-cross) = (6,6), (3,6), (6,3), (3,3). */
  val words: Vector[String] = Vector("ch", "cch", "chh", "cchh")

  private[research_core] def add(a: V3, b: V3): V3      = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private[research_core] def sub(a: V3, b: V3): V3      = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def dot(a: V3, b: V3): Double                 = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private[research_core] def norm(a: V3): Double        = math.sqrt(dot(a, a))
  private[research_core] def dist(a: V3, b: V3): Double = norm(sub(a, b))
  private def cross(a: V3, b: V3): V3                   =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)
  private[research_core] def unit(a: V3): V3            = { val n = norm(a); (a._1 / n, a._2 / n, a._3 / n) }

  private val dz    = math.sqrt(2.0 / 3.0)        // interlayer height of a unit-edge octet stacking
  private val delta = (0.5, math.sqrt(3.0) / 6.0) // in-plane shift per position step: (d1 + d2) / 3

  /** All stacking vertices with species letters, layers -L..L, lattice coordinates -m..m. */
  def verticesOf(word: String, big: Int, m: Int): Vector[(V3, Char)] =
    val n                    = word.length
    def letter(i: Int): Char = word(((i % n) + n) % n)
    // steps s(i) (layer i -> i+1) flip exactly at h-junctions: letter(i) == 'c' iff s(i-1) == s(i)
    val s                    = collection.mutable.Map(-big - 1 -> 1)
    for i <- -big to big do s(i) = s(i - 1) * (if letter(i) == 'h' then -1 else 1)
    // layer positions p(i) on Z/3
    val p                    = collection.mutable.Map(0 -> 0)
    for i <- 0 until big do p(i + 1) = p(i) + s(i)
    for i <- 0 until big do p(-i - 1) = p(-i) - s(-i - 1)
    (for
      i <- (-big to big).toVector
      a <- -m to m
      b <- -m to m
    yield
      val q = ((p(i) % 3) + 3) % 3
      val x = a * 1.0 + b * 0.5 + q * delta._1
      val y = b * math.sqrt(3.0) / 2 + q * delta._2
      ((x, y, i * dz), letter(i))
    ).toVector

  private[research_core] def nbrs(vs: Vector[(V3, Char)], v: V3): Vector[(V3, Char)] =
    vs.filter(w => math.abs(dist(w._1, v) - 1.0) < 1e-6)

  private def frame(b1: V3, b2raw: V3, det: Double): (V3, V3, V3) =
    val b2 = unit(sub(b2raw, (b1._1 * dot(b1, b2raw), b1._2 * dot(b1, b2raw), b1._3 * dot(b1, b2raw))))
    val c  = cross(b1, b2)
    (b1, b2, (c._1 * det, c._2 * det, c._3 * det))

  private def mapOfFrames(a: (V3, V3, V3), b: (V3, V3, V3)): Mat =
    // M with M(a_k) = b_k for orthonormal frames: M = B * A^T, rows r_i = sum_k b_k(i) * a_k
    def row(i: Int): V3 =
      def c(v: V3, j: Int) = j match { case 0 => v._1; case 1 => v._2; case 2 => v._3 }
      (
        c(b._1, i) * a._1._1 + c(b._2, i) * a._2._1 + c(b._3, i) * a._3._1,
        c(b._1, i) * a._1._2 + c(b._2, i) * a._2._2 + c(b._3, i) * a._3._2,
        c(b._1, i) * a._1._3 + c(b._2, i) * a._2._3 + c(b._3, i) * a._3._3
      )
    Mat(row(0), row(1), row(2))

  private def rotOf(m: Mat): Rot =
    val e = Vector((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0))
    Rot(e(0), e(1), e(2), m(e(0)), m(e(1)), m(e(2)))

  /** All orthogonal maps sending the model direction set U ONTO the direction set D; when `back` is set, only
    * maps with M(U(y)) = back for some y are produced, returned with that y. Candidate frames are anchored on
    * ANY non-collinear direction pair, matched by equal chord (equal angle) — anchoring only on unit-distance
    * (60°) pairs missed every alignment whose back direction has no 60° partner, e.g. a prism's vertical
    * direction, isolated at 90° from the rest of its star.
    */
  private[research_core] def alignments(
      U: Vector[V3],
      D: Vector[V3],
      back: Option[V3]
  ): Vector[(Int, Mat)] =
    val out = collection.mutable.ArrayBuffer.empty[(Int, Mat)]
    for
      y      <- U.indices
      w      <- U.indices
      if w != y && math.abs(math.abs(dot(U(y), U(w))) - 1.0) > 1e-6
      target <- back.map(Vector(_)).getOrElse(D)
      dAdj   <- D
      if math.abs(dist(target, dAdj) - dist(U(y), U(w))) < 1e-6
      det    <- Vector(1.0, -1.0)
    do
      val m = mapOfFrames(frame(U(y), U(w), 1.0), frame(target, dAdj, det))
      if U.forall(u => D.exists(d => dist(m(u), d) < 1e-4)) &&
        !out.exists(_._2.dist(m) < 1e-4)
      then out += ((y, m))
    out.toVector

  /** All orthogonal maps of the model direction set U onto D, for the fixture specs' own teeth tests. */
  private[research_core] def alignmentsForSpec(U: Vector[V3], D: Vector[V3]): Vector[(Int, Mat)] =
    alignments(U, D, None)

  /** GENERIC germ-domain extraction from a labeled vertex cloud (species letter per vertex, unit-distance
    * adjacency): per role, locate the base vertex of the role's letter nearest the cloud center, align the
    * role's star model onto its actual neighbor directions, and produce for every model slot all geometric
    * alignments of the neighbor's star model with the back-direction constraint — small known-genuine domains
    * for the pattern search. The cloud must extend a comfortable margin (≳ 2 units) beyond every base vertex;
    * `letterRole` maps a species letter to its ctx role.
    */
  def germDomainsOf(
      ctx: PairCtx,
      vs: Vector[(V3, Char)],
      roleLetters: Vector[Char],
      letterRole: Char => Int
  ): (Vector[Vector[Vector[Pl]]], Vector[(V3, Mat)]) =
    def germAt(r: Int): (Vector[Vector[Pl]], (V3, Mat)) =
      val letter  = roleLetters(r)
      val gBase   = ctx.g(r)
      // the base vertex of the wanted letter nearest the cloud center
      val v0      = vs.filter((_, c) => c == letter).minBy((v, _) => norm(v))._1
      val D0      = nbrs(vs, v0).map((w, _) => unit(sub(w, v0)))
      val (_, m0) = alignments(gBase.u, D0, None).head
      val m0t     = m0.t
      // model coordinates: w -> m0t(w - v0); the base star's directions become exactly gBase.u
      val doms    = gBase.u.indices.toVector.map { x =>
        val ux         = gBase.u(x)
        val approx     = add(v0, m0(ux))
        val (nPos, c2) = vs.minBy((w, _) => dist(w, approx)) // snap to the actual vertex
        require(dist(nPos, approx) < 1e-4, s"no fixture vertex at slot $x of the $letter-germ")
        val r2         = letterRole(c2)
        val g2         = ctx.g(r2)
        val dirs       = nbrs(vs, nPos).map((w, _) => m0t(unit(sub(w, nPos))))
        alignments(g2.u, dirs, Some((-ux._1, -ux._2, -ux._3))).map((y, m) =>
          Pl(ctx.spIdx(r2), Glu(y, rotOf(m)))
        )
      }
      (doms, (v0, m0))
    val perRole                                         = ctx.roles.toVector.map(germAt)
    (perRole.map(_._1), perRole.map(_._2))

  /** Does the pattern's development, re-anchored to the cloud through (v0, m0), reproduce the cloud — every
    * developed vertex at a cloud vertex of the SAME species letter? This pins a fixture-derived pattern to
    * its word: germ domains only see radius ~2, where distinct classes of the same pair may locally agree, so
    * "some developable pattern in the domains" is not yet "the word's class".
    */
  def developmentMatchesCloud(
      ctx: PairCtx,
      pat: PairPatterns.PPattern,
      vs: Vector[(V3, Char)],
      anchor: (V3, Mat),
      letterRole: Char => Int,
      radius: Double = 3.05,
      start: Int = 0
  ): Boolean =
    PairPatterns.developBall(ctx, pat, radius, start).exists { ball =>
      val (v0, m0) = anchor
      // compare within the stated radius only: developBall returns entries out to its slack annulus
      // (radius + 1.6), where a finite cloud window legitimately runs out of vertices
      ball.filter((_, t) => norm(t.t) <= radius + 1e-6).forall { (r, t) =>
        val p = add(v0, m0(t.t))
        vs.exists((w, c) => dist(w, p) < 1e-3 && letterRole(c) == r)
      }
    }

  /** The DIRECT fixture pattern — TransitivePatterns' pattern-from-honeycomb construction on the labeled
    * cloud, no pattern search: for each role's base star and each slot, the placement is the neighbor
    * species' base germ transported by a LOCAL SYMMETRY of the cloud (an orthogonal map taking the other
    * base's labeled neighborhood onto the neighbor's, to radius `symRadius`). Every honest (true-symmetry)
    * choice describes the honeycomb itself, so the first survivor is taken per slot; the caller MUST verify
    * the result with `developmentMatchesCloud` from BOTH roles, which catches any false local symmetry
    * loudly. Motivation : searching the germ domains finds thousands of R1+R2-consistent, role-0-developable
    * patterns whose role-1 development clashes — the developability gap — and the both-roles-consistent
    * gauges sit arbitrarily deep in the DFS order.
    */
  /** The GAUGE SLOTS of a cloud/anchor configuration: per (role, star-slot) the full letter-valid candidate
    * placements, in the ORIGINAL alignment order (index 0 = the historical pre-reconciliation choice). The
    * shared substrate of `directPattern`'s joint search and the gauge measurement probes.
    */
  private[research_core] def gaugeSlots(
      ctx: PairCtx,
      vs: Vector[(V3, Char)],
      anchors: Vector[(V3, Mat)],
      letterRole: Char => Int,
      symRadius: Double = 2.5
  ): Vector[((Int, Int), Vector[Pl])] =
    def dirsAt(v: V3): Vector[V3]                  = nbrs(vs, v).map((w, _) => unit(sub(w, v)))
    val candCache                                  = collection.mutable.Map.empty[(V3, V3), Vector[Mat]]
    def localSyms(vFrom: V3, vTo: V3): Vector[Mat] = candCache.getOrElseUpdate(
      (vFrom, vTo),
      alignments(dirsAt(vFrom), dirsAt(vTo), None)
        .map(_._2)
        .filter { l =>
          vs.forall { (p, c) =>
            val d = sub(p, vFrom)
            norm(d) > symRadius || {
              val q = add(vTo, l(d))
              vs.exists((p2, c2) => dist(p2, q) < 1e-3 && c2 == c)
            }
          }
        }
    )
    for
      r <- ctx.roles.toVector
      x <- ctx.g(r).u.indices.toVector
    yield
      val (v0r, m0r) = anchors(r)
      val ux         = ctx.g(r).u(x)
      val approx     = add(v0r, m0r(ux))
      val (w, cw)    = vs.minBy((p, _) => dist(p, approx))
      require(dist(w, approx) < 1e-4, s"no fixture vertex at slot $x of role $r")
      val sRole      = letterRole(cw)
      val (v0s, m0s) = anchors(sRole)
      val cands      = localSyms(v0s, w).flatMap { l =>
        val rot   = m0r.t * (l * m0s) // placed-model -> base-model
        val backM = rot.t((-ux._1, -ux._2, -ux._3))
        ctx.g(sRole).u.indices
          .find(i => dist(ctx.g(sRole).u(i), backM) < 1e-4)
          .map(y => Pl(ctx.spIdx(sRole), Glu(y, rotOf(rot))))
      }
      require(cands.nonEmpty, s"no local cloud symmetry onto the site of slot $x of role $r")
      ((r, x), cands)

  def directPattern(
      ctx: PairCtx,
      vs: Vector[(V3, Char)],
      anchors: Vector[(V3, Mat)],
      letterRole: Char => Int,
      symRadius: Double = 2.5
  ): PairPatterns.PPattern =
    // JOINT gauge consistency: letter validity per slot cannot pin the gauge — only certain
    // COMBINATIONS across slots describe one honeycomb. The arbiter is the full derivation gauntlet; the
    // search design is under measurement — the current interim
    // search is attempt #1 (historical) plus a bounded odometer.
    val slots                                                 = gaugeSlots(ctx, vs, anchors, letterRole, symRadius)
    val roleSlotIdx                                           = slots.map(_._1).zipWithIndex.toMap
    def patternOf(choice: Vector[Int]): PairPatterns.PPattern =
      PairPatterns.PPattern(ctx.roles.toVector.map { r =>
        ctx.g(r).u.indices.toVector.map { x =>
          val i = roleSlotIdx((r, x))
          slots(i)._2(choice(i))
        }
      })
    def consistent(pat: PairPatterns.PPattern): Boolean       =
      // the arbiter is the FULL derivation gauntlet (descent + bounded-face validity + minimality):
      // unfolded-reading involutivity is too strict -- the stabilizer fold legitimately absorbs frame
      // ambiguity INSIDE the site group, and only ambiguity OUTSIDE it (the k = 5 lesson) must reject
      scala.util.Try(PairRealization.derivedPairSymbol(ctx, pat)).isSuccess
    // GREEDY REPAIR, the search order MEASURED into shape: ambiguity is
    // universal (every slot, 2–12 candidates) and almost entirely fold-absorbed — the historical
    // assignment is consistent on every k ≤ 4 cloud and on most k = 5 tilings; where it is not (the
    // original k = 5 fixture) the defect is LOCAL (one slot, repaired by flipping it alone). So: attempt
    // the historical assignment, then all SINGLE deviations in slot order, then pairs, capped — never a
    // blind full odometer (its last-slot-fastest order provably misses early-slot repairs).
    val sizes                                                 = slots.map(_._2.size)
    val zero                                                  = Vector.fill(slots.size)(0)
    val amb                                                   = slots.indices.filter(sizes(_) > 1).toVector
    val singles                                               =
      for i <- amb.iterator; j <- (1 until sizes(i)).iterator yield zero.updated(i, j)
    val pairs                                                 =
      for
        ii <- amb.indices.iterator
        jj <- (ii + 1 until amb.size).iterator
        a  <- (1 until sizes(amb(ii))).iterator
        b  <- (1 until sizes(amb(jj))).iterator
      yield zero.updated(amb(ii), a).updated(amb(jj), b)
    val maxAttempts                                           = 4000
    var attempts                                              = 0
    var found: Option[PairPatterns.PPattern]                  = None
    val it                                                    = Iterator(zero) ++ singles ++ pairs
    while found.isEmpty && it.hasNext && attempts < maxAttempts do
      attempts += 1
      val pat = patternOf(it.next())
      if consistent(pat) then found = Some(pat)
    found.getOrElse(
      throw new IllegalStateException(
        s"no jointly consistent gauge assignment within $attempts attempts (singles + pairs)"
      )
    )

  /** The germ DOMAINS of `word` for the pair context (role 0 = c-species, role 1 = h-species): the generic
    * extraction on the Barlow stacking cloud. Requires ctx.spIdx to be (octet-c, octet-h) in this order.
    */
  def germDomains(ctx: PairCtx, word: String): Vector[Vector[Vector[Pl]]] =
    germDomainsOf(ctx, verticesOf(word, 7, 5), Vector('c', 'h'), c => if c == 'c' then 0 else 1)._1
