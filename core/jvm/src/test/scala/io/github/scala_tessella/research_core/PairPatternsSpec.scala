package io.github.scala_tessella.research_core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import PairPatterns.*
import PairShell.Pl
import SpeciesEnumerator.species
import TransitivePatterns.matOf

/** the pair-pattern machinery, piece by piece. The fast unguarded tests pin the SMALL machinery against edge
  * cases and against CERTIFIED TransitivePatterns output — the coset partition of the joint domains, the
  * per-role skeleton enumeration against brute force, the soundness of each skeleton-level filter
  * (species-count shell, R1-viability fixpoint, walk support, gauge pin), the role-hopping face walk, and the
  * whole pipeline on embedded TransitivePatterns germs (a certified mono-species pattern fed through the pair
  * search must be accepted and develop to the identical ball). The KNOWN-ANSWER anchors — Barlow {c,h} →
  * exactly 4 (4H, 6H, 9R, 12R), slab {c,e} and {h,e} → exactly 4 each per the slab-world classification — are
  * guarded behind -Dpairs.patterns (PairShellSpec-style); the census over the 33 fair pairs and its audit are
  * the census audit.
  */
class PairPatternsSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  // the slab species (slab-world labels): octet c/h, elongated e, parallel prisms p — as in PairShellSpec
  private lazy val Vector(octetH, octetC) = // #1 h (two distinct figures), #2 c (single figure)
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse
  private lazy val elongated              = bySupport("{tet:4 oct:3 p3:6}").head
  private lazy val prisms                 = bySupport("{p3:12}")
  private lazy val prismMix               = prisms.find(i => species(i).figures.exists(_._1.size == 5)).get
  private lazy val prismPar               = prisms.find(_ != prismMix).get

  private def jointDomains(ctx: PairCtx, flags: MonoShell.Flags): Vector[Vector[Vector[Pl]]] =
    Vector(0, 1).map { r =>
      ctx.g(r).u.indices.toVector.map { x =>
        PairShell.crossGluings(ctx.g(r), x, ctx.g(r), flags).map(Pl(ctx.spIdx(r), _)) ++
          PairShell.crossGluings(ctx.g(r), x, ctx.g(1 - r), flags).map(Pl(ctx.spIdx(1 - r), _))
      }
    }

  private def placedPositions(ctx: PairCtx, r: Int, x: Int, pl: Pl): Set[(Long, Long, Long)] =
    val u = ctx.g(r).u(x)
    ctx.g(ctx.roleOf(pl.sp)).u
      .map(z => pl.glu.rot(z))
      .map(p => TransitivePatterns.round4((p._1 + u._1, p._2 + u._2, p._3 + u._3)))
      .toSet

  private lazy val flagsCH  = MonoShell.Flags()
  private lazy val ctxCH    = ctxOf(octetC, octetH)
  private lazy val domsCH   = jointDomains(ctxCH, flagsCH)
  private lazy val cosetsCH = Vector(0, 1).map(r => domsCH(r).map(d => cosetsOf(ctxCH, d)))

  "the Barlow joint domains" should "have the observed sizes and stabilizer orders" in:
    domsCH(0).map(_.size) shouldBe Vector.fill(12)(72)
    domsCH(1).map(_.size) shouldBe Vector(12, 12, 72, 72, 72, 12, 72, 12, 72, 12, 72, 12)
    ctxCH.stab(0).size shouldBe 48
    ctxCH.stab(1).size shouldBe 12
    flagsCH.items.distinct shouldBe empty

  "the coset structure" should "partition each Barlow domain into species-homogeneous Stab-classes" in:
    for r <- 0 to 1; x <- domsCH(r).indices do
      val dom    = domsCH(r)(x)
      val cosets = cosetsCH(r)(x)
      withClue(s"role $r vertex $x: "):
        // partition
        cosets.flatten.size shouldBe dom.size
        cosets.flatten.toSet shouldBe dom.toSet
        // homogeneous species, members pairwise Stab-related, same placed star as a vertex set
        for c <- cosets do
          c.map(_.sp).distinct.size shouldBe 1
          val rep   = c.head
          val stabP = ctxCH.stab(ctxCH.roleOf(rep.sp))
          val posR  = placedPositions(ctxCH, r, x, rep)
          for m <- c do
            assert(TransitivePatterns.inStab(matOf(rep.glu.rot).t * matOf(m.glu.rot), stabP))
            placedPositions(ctxCH, r, x, m) shouldBe posR
        // distinct cosets of the SAME species are not Stab-related
        for
          (a, i) <- cosets.zipWithIndex
          (b, j) <- cosets.zipWithIndex
          if i < j && a.head.sp == b.head.sp
        do
          assert(
            !TransitivePatterns
              .inStab(matOf(a.head.glu.rot).t * matOf(b.head.glu.rot), ctxCH.stab(ctxCH.roleOf(a.head.sp)))
          )

  it should "give 3 cosets per c-vertex (one 48-member c-coset, two 12-member h-cosets)" in:
    for x <- cosetsCH(0).indices do
      val sizes = cosetsCH(0)(x).map(c => (c.head.sp == octetC, c.size)).sorted
      sizes shouldBe Vector((false, 12), (false, 12), (true, 48)).sorted

  "roleSkeletons" should "agree with brute force over the h-role's coset choices" in:
    val cosets                                          = cosetsCH(1)
    val (skels, capped)                                 = roleSkeletons(ctxCH, 1, cosets, flagsCH)
    capped shouldBe false
    // brute force: every mixed, pairwise-compatible coset choice
    def choiceOf(skel: Vector[Vector[Pl]]): Vector[Int] =
      skel.zipWithIndex.map((c, v) => cosets(v).indexOf(c))
    val all                                             = cosets.indices.foldLeft(Vector(Vector.empty[Int])) { (acc, v) =>
      acc.flatMap(p => cosets(v).indices.map(ci => p :+ ci))
    }
    val bruteSkels                                      = all.filter { p =>
      val mixed      = p.zipWithIndex.exists((ci, v) => cosets(v)(ci).head.sp != ctxCH.spIdx(1))
      def compatible = p.indices.forall { v =>
        (0 until v).forall { v2 =>
          PairShell.compatibleP(ctxCH.g(1), v, cosets(v)(p(v)).head, v2, cosets(v2)(p(v2)).head, flagsCH)
        }
      }
      mixed && compatible
    }
    skels.map(choiceOf).toSet shouldBe bruteSkels.toSet
    skels.foreach { skel =>
      assert(skel.exists(_.head.sp != ctxCH.spIdx(1)), "every skeleton must be mixed")
    }

  it should "enumerate the observed 34 x 3 Barlow skeletons, all mixed and pairwise-compatible" in:
    val (s0, c0) = roleSkeletons(ctxCH, 0, cosetsCH(0), flagsCH)
    val (s1, c1) = roleSkeletons(ctxCH, 1, cosetsCH(1), flagsCH)
    (c0, c1) shouldBe (false, false)
    s0.size shouldBe 34
    s1.size shouldBe 3
    for (skels, r) <- Vector(s0 -> 0, s1 -> 1) do
      skels.foreach { skel =>
        assert(skel.exists(_.head.sp != ctxCH.spIdx(r)))
        for v <- skel.indices; v2 <- 0 until v do
          assert(PairShell.compatibleP(ctxCH.g(r), v, skel(v).head, v2, skel(v2).head, flagsCH))
      }

  "the species-count shell filter" should "kill a Barlow skeleton with a lone cross neighbor" in:
    // 11 c + 1 h around the c-star is impossible: the trios above and below a vertex lie in one layer
    val cCoset  = (x: Int) => cosetsCH(0)(x).find(_.head.sp == octetC).get
    val hCoset  = (x: Int) => cosetsCH(0)(x).find(_.head.sp == octetH).get
    val hRole   = cosetsCH(1).map(_.head) // any homogeneous role-1 choice with one cross
    val hRoleX  = hRole.zipWithIndex.map((c, v) =>
      if v == cosetsCH(1).indexWhere(_.exists(_.head.sp == octetC)) then
        cosetsCH(1)(v).find(_.head.sp == octetC).get
      else c
    )
    val badSkel = Vector(
      domsCH(0).indices.toVector.map(x => if x == 0 then hCoset(x) else cCoset(x)),
      hRoleX
    )
    speciesShellOk(ctxCH, badSkel) shouldBe false

  it should "keep at least one enumerated Barlow joint skeleton (the filter is not vacuously false)" in:
    val (s0, _)   = roleSkeletons(ctxCH, 0, cosetsCH(0), flagsCH)
    val (s1, _)   = roleSkeletons(ctxCH, 1, cosetsCH(1), flagsCH)
    val survivors = (for a <- s0; b <- s1 yield Vector(a, b)).count(speciesShellOk(ctxCH, _))
    withClue(s"survivors of ${s0.size * s1.size}: ")(survivors should be > 0)
    info(s"joint skeletons surviving the species-count filter: $survivors of ${s0.size * s1.size}")

  it should "be vacuously true on the raw (species-mixed) joint domains" in:
    speciesShellOk(ctxCH, domsCH) shouldBe true

  "the species-arrangement filter" should "cut the Barlow joint product to the axis-coherent 36 of 102" in:
    // the decorated-shell invariant one level deeper: a placed star's tiling-vertex z carries the species
    // its own role's skeleton assigns to z, so coinciding positions force skeleton agreement — only the
    // geometrically coherent cross distributions survive, with drastically cut domains
    val (s0, _) = roleSkeletons(ctxCH, 0, cosetsCH(0), flagsCH)
    val (s1, _) = roleSkeletons(ctxCH, 1, cosetsCH(1), flagsCH)
    val joint   = for a <- s0; b <- s1 yield Vector(a, b)
    val alive   = joint.filter(speciesArrangementFilter(ctxCH, _).isDefined)
    alive.size shouldBe 36
    // the surviving domains are small: no slot keeps more than 12 of its up-to-48 members
    for skel <- alive; d <- speciesArrangementFilter(ctxCH, skel) do
      d.flatten.map(_.size).max should be <= 12

  it should "be vacuously true on the raw joint domains and keep all-same skeletons" in:
    speciesArrangementFilter(ctxCH, domsCH) shouldBe Some(domsCH)

  // the fixture pattern, its class key, and its coset skeleton, per Barlow word (cached — several tests
  // consume these)
  private val fixtureCache                                                              =
    collection.mutable.Map.empty[String, (PPattern, (Fp, Fp), Vector[Vector[Vector[Pl]]])]
  private def fixtureOf(word: String): (PPattern, (Fp, Fp), Vector[Vector[Vector[Pl]]]) =
    fixtureCache.getOrElseUpdate(
      word, {
        val doms        = BarlowFixtures.germDomains(ctxCH, word)
        val found       = collection.mutable.ArrayBuffer.empty[PPattern]
        searchPairPatterns(ctxCH, doms, 40, found += _)
        val (pat, ball) = found.iterator.flatMap(p => developBall(ctxCH, p, 3.05).map(p -> _)).next()
        val key         = (fingerprintOf(ctxCH, ball, 2.05), fingerprintOf(ctxCH, ball, 3.05))
        val skel        = Vector(0, 1).map { r =>
          pat.p(r).zipWithIndex.map { (pl, x) =>
            cosetsCH(r)(x)
              .find(c =>
                c.head.sp == pl.sp &&
                  TransitivePatterns.inStab(
                    matOf(c.head.glu.rot).t * matOf(pl.glu.rot),
                    ctxCH.stab(ctxCH.roleOf(pl.sp))
                  )
              )
              .get
          }
        }
        (pat, key, skel)
      }
    )
  private type Fp = Vector[(Long, Long, Long)]

  // IGNORED — the recorded PairPatterns enumeration gap: the per-skeleton pattern DFS drowns in
  // R1+R2-consistent but non-developable patterns and does not reach the genuine classes within any
  // defensible budget; the census route is the 3D Delaney-Dress symbol machinery, with this test as the
  // minimal reproduction kept for the pattern machinery's record.
  "the full-skeleton search" should
    "find each Barlow fixture's class inside the fixture's own skeleton" ignore:
      // the minimal reproduction of the anchor gap: the skeleton is enumerated and filter-surviving (tested
      // below), the class provably lives in it (the fixture), so the search must reach it
      for word <- BarlowFixtures.words do
        val (_, key, skel) = fixtureOf(word)
        var hit            = false
        var emitted        = 0
        var rejected       = 0
        searchPairPatterns(
          ctxCH,
          skel,
          4000,
          pat => {
            emitted += 1
            developBall(ctxCH, pat, 3.05) match
              case None       => rejected += 1
              case Some(ball) =>
                if !hit then
                  hit = (fingerprintOf(ctxCH, ball, 2.05), fingerprintOf(ctxCH, ball, 3.05)) == key
          },
          nodeBudget = 4000000L
        )
        info(s"word $word: emitted $emitted, rejected $rejected, class found: $hit")
        withClue(s"word $word: ")(hit shouldBe true)

  "the gauge pin" should "pin exactly one maximal homogeneous cross slot and nothing else" in:
    val (s0, _)  = roleSkeletons(ctxCH, 0, cosetsCH(0), flagsCH)
    val (s1, _)  = roleSkeletons(ctxCH, 1, cosetsCH(1), flagsCH)
    val skel     = Vector(s0.head, s1.head)
    val pinnedD  = gaugePin(ctxCH, skel)
    val diffs    =
      for
        r <- 0 to 1
        v <- skel(r).indices
        if pinnedD(r)(v) != skel(r)(v)
      yield (r, v)
    diffs.size shouldBe 1
    val (pr, pv) = diffs.head
    pinnedD(pr)(pv).size shouldBe 1
    skel(pr)(pv).head.sp should not be ctxCH.spIdx(pr)
    // maximality: no unpinned homogeneous cross slot is larger
    val maxCross = (for
      r <- 0 to 1
      v <- skel(r).indices
      if skel(r)(v).head.sp != ctxCH.spIdx(r)
    yield skel(r)(v).size).max
    skel(pr)(pv).size shouldBe maxCross

  it should "leave all-same domains unchanged" in:
    val allSame = Vector(
      domsCH(0).indices.toVector.map(x => cosetsCH(0)(x).find(_.head.sp == octetC).get),
      domsCH(1).indices.toVector.map(x => cosetsCH(1)(x).find(_.head.sp == octetH).get)
    )
    gaugePin(ctxCH, allSame) shouldBe allSame

  "the face walk" should "hop into the other role's pattern at a cross placement" in:
    val flags = MonoShell.Flags()
    val x0    = 0
    val cross = PairShell.crossGluings(ctxCH.g(0), x0, ctxCH.g(1), flags)
    cross should not be empty
    val pl    = Pl(octetH, cross.head)
    val key   = ctxCH.arcKeys(0).find(_._1 == x0).get
    // with only (0, x0) assigned to a cross placement, the walk's next demand must be a ROLE-1 slot
    // (the placed star is an h-star, so the continuation reads role 1's pattern)
    walkFace(ctxCH, (r, v) => if r == 0 && v == x0 then Some(pl) else None, 0, key) match
      case Left((r, v)) =>
        r shouldBe 1
        v should (be >= 0 and be < ctxCH.g(1).u.size)
      case other        => fail(s"expected a role-1 demand, got $other")
    flags.items.distinct shouldBe empty

  "an embedded TransitivePatterns germ" should
    "pass the whole pair pipeline and develop to the identical ball" in:
      // fast tier: the two (forced, cheap) octet orientations; the multi-coset elongated~prism pair — whose
      // TransitivePatterns acceptance is itself an enumeration — runs in the guarded battery below
      embeddedGermCheck(Vector((octetC, octetH), (octetH, octetC)))

  it should "also hold for the multi-coset elongated ~ prism pair (-Dpairs.patterns)" in:
    assume(
      sys.props.contains("pairs.patterns"),
      "heavy: acceptedOf(elongated) enumerates TransitivePatterns skeletons"
    )
    embeddedGermCheck(Vector((elongated, prismPar)))

  private def embeddedGermCheck(pairs: Vector[(Int, Int)]): Unit =
    val flags = MonoShell.Flags()
    for (i, j) <- pairs do
      val ctx  = ctxOf(i, j)
      val accI = TransitivePatterns.acceptedOf(i, flags)
      val accJ = TransitivePatterns.acceptedOf(j, flags)
      val patI = accI.patterns.head._2
      val patJ = accJ.patterns.head._2
      val doms = Vector(
        patI.glus.map(glu => Vector(Pl(i, glu))),
        patJ.glus.map(glu => Vector(Pl(j, glu)))
      )
      withClue(s"${SpeciesCorona.label(i)} ~ ${SpeciesCorona.label(j)}: "):
        // each filter is sound: the embedded certified germ survives untouched
        speciesShellOk(ctx, doms) shouldBe true
        r1ViabilityFixpoint(ctx, doms) shouldBe Some(doms)
        walkSupportFixpoint(ctx, doms) shouldBe Some(doms)
        gaugePin(ctx, doms) shouldBe doms // all-same: nothing to pin
        // the search accepts exactly the embedding (mixedness off: this is a k = 1 germ)
        val found         = collection.mutable.ArrayBuffer.empty[PPattern]
        val (n, capped)   = searchPairPatterns(ctx, doms, 2, found += _, requireMixed = false)
        (n, capped) shouldBe (1, false)
        found.head.p(0).map(_.glu) shouldBe patI.glus
        found.head.p(1).map(_.glu) shouldBe patJ.glus
        // and the pair development reproduces TransitivePatterns' certified ball exactly (all in role 0)
        val ball          = developBall(ctx, found.head, 3.05)
        ball should not be empty
        val Some(entries) = ball: @unchecked
        entries.map(_._1).distinct shouldBe Vector(0)
        val g3bBall       = TransitivePatterns.developBall(accI.g, patI, accI.stab, 3.05).get
        entries.map((_, t) => TransitivePatterns.round4(t.t)).toSet shouldBe
          g3bBall.map(t => TransitivePatterns.round4(t.t)).toSet
    flags.items.distinct shouldBe empty

  it should "be rejected when mixedness is required (a k = 1 germ is not a k = 2 pattern)" in:
    val flags       = MonoShell.Flags()
    val ctx         = ctxOf(octetC, octetH)
    val patC        = TransitivePatterns.acceptedOf(octetC, flags).patterns.head._2
    val patH        = TransitivePatterns.acceptedOf(octetH, flags).patterns.head._2
    val doms        = Vector(
      patC.glus.map(glu => Vector(Pl(octetC, glu))),
      patH.glus.map(glu => Vector(Pl(octetH, glu)))
    )
    val (n, capped) = searchPairPatterns(ctx, doms, 2, _ => ())
    (n, capped) shouldBe (0, false)

  // ---------- the Barlow word fixtures: independent known answers (fixture-first) ----------

  private def atlasCheck(word: String): Unit =
    val flags = MonoShell.Flags()
    val doms  = BarlowFixtures.germDomains(ctxCH, word)
    withClue(s"word $word: "):
      for r <- 0 to 1; x <- doms(r).indices do
        val d = doms(r)(x)
        withClue(s"role $r slot $x: "):
          d should not be empty
          d.map(_.sp).distinct.size shouldBe 1
          val atlas = PairShell.crossGluings(ctxCH.g(r), x, ctxCH.g(ctxCH.roleOf(d.head.sp)), flags)
          d.foreach { pl =>
            assert(
              atlas.exists(g => g.y == pl.glu.y && matOf(g.rot).dist(matOf(pl.glu.rot)) < 1e-4),
              "fixture placement not found in the cross atlas"
            )
          }
    flags.items.distinct shouldBe empty

  "the Barlow word fixtures" should "pass the cch canary (a class the pattern search missed)" in:
    // fast tier: one word exercises the whole fixture pipeline — geometric germ extraction, atlas
    // membership, consistent pattern found, development accepted (fixtureOf throws on any failure)
    atlasCheck("cch")
    fixtureOf("cch")._2._1 should not be empty

  it should "sit in the atlas for all four words, fingerprinting apart (-Dpairs.patterns)" in:
    assume(sys.props.contains("pairs.patterns"), "full four-word battery — enable with -Dpairs.patterns")
    BarlowFixtures.words.filterNot(_ == "cch").foreach(atlasCheck)
    val keys = BarlowFixtures.words.map(w => fixtureOf(w)._2)
    withClue("the four Barlow classes must be fingerprint-distinct: ")(keys.distinct.size shouldBe 4)

  it should "have coset skeletons that the enumeration produces and every filter keeps (-Dpairs.patterns)" in:
    assume(sys.props.contains("pairs.patterns"), "full four-word battery — enable with -Dpairs.patterns")
    val (s0, _) = roleSkeletons(ctxCH, 0, cosetsCH(0), flagsCH)
    val (s1, _) = roleSkeletons(ctxCH, 1, cosetsCH(1), flagsCH)
    for word <- BarlowFixtures.words do
      val (_, _, skel) = fixtureOf(word)
      withClue(s"word $word: "):
        assert(s0.contains(skel(0)), "role-0 skeleton of the fixture is not enumerated")
        assert(s1.contains(skel(1)), "role-1 skeleton of the fixture is not enumerated")
        speciesShellOk(ctxCH, skel) shouldBe true
        speciesArrangementFilter(ctxCH, skel) should not be None
        r1ViabilityFixpoint(ctxCH, skel) should not be None
        walkSupportFixpoint(ctxCH, skel) should not be None

  // ---------- the known-answer anchors (heavy; enable with -Dpairs.patterns) ----------

  private def checkAnchor(i: Int, j: Int, expected: Int): Report =
    val flags = MonoShell.Flags()
    val r     = analyze(i, j, flags)
    withClue(s"${SpeciesCorona.label(i)} ~ ${SpeciesCorona.label(j)}: "):
      r.honeycombs shouldBe expected
      r.stableAtR3 shouldBe true
      flags.items.distinct shouldBe empty
    r

  "the Barlow pair {c,h}" should "yield exactly the 4 stackings 4H, 6H, 9R, 12R (-Dpairs.patterns)" in:
    assume(sys.props.contains("pairs.patterns"), "heavy anchor — enable with -Dpairs.patterns")
    checkAnchor(octetC, octetH, 4)

  "the slab pair {c,e}" should
    "yield exactly 4 classes, per the slab-world classification (-Dpairs.patterns)" in:
      assume(sys.props.contains("pairs.patterns"), "heavy anchor — enable with -Dpairs.patterns")
      checkAnchor(octetC, elongated, 4)

  "the slab pair {h,e}" should
    "yield exactly 4 classes, per the slab-world classification (-Dpairs.patterns)" in:
      assume(sys.props.contains("pairs.patterns"), "heavy anchor — enable with -Dpairs.patterns")
      checkAnchor(octetH, elongated, 4)
