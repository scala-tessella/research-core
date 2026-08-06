package io.github.scala_tessella.research_core

import MonoShell.{Flags, Glu, Vec}
import TransitivePatterns.*

/** The completeness audit — the finite certificates that upgrade [[TransitivePatterns]]'s finite-radius
  * enumeration to a classification statement.
  *
  * The audited claims, closing the enumeration's four gaps:
  *
  * (A) PERIODIZATION CERTIFICATE — every accepted pattern develops a genuine transitive honeycomb. For a
  * class representative: find three linearly independent TRANSLATIONS as words in the pattern's gluings
  * (rotational part = identity), verify on the developed ball that (i) the ball data is Λ-periodic (every
  * vertex translated by ±τᵢ within range has a Stab-matching star), (ii) Λ is invariant under the rotational
  * parts of all generators (so γ(H*) is Λ-periodic for every generator γ), and (iii) the verified radius
  * covers a fundamental domain with margin: R_per ≥ covBound(Λ) + max|τ| + 1.5, where covBound = half the
  * circumdiameter of the fundamental parallelepiped. Then the Λ-periodization H* of the ball data is a
  * well-defined honeycomb (every point of space is Λ-equivalent to a verified region; local structure is
  * everywhere a translate of verified structure), the generators map H* to itself (Λ-periodic images agreeing
  * on a fundamental box are equal), Γ = ⟨gluings⟩ ∋ Λ acts vertex-transitively, and the development equals
  * H*.
  *
  * (B) CLASS COHERENCE — same fingerprint ⇒ same honeycomb. A certified honeycomb is DETERMINED by its ball
  * of radius covBound + 1.5 together with Λ (periodization). For every accepted pattern, align its ball to
  * its class representative's by the canonical-encoding element of Stab(S) and verify EXACT agreement out to
  * the determination radius, and verify the pattern's ball is periodic under the representative's lattice:
  * two Λ-periodic honeycombs agreeing on a fundamental box are equal.
  *
  * (C) CLASS SEPARATION — different fingerprint ⇒ different honeycomb: the fingerprinted set is INTRINSIC
  * (vertices reachable by edge-paths of ≤ 14 steps staying within R + 1.6, restricted to the R-ball), so the
  * canonical fingerprint is a congruence invariant — congruent honeycombs (reflections included, by the
  * rebasing argument: any congruence normalizes to an element of Stab±(S)) have equal fingerprints. No
  * computation needed beyond [[TransitivePatterns]]'s.
  *
  * (D) GERM FORCING — the pattern caps hide nothing. Per skeleton, the 1-shell GERM (the star plus all
  * neighbor-star placements) is tested to FORCE the germ of every neighbor: for each tiling-vertex w,
  * enumerate all isometries taking the germ onto a configuration at u_w whose star matches and whose
  * placements agree with everything already known around w; if all of them induce the SAME placements at w's
  * unknown neighbors, the germ propagates uniquely. Since every vertex of a transitive honeycomb carries a
  * congruent germ, a forcing germ admits AT MOST ONE honeycomb — so a skeleton with any accepted pattern
  * carries exactly its certified honeycomb, caps notwithstanding, and a skeleton exhausted without an
  * accepted pattern (search uncapped in that case) carries none.
  *
  * These four certificates are what a completeness argument consumes on top of the cell alphabet, the species
  * table, the shell filter and the pattern enumeration; the argument itself is stated and asserted by the
  * verification repository of the result, not here.
  */
object CompletenessAudit:

  private def vSub(a: Vec, b: Vec): Vec      = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def vAdd(a: Vec, b: Vec): Vec      = (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private def vDot(a: Vec, b: Vec): Double   = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def vNorm(a: Vec): Double          = math.sqrt(vDot(a, a))
  private def vDist(a: Vec, b: Vec): Double  = vNorm(vSub(a, b))
  private def vScale(a: Vec, s: Double): Vec = (a._1 * s, a._2 * s, a._3 * s)

  private val idMat = Mat((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0))

  private def isIdentity(m: Mat): Boolean = m.dist(idMat) < 1e-6

  /** Translation words of length ≤ maxLen over the pattern's gluings whose rotational part is the identity
    * and whose translation is nonzero; deduplicated by translation vector.
    */
  private def translationWords(pat: Pattern, maxLen: Int): Vector[Vec] =
    val gens                        = pat.g.u.indices.map(pat.iso).toVector
    val found                       = collection.mutable.ArrayBuffer.empty[Vec]
    def rec(t: Iso, len: Int): Unit =
      if isIdentity(t.m) && vNorm(t.t) > 1e-4 && !found.exists(v => vDist(v, t.t) < 1e-4) then
        found += t.t
      if len < maxLen then gens.foreach(g => rec(t.compose(g), len + 1))
    rec(Iso(idMat, (0.0, 0.0, 0.0)), 0)
    found.toVector.sortBy(vNorm)

  /** A short independent basis from the found translations (greedy by norm, then one reduction sweep). */
  def latticeBasis(ts: Vector[Vec]): Option[(Vec, Vec, Vec)] =
    def det(a: Vec, b: Vec, c: Vec): Double =
      a._1 *
        (b._2 * c._3 - b._3 * c._2) -
        a._2 *
        (b._1 * c._3 - b._3 * c._1) +
        a._3 *
        (b._1 * c._2 - b._2 * c._1)
    for
      t1 <- ts.headOption
      t2 <- ts.find(t => vNorm(vSub(t, vScale(t1, vDot(t, t1) / vDot(t1, t1)))) > 1e-3)
      t3 <- ts.find(t => math.abs(det(t1, t2, t)) > 1e-3)
    yield (t1, t2, t3)

  /** Integer lattice coordinates of v in basis (t1,t2,t3), if v ∈ Λ within tolerance. */
  def inLattice(basis: (Vec, Vec, Vec), v: Vec): Boolean =
    val (a, b, c)                             = basis
    val d                                     = a._1 *
      (b._2 * c._3 - b._3 * c._2) -
      a._2 *
      (b._1 * c._3 - b._3 * c._1) +
      a._3 *
      (b._1 * c._2 - b._2 * c._1)
    def solve(x: Vec, y: Vec, w: Vec): Double = // Cramer component
      (w._1 *
        (x._2 * y._3 - x._3 * y._2) -
        w._2 *
        (x._1 * y._3 - x._3 * y._1) +
        w._3 *
        (x._1 * y._2 - x._2 * y._1)) / d
    val (ca, cb, cc)                          = (solve(b, c, v), solve(c, a, v), solve(a, b, v))
    def isInt(x: Double)                      = math.abs(x - math.round(x)) < 1e-3
    isInt(ca) && isInt(cb) && isInt(cc)

  final case class Certificate(
      classId: Int,
      tau: (Vec, Vec, Vec),
      covBound: Double,
      rPer: Double,
      periodic: Boolean,         // ball data Λ-periodic under all ±τᵢ
      latticeInvariant: Boolean, // ρ(g_x)(τⱼ) ∈ Λ for all generators and basis vectors
      coverage: Boolean          // rPer ≥ covBound + max|τ| + 1.5
  ):
    def ok: Boolean = periodic && latticeInvariant && coverage

  /** The periodization certificate for one pattern: translations, periodicity of the developed ball,
    * generator-invariance of the lattice, coverage arithmetic.
    */
  private def certify(acc: Accepted, pat: Pattern, classId: Int, flags: Flags): Option[Certificate] =
    LazyList(4, 5, 6, 7).map(l => latticeBasis(translationWords(pat, l))).collectFirst { case Some(b) =>
      b
    }.map { basis =>
      val (t1, t2, t3) = basis
      val maxT         = Vector(t1, t2, t3).map(vNorm).max
      val covBound     =
        (for
          e1 <- Vector(1.0, -1.0)
          e2 <- Vector(1.0, -1.0)
          e3 <- Vector(1.0, -1.0)
        yield vNorm(vAdd(vAdd(vScale(t1, e1), vScale(t2, e2)), vScale(t3, e3)))).max / 2.0
      val rPer         = covBound + maxT + 1.6
      val ball         = developBall(acc.g, pat, acc.stab, rPer)
      val periodic     = ball.exists { entries =>
        val byPos = entries.map(t => TransitivePatterns.round4(t.t) -> t).toMap
        entries.forall { t =>
          Vector(t1, t2, t3).flatMap(tau => Vector(tau, vScale(tau, -1.0))).forall { tau =>
            val q = vAdd(t.t, tau)
            if vNorm(q) > rPer - 1e-6 then true
            else
              byPos.get(TransitivePatterns.round4(q)) match
                case None      => false
                case Some(t2i) =>
                  t2i.m.dist(t.m) < 1e-4 || acc.stab.exists(s => (t2i.m.t * t.m).dist(s) < 1e-3)
          }
        }
      }
      val latInv       = acc.g.u.indices.forall { x =>
        val rot = matOf(pat.glus(x).rot)
        Vector(t1, t2, t3).forall(tau => inLattice(basis, rot(tau)))
      }
      Certificate(classId, basis, covBound, rPer, periodic, latInv, rPer >= covBound + maxT + 1.5)
    }

  /** Class coherence: the pattern's ball, canonically encoded at the determination radius, equals the class
    * representative's — with the representative's lattice periodicity holding on the pattern's ball too.
    */
  private def coheres(acc: Accepted, pat: Pattern, rep: Pattern, cert: Certificate): Boolean =
    val rDet = cert.covBound + 1.5
    (developBall(acc.g, pat, acc.stab, rDet), developBall(acc.g, rep, acc.stab, rDet)) match
      case (Some(b1), Some(b2)) =>
        fingerprintOf(acc.corners, acc.stab, b1, rDet) == fingerprintOf(acc.corners, acc.stab, b2, rDet)
      case _                    => false // collision at the determination radius: not certifiable

  /** The germ-forcing test for one skeleton: the 1-shell germ (star + neighbor placements) forces the germ of
    * every neighbor — then at most one honeycomb carries this germ, and the pattern caps hide nothing.
    */
  private def germForces(acc: Accepted, skeleton: Vector[Vector[Glu]], flags: Flags): Boolean =
    val g                   = acc.g
    val stab                = acc.stab
    val placements          = skeleton.map(_.head) // placement per vertex (coset representative)
    // the germ's data: at position u_x, the star placed by rotation matOf(placements(x).rot)
    def starAt(x: Int): Mat = matOf(placements(x).rot)
    g.u.indices.forall { w =>
      // isometries taking the germ onto vertex u_w: m = gluing rotation q at w times any stabilizer;
      // each maps the germ's neighbor set {0 + q·s(u_z)} with star rotations q·s·starAt(z)
      val q          = starAt(w)
      val candidates = acc.stab.map(s => q * s)
      // known data around w: the center star (rotation id at position 0) and the shell placements at
      // positions u_x; a candidate m is admissible iff every germ-neighbor it places at a KNOWN position
      // carries the known star; forced iff all admissible candidates place identical stars at the
      // UNKNOWN positions (the shell-2 vertices around w)
      val placedByC  = candidates.map { m =>
        g.u.indices.map { z =>
          val pos = vAdd(g.u(w), m(g.u(z)))
          (TransitivePatterns.round4(pos), m * starAt(z))
        }
      }
      val admissible = placedByC.filter { placed =>
        placed.forall { (pos, rot) =>
          if pos == TransitivePatterns.round4((0.0, 0.0, 0.0)) then
            rot.dist(idMat) < 1e-4 || stab.exists(s => (rot.t * idMat).dist(s) < 1e-3)
          else
            g.u.indices.find(x => TransitivePatterns.round4(g.u(x)) == pos) match
              case Some(x) =>
                rot.dist(starAt(x)) < 1e-4 || stab.exists(s => (rot.t * starAt(x)).dist(s) < 1e-3)
              case None    => true // unknown position: no constraint, contributes to the forcing test
        }
      }
      admissible.nonEmpty && {
        // all admissible candidates must agree at every unknown position they populate
        val unknowns = admissible.map { placed =>
          placed
            .filter((pos, _) =>
              pos != TransitivePatterns.round4((0.0, 0.0, 0.0)) &&
                !g.u.indices.exists(x => TransitivePatterns.round4(g.u(x)) == pos)
            )
            .sortBy(_._1)
        }
        unknowns.forall { u =>
          u.size == unknowns.head.size &&
          u.zip(unknowns.head).forall { case ((p1, r1), (p2, r2)) =>
            p1 == p2 && (r1.dist(r2) < 1e-4 || stab.exists(s => (r1.t * r2).dist(s) < 1e-3))
          }
        }
      }
    }

  /** Is the isometry a symmetry of the certified honeycomb? Tested on the certified periodic ball: every ball
    * vertex whose image stays within the ball must map onto a ball vertex with a Stab-matching star.
    */
  private def isSymmetry(acc: Accepted, ball: Vector[Iso], rPer: Double, iso: Iso): Boolean =
    val byPos = ball.map(t => TransitivePatterns.round4(t.t) -> t).toMap
    ball.forall { t =>
      val img = iso.compose(t)
      if vNorm(img.t) > rPer - 1e-6 then true
      else
        byPos.get(TransitivePatterns.round4(img.t)) match
          case None      => false
          case Some(t2i) =>
            t2i.m.dist(img.m) < 1e-4 || acc.stab.exists(s => (t2i.m.t * img.m).dist(s) < 1e-3)
    }

  /** Exhaustion certificate for a non-forcing skeleton: enumerate ALL R1+R2-passing patterns (uncapped),
    * develop each; every accepted one must fingerprint into a KNOWN certified class (the canonicalization
    * over Stab identifies rebased copies of the same honeycomb) and cohere with that class representative at
    * the determination radius — then no honeycomb hides beyond any cap.
    */
  private def exhaustSkeleton(
      acc: Accepted,
      si: Int,
      classData: Vector[(Vector[(Long, Long, Long)], Pattern, Certificate)],
      flags: Flags
  ): Boolean =
    val domains     = acc.skeletons(si)
    var allKnown    = true
    val (_, capped) = TransitivePatterns.searchPatterns(
      acc.g,
      domains,
      acc.stab,
      500000,
      glus => {
        val pat = Pattern(acc.g, glus)
        developBall(acc.g, pat, acc.stab, 3.05) match
          case None       => ()
          case Some(ball) =>
            val f2 = fingerprintOf(acc.corners, acc.stab, ball, 2.05)
            classData.find(_._1 == f2) match
              case Some((_, rep, cert)) => if !coheres(acc, pat, rep, cert) then allKnown = false
              case None                 => allKnown = false
      }
    )
    if capped || !allKnown then
      flags.add(s"exhaustion failure: skeleton $si capped=$capped allKnown=$allKnown")
    !capped && allKnown

  private[research_core] def debugTranslations(pat: Pattern, l: Int): Vector[Vec]           = translationWords(pat, l)
  private[research_core] def debugCertify(acc: Accepted, pat: Pattern): Option[Certificate] =
    certify(acc, pat, 0, new Flags)

  // ---------- the audit driver ----------

  final case class Audit(
      idx: Int,
      classes: Int,
      certified: Int,    // classes with a full periodization certificate
      coherent: Boolean, // every accepted pattern coheres with its class representative
      forcingSkeletons: Int,
      skeletonsWithPatterns: Int
  ):
    def ok: Boolean = certified == classes && coherent && forcingSkeletons == skeletonsWithPatterns

  def audit(idx: Int, flags: Flags): Audit =
    val acc       = acceptedOf(idx, flags)
    // fingerprint classes at the pattern radii
    val byClass   = collection.mutable.LinkedHashMap.empty[Vector[(Long, Long, Long)], Vector[(Int, Pattern)]]
    for (si, pat) <- acc.patterns do
      val ball = developBall(acc.g, pat, acc.stab, 3.05).get
      val f2   = fingerprintOf(acc.corners, acc.stab, ball, 2.05)
      byClass(f2) = byClass.getOrElse(f2, Vector.empty) :+ (si, pat)
    val classes   = byClass.toVector
    var certOk    = 0
    var allCoh    = true
    val classData = Vector.newBuilder[(Vector[(Long, Long, Long)], Pattern, Certificate)]
    classes.zipWithIndex.foreach { case ((fp, members), ci) =>
      val rep = members.head._2
      certify(acc, rep, ci, flags) match
        case Some(cert) if cert.ok =>
          certOk += 1
          classData += ((fp, rep, cert))
          if !members.forall((_, p) => coheres(acc, p, rep, cert)) then allCoh = false
        case _                     => ()
    }
    val cd        = classData.result()
    val skelsWith = acc.skeletons.indices.toVector // close EVERY skeleton, including capped-empty ones
    val closure   = skelsWith.map { si =>
      germForces(acc, acc.skeletons(si), flags) || exhaustSkeleton(acc, si, cd, flags)
    }
    Audit(idx, classes.size, certOk, allCoh, closure.count(identity), skelsWith.size)

  /** The full audit over the 26 species. */
  lazy val results: (Vector[Audit], Vector[String]) =
    val flags   = Flags()
    val (rs, _) = MonoShell.results
    val sat     = rs.filter(_._2.sat).map(_._1)
    (sat.map(i => audit(i, flags)), flags.items.distinct.toVector)
