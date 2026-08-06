package io.github.scala_tessella.research_core

import MonoShell.{ringDescriptors, Flags, StarGeom, Vec}
import SpeciesEnumerator.species
import StarChambers.Chamber
import StarFoldings.{fold, symmetryOf}
import SymbolCatalog.Sym
import TransitivePatterns.{developBall, fingerprintOf, Iso, Mat, Pattern}

/** The REALIZATION side of the k = 1 symbol gate — the minimal symbol of every certified honeycomb, derived
  * from its certified pattern, for key-for-key comparison with the combinatorial census of [[SymbolCatalog]].
  * The gate logic: the census holds exactly the minimal k = 1 symbols over the certified stars, and a genuine
  * honeycomb's minimal symbol provably appears in it (its vertex-orbit complex is the star's folding by the
  * vertex stabilizer); so census keys == derived keys certifies BOTH directions at once — every census symbol
  * is realized by a certified honeycomb, and every certified honeycomb's symbol is in the census.
  *
  * Derivation per honeycomb class: (1) the VERTEX STABILIZER Stab(v) = the star symmetries preserving the
  * certified development ball (positions and Stab-compatible frames — the same collision rule as the
  * development itself); (2) the star complex FOLDED by the corresponding chamber permutations (the stabilizer
  * matrices and `StarFoldings.symmetryOf`'s permutations share the deterministic `stabilizers` order); (3) σ₀
  * read off the certified pattern: the far flag of the chamber's edge, pulled back through the pattern's
  * placement, located by DESCRIPTOR RIGIDITY — the transported ring descriptors at the placed star's back
  * vertex match the chamber's cell type and face-germ pair uniquely, exactly the invariant MonoShell's atlas
  * is built on. Every derived symbol is asserted to satisfy the symbol axioms and minimality — a wrong
  * stabilizer or a wrong pullback fails loudly.
  */
object SymbolRealization:

  private def vSub(a: Vec, b: Vec): Vec                     = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def vDist(a: Vec, b: Vec): Double                 =
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

  /** Does the star symmetry s preserve the developed ball (positions + Stab-compatible frames)? */
  private def preservesBall(s: Mat, ball: Vector[Iso], stab: Vector[Mat]): Boolean =
    val index = ball.map(t => TransitivePatterns.round4(t.t) -> t).toMap
    ball.forall { t =>
      index.get(TransitivePatterns.round4(s(t.t))) match
        case None     => false
        case Some(t2) =>
          val m = s * t.m
          t2.m.dist(m) < 1e-3 || stab.exists(h => (t2.m.t * m).dist(h) < 1e-3)
    }

  /** σ₀ on the UNFOLDED star complex, read off the pattern: chamber -> the far flag of its edge pulled back
    * through the placement at its end vertex.
    */
  private def sigma0Unfolded(i: Int, g: StarGeom, pat: Pattern, flags: Flags): Vector[Int] =
    val cx    = StarChambers.complexOf(species(i))
    val st    = species(i).state
    val rings = st.positions.indices.map(x => x -> SpeciesCorona.ringAt(st, x)).toMap
    val descs = st.positions.indices.map(x => x -> ringDescriptors(g, x, identity)).toMap
    cx.chambers.indices.toVector.map { ci =>
      val ch                     = cx.chambers(ci)
      val x                      = cx.endVertex(ci)
      val glu                    = pat.glus(x)
      val y                      = glu.y
      // the chamber's ring entry at x: its corner's position in the ring, with the germ pair
      val ringX                  = rings(x)
      val kx                     = ringX.indexWhere(_._1 == ch.corner)
      require(kx >= 0, s"chamber corner not on its end vertex ring (species $i chamber $ci)")
      val (cellX, germsX)        = descs(x)(kx)
      // the matching ring entry at y of the placed star: transported descriptors, unique by rigidity
      val descY                  = ringDescriptors(g, y, glu.rot.apply)
      val ky                     = descY.indices.filter { k =>
        val (cellY, germsY) = descY(k)
        cellY == cellX && {
          (germEq(germsY(0), germsX(0)) && germEq(germsY(1), germsX(1))) ||
          (germEq(germsY(0), germsX(1)) && germEq(germsY(1), germsX(0)))
        }
      }
      require(ky.size == 1, s"far corner not unique (species $i chamber $ci: ${ky.size} matches)")
      val (cornerY, aInY, aOutY) = rings(y)(ky.head)
      // the chamber's own FACE germ (the germ of its arc, at the position within the germ pair)
      val arcX                   = // the chamber's arc as a vid pair
        val vids = st.corners(ch.corner).vids
        Set(vids(ch.side), vids((ch.side + 1) % vids.size))
      val (aInX, aOutX)          = (ringX(kx)._2, ringX(kx)._3)
      val faceIsIn               = Set(aInX._1, aInX._2) == arcX
      require(faceIsIn || Set(aOutX._1, aOutX._2) == arcX, s"chamber arc is neither ring arc")
      val germX                  = if faceIsIn then germsX(0) else germsX(1)
      // which of the far corner's two arcs carries the same transported germ
      val (cY, germsY)           = descY(ky.head)
      val arcY                   =
        if germEq(germsY(0), germX) then aInY
        else
          require(germEq(germsY(1), germX), s"far face germ not found (species $i chamber $ci)")
          aOutY
      // the pulled-back chamber: far corner, the side of arcY, the end at y
      val vidsY                  = st.corners(cornerY).vids
      val sideY                  = vidsY.indices
        .find(j => Set(vidsY(j), vidsY((j + 1) % vidsY.size)) == Set(arcY._1, arcY._2))
        .get
      val endY                   = if vidsY(sideY) == y then 0 else 1
      cx.index(Chamber(cornerY, sideY, endY))
    }

  /** The minimal symbols of species i's certified honeycombs: one per accepted pattern class. */
  def derivedSymbolsOf(i: Int, flags: Flags): Vector[Sym] =
    val acc     = TransitivePatterns.acceptedOf(i, flags)
    // one certified (pattern, ball) per honeycomb class, separated by the certified fingerprints
    val classes = collection.mutable.LinkedHashMap.empty[Vector[(Long, Long, Long)], (Pattern, Vector[Iso])]
    for (_, pat) <- acc.patterns do
      developBall(acc.g, pat, acc.stab, 3.05).foreach { ball =>
        val key = fingerprintOf(acc.corners, acc.stab, ball, 2.05)
        if !classes.contains(key) then classes(key) = (pat, ball)
      }
    val sym     = symmetryOf(i)
    require(sym.perms.size == acc.stab.size, "stabilizer orders must agree between machineries")
    classes.values.toVector.map { (pat, ball) =>
      // (1) the vertex stabilizer as ball-preserving star symmetries; (2) the corresponding folding
      val hIdx = acc.stab.indices.filter(k => preservesBall(acc.stab(k), ball, acc.stab))
      val hSet = hIdx.map(sym.perms).toSet
      require(
        hSet.forall(a => hSet.forall(b => hSet.contains(b.map(a)))),
        s"ball stabilizer is not a subgroup (species $i)"
      )
      val f    = fold(sym, hSet)
      // (3) σ₀ read off the pattern, then descended to the folding
      val s0Un = sigma0Unfolded(i, acc.g, pat, flags)
      val s0   = Vector.tabulate(f.size) { q =>
        val rep = f.orbitOf.indexOf(q)
        f.orbitOf(s0Un(rep))
      }
      require(
        s0Un.indices.forall(c => f.orbitOf(s0Un(c)) == s0(f.orbitOf(c))),
        s"pattern sigma0 does not descend to the stabilizer folding (species $i)"
      )
      val s    = Sym(s0, f.s1, f.s2, f.s3, f.m01, f.m23, f.cell, Vector.fill(f.size)(i))
      require(SymbolCatalog.valid(s), s"derived symbol violates the axioms (species $i)")
      require(SymbolCatalog.isMinimal(s), s"derived symbol is not minimal (species $i)")
      s
    }

  // ---------- the k = 2 side: the minimal symbol of a certified pair honeycomb ----------
