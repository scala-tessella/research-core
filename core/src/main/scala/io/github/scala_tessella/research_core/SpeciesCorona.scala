package io.github.scala_tessella.research_core

import HoneycombAlphabet.CanonKey
import SpeciesEnumerator.{species, Species, State}

/** Corona structure over the vertex species of [[SpeciesEnumerator]] — which survive corona extension, and
  * the adjacency data a multi-orbit derivation consumes.
  *
  * Pair level (edges): a honeycomb edge with ring figure f joins two vertices whose species BOTH contain f as
  * a vertex figure. Since species are counted up to reflection, every species self-hosts each of its own
  * figures (the mirrored reading at the far end is the same species) — so the naive pair fixpoint is PROVABLY
  * VACUOUS: no species can die across a single edge, exactly as 2D's 3.8.24 survives pair gluing. What the
  * pair level does yield: the hosting table (figure → species containing it) and the species adjacency graph,
  * and a free theorem — every edge figure realizable in ANY honeycomb must occur as a species vertex figure,
  * so the figures used by the species are the only live letters of [[HoneycombAlphabet]]'s catalogue.
  *
  * Face level (the 2D killer, one dimension up): let F be a p-gonal face of the star at v, flanked by cells
  * C, D. Every vertex u₀ = v, u₁, …, u_{p−1} of F carries a species; at uᵢ the arc of F is flanked by corners
  * of the SAME cells C, D, and the ring around the edge (uᵢ, uᵢ₊₁) — a vertex figure of both endpoint species
  * — carries a marked C·D adjacency (the position of F in the ring). The MARKED-RING FORM (token reading of
  * the ring starting at the mark, minimized over the two directions) is end-independent, so gluing along the
  * face's edges is exactly form equality, and "F is completable" becomes: a path of exactly p−1 steps between
  * the two end forms of the arc at v, in the relation collecting the (end form, end form) pairs of all size-p
  * arcs of all surviving species. A species with an uncompletable face cannot occur in any honeycomb;
  * removals shrink the relation, so iterate to a fixpoint (the OutsiderExclusion pattern, two levels up).
  * This is the exact 3D transposition of the odd-face walk that kills 3.8.24 in 2D. It is a sound filter —
  * survivors are corona-consistent, not yet proven realizable.
  */
object SpeciesCorona:

  import scala.math.Ordering.Implicits.seqOrdering

  private type Vid  = Int
  private type AKey = (Int, Int)
  private def key(a: Int, b: Int): AKey = if a < b then (a, b) else (b, a)

  /** Marked-ring form: the token reading of a vertex figure starting at a marked incident arc. */
  type Form = Vector[(Int, Int, Int)]

  // ---------- combinatorial rings ----------

  /** The cyclic corner sequence around tiling vertex `vid` of a closed species state, walked purely
    * combinatorially (follow shared arcs): slots (corner index, entry arc, exit arc).
    */
  def ringAt(st: State, vid: Int): Vector[(Int, (Int, Int), (Int, Int))] =
    val ws    =
      (for
        (pl, ci) <- st.corners.zipWithIndex
        k         = pl.vids.size
        i        <- 0 until k
        if pl.vids(i) == vid
      yield (ci, key(vid, pl.vids((i + k - 1) % k)), key(vid, pl.vids((i + 1) % k)))).toVector
    val byArc = collection.mutable.Map.empty[AKey, List[Int]]
    for
      (w, j) <- ws.zipWithIndex
      a      <- List(w._2, w._3)
    do byArc(a) = j :: byArc.getOrElse(a, Nil)
    val out   = Vector.newBuilder[(Int, AKey, AKey)]
    var j     = 0
    var in    = ws(0)._2
    for _ <- ws.indices do
      val (ci, a1, a2) = ws(j)
      val exit         = if in == a1 then a2 else a1
      out += ((ci, in, exit))
      j = byArc(exit).find(_ != j).get
      in = exit
    out.result()

  /** Canonical marked-ring form at `vid`, marked at incident arc `mark`: token reading starting at the slot
    * entered via the mark, minimized over the two directions — identical when computed from either end of the
    * corresponding honeycomb edge (the far end reads the mirrored ring, which is the other direction).
    */
  private def markedForm(st: State, vid: Vid, mark: AKey): Form =
    val ring                                                         = ringAt(st, vid)
    val n                                                            = ring.size
    def tok(slot: (Int, AKey, AKey), swap: Boolean): (Int, Int, Int) =
      val (ci, a, b) = slot
      val (fa, fb)   = (st.arcs(a).face, st.arcs(b).face)
      if swap then (st.corners(ci).cell.ordinal, fb, fa) else (st.corners(ci).cell.ordinal, fa, fb)
    val fwdStart                                                     = ring.indexWhere(_._2 == mark)
    val bwdStart                                                     = ring.indexWhere(_._3 == mark)
    val fwd                                                          = (0 until n).toVector.map(d => tok(ring((fwdStart + d) % n), swap = false))
    val bwd                                                          = (0 until n).toVector.map(d => tok(ring((bwdStart - d + 2 * n) % n), swap = true))
    if Ordering[Form].lteq(fwd, bwd) then fwd else bwd

  /** Every arc of a species as (face size, end form, end form). */
  private def arcInterfaces(st: State): Vector[(Int, Form, Form)] =
    st.arcs.toVector.map { case ((x, y), arc) =>
      (arc.face, markedForm(st, x, (x, y)), markedForm(st, y, (x, y)))
    }

  // ---------- the face-cycle fixpoint ----------

  def pathExists(
      rel: Map[Form, Set[Form]],
      from: Form,
      to: Form,
      steps: Int
  ): Boolean =
    var frontier = Set(from)
    for _ <- 0 until steps do frontier = frontier.flatMap(m => rel.getOrElse(m, Set.empty))
    frontier.contains(to)

  final case class Analysis(
      survivors: Vector[Int],              // indices into SpeciesEnumerator.species
      killedByRound: Vector[Vector[Int]],  // non-surviving species, per fixpoint round
      figuresUsed: Set[CanonKey],          // vertex figures occurring in ANY of the 34
      hosting: Map[CanonKey, Vector[Int]], // figure -> species containing it
      adjacency: Map[Int, Vector[Int]]     // species -> species sharing >= 1 figure
  )

  lazy val analysis: Analysis =
    val sp        = species
    val ifaces    = sp.map(s => arcInterfaces(s.state))
    var alive     = sp.indices.toSet
    val killed    = Vector.newBuilder[Vector[Int]]
    var changed   = true
    while changed do
      val rel                                  = collection.mutable.Map.empty[Int, Map[Form, Set[Form]]]
      def relFor(p: Int): Map[Form, Set[Form]] =
        rel.getOrElseUpdate(
          p, {
            val pairs =
              for
                i          <- alive.toVector
                (q, m, m2) <- ifaces(i)
                if q == p
                pair       <- Vector(m -> m2, m2 -> m)
              yield pair
            pairs.groupMapReduce(_._1)(t => Set(t._2))(_ ++ _)
          }
        )
      val next                                 = alive.filter { i =>
        ifaces(i).forall((p, m0, m1) => pathExists(relFor(p), m0, m1, p - 1))
      }
      val dead                                 = (alive -- next).toVector.sorted
      if dead.nonEmpty then killed += dead
      changed = next != alive
      alive = next
    val hosting   = sp.indices.toVector
      .flatMap(i => sp(i).figures.map((f, _) => f -> i))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.distinct.sorted)
      .toMap
    val adjacency = sp.indices.toVector.map { i =>
      val mine = sp(i).figures.map(_._1).toSet
      i -> sp.indices.toVector.filter(j => sp(j).figures.exists((f, _) => mine(f)))
    }.toMap
    Analysis(alive.toVector.sorted, killed.result(), hosting.keySet, hosting, adjacency)

  /** Display label matching the species-table cert: support string + index within the support. */
  def label(i: Int): String =
    val sp    = species
    val group = sp.indices.filter(j => sp(j).counts == sp(i).counts)
    s"${sp(i).showSupport}#${group.indexOf(i) + 1}"
