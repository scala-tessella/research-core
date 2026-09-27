package io.github.scala_tessella.research_core

import MonoShell.Flags
import PairShell.{geom, Pl}

/** The FAIR k-SETS — the k-species generalization of the fair pairs of `PairShell`, the candidate substrate
  * of the k-orbit assembly. A k-uniform Krötenheerdt honeycomb carries k orbits with pairwise distinct
  * species; orbit-transitivity makes each species' decorated shell an invariant, and connectivity forces the
  * SPECIES GRAPH — S adjacent T iff THE S-shell contains a T-star (symmetric via the reverse edge) — to
  * connect all k species. Note the pair condition does NOT generalize to "every shell contains all k − 1
  * others": the genuine SlabNecklaces triple {c, e, p} has no c–p edge at all (an octet vertex and a prism
  * vertex share no edge figure) — its species graph is the path c–e–p. Connectivity is the necessary
  * condition.
  *
  * Machine-checked necessary conditions per candidate set K:
  *
  *   1. SUBSTRATE (the shared-figure graph): an S–T honeycomb edge shows one edge figure to both ends, so
  *      honeycomb-adjacent species share a figure — candidate sets are the CONNECTED k-subsets of the
  *      certified shared-figure adjacency graph, and edge tests run only on its edges.
  *   2. THE EDGE RELATION: e(S→T | K) = a consistent full shell around a seed S-star exists with every
  *      neighbor an K-species star (placements from the cross-gluing atlas, pairwise mutual-edge/ring
  *      compatibility — `PairShell`'s machinery verbatim) and AT LEAST ONE T-star. In the genuine honeycomb
  *      the S-shell itself witnesses e(S→T) for every true edge, so the true species graph is a subgraph of
  *      the computed one; K survives iff the computed undirected graph (e both ways) CONNECTS K. Mixedness is
  *      subsumed: an S with no incident edge has no shell with any cross neighbor.
  *
  * At k = 2 the relation e(S→T | {S,T}) is EXACTLY `PairShell`'s mixed shell (identical domains, identical
  * search), so fairKSets(2) must equal PairShell's fair pairs — the regression oracle, spec-pinned. Verdicts
  * are memoized per (seed, target, K) in a concurrent map (a sweep may run candidates in parallel; the
  * candidates are independent — geometry memos pre-warmed, flags per candidate); sat searches return fast
  * (first witness), unsat searches exhaust — the full k-sweeps are long runs for the caller to schedule.
  */
object KSetShell:

  /** e(S→T | K): the witness shell, when satisfiable. */
  final case class EdgeResult(seed: Int, target: Int, kSet: Vector[Int], witness: Option[Vector[Pl]]):
    def sat: Boolean = witness.isDefined

  private val edgeCache = new java.util.concurrent.ConcurrentHashMap[(Int, Int, Vector[Int]), EdgeResult]

  /** The full-shell domains of a seed within K: same-species placements first, then the other K-species in
    * K-order — at K = {S, T} exactly `PairShell.mixedShell`'s domains.
    */
  private def domainsOf(seed: Int, kSet: Vector[Int], flags: Flags): Vector[Vector[Pl]] =
    val gS = geom(seed)
    gS.u.indices.toVector.map { x =>
      PairShell.crossGluings(gS, x, gS, flags).map(Pl(seed, _)) ++
        kSet
          .filter(_ != seed)
          .flatMap(t => PairShell.crossGluings(gS, x, geom(t), flags).map(Pl(t, _)))
    }

  /** The edge test e(S→T | K), memoized. The search is `PairShell.mixedShell`'s backtracker with the forced
    * slot filtered to the TARGET species (mixedShell's cross filter, sharpened to one species).
    */
  def shellWithTarget(seed: Int, target: Int, kSet: Vector[Int], flags: Flags): EdgeResult =
    require(kSet.contains(seed) && kSet.contains(target) && seed != target, "need seed != target, both in K")
    // NOT computeIfAbsent: the edge test can run for hours, and computing inside the bin lock
    // serializes unrelated keys that share a bin — compute outside, publish with putIfAbsent
    // (keys are per-candidate unique, so no duplicated work in the sweep)
    val cacheKey = (seed, target, kSet)
    val cached   = edgeCache.get(cacheKey)
    if cached != null then cached
    else
      val computed = {
        val gS                                   = geom(seed)
        val n                                    = gS.u.size
        val domains                              = domainsOf(seed, kSet, flags)
        val order                                = (0 until n).sortBy(domains(_).size).toVector
        val chosen                               = Array.fill(n)(null.asInstanceOf[Pl])
        def bt(k: Int, forcedSlot: Int): Boolean =
          if k == n then true
          else
            val x  = order(k)
            val ds = if k == forcedSlot then domains(x).filter(_.sp == target) else domains(x)
            ds.exists { pl =>
              chosen(x) = pl
              val ok  = (0 until k).forall { k2 =>
                val x2 = order(k2)
                PairShell.compatibleP(gS, x, pl, x2, chosen(x2), flags)
              }
              val res = ok && bt(k + 1, forcedSlot)
              if !res then chosen(x) = null.asInstanceOf[Pl]
              res
            }
        // a forced slot without any target member can never succeed — skipping it up front is
        // witness-preserving (the first succeeding forced slot is unchanged)
        val sat                                  = (0 until n).exists { slot =>
          domains(order(slot)).exists(_.sp == target) && {
            java.util.Arrays.fill(chosen.asInstanceOf[Array[AnyRef]], null)
            bt(0, slot)
          }
        }
        EdgeResult(seed, target, kSet, if sat then Some(chosen.toVector) else None)
      }
      val prev     = edgeCache.putIfAbsent(cacheKey, computed)
      if prev != null then prev else computed

  /** The fairness verdict of one candidate set: the undirected edges computed lazily (substrate pairs only,
    * both directions) until K is connected or the pairs are exhausted.
    */
  final case class KVerdict(kSet: Vector[Int], edges: Vector[(Int, Int)], fair: Boolean)

  /** EDGE MONOTONICITY: `e(S→T | K)` is witnessed by a shell whose neighbours are all K-species stars, so for
    * `K ⊆ K'` the same shell witnesses `e(S→T | K')` — an edge established on a subset is established here.
    * `known(i, j)` reports such an inherited edge; it is consulted INSTEAD of the two shell searches, which
    * is where the saving is.
    *
    * Sound in the only direction that matters: inherited edges are a subset of the true ones, so a set the
    * shortcut spans is genuinely fair, and a set it fails to span is searched exactly as before. Measured
    * against the k = 7 sweep's own verdicts, inheritance from the k = 6 journal alone spans 31.5% of
    * candidates with zero false positives over 921 predictions.
    */
  def fairKSet(
      kSet: Vector[Int],
      flags: Flags,
      log: String => Unit = _ => (),
      known: (Int, Int) => Boolean = (_, _) => false
  ): KVerdict =
    val adj               = SpeciesCorona.analysis.adjacency
    val pairs             = (for
      i <- kSet
      j <- kSet
      if i < j && adj.getOrElse(i, Vector.empty).contains(j)
    yield (i, j)).toVector
    // union-find over K; stop as soon as one component remains
    val root              = collection.mutable.Map.from(kSet.map(s => s -> s))
    def find(s: Int): Int = if root(s) == s then s else { root(s) = find(root(s)); root(s) }
    var components        = kSet.size
    val edges             = Vector.newBuilder[(Int, Int)]
    val it                = pairs.iterator
    while components > 1 && it.hasNext do
      val (i, j) = it.next()
      if find(i) != find(j) then
        if !known(i, j) then
          log(
            s"    edge test ${SpeciesCorona.label(i)} ~ ${SpeciesCorona.label(j)} in ${kSet.mkString("{", ",", "}")}"
          )
        if known(i, j) || (shellWithTarget(i, j, kSet, flags).sat && shellWithTarget(j, i, kSet, flags).sat)
        then
          edges += ((i, j))
          root(find(i)) = find(j)
          components -= 1
    KVerdict(kSet, edges.result(), components == 1)

  /** Candidate k-sets: the CONNECTED k-subsets of the shared-figure adjacency graph, sorted, canonical. At k =
    * 2 exactly `PairShell.candidatePairs`. Enumeration is CANONICAL GROWTH (the ESU scheme: anchor = the
    * subset's minimum vertex, monotone extension over an exclusive frontier), so every connected k-subset is
    * generated exactly once — no order-redundant DFS, no dedup set; the naive growth enumeration is the
    * spec-side oracle (`KSetShellSpec`).
    */
  def candidateKSets(k: Int): Vector[Vector[Int]] =
    val adj                       = SpeciesCorona.analysis.adjacency
    def nbrs(s: Int): Vector[Int] = adj.getOrElse(s, Vector.empty).filter(_ != s)
    val out                       = Vector.newBuilder[Vector[Int]]
    for anchor <- SpeciesEnumerator.species.indices do
      // connected subsets whose minimum vertex is `anchor`: binary branching over the extension frontier,
      // each vertex offered at most once per branch (ESU exclusive neighborhood => exactly-once output)
      def rec(cur: List[Int], size: Int, ext: List[Int], offered: Set[Int]): Unit =
        if size == k then out += cur.sorted.toVector
        else
          ext match
            case Nil       => ()
            case t :: rest =>
              val fresh = nbrs(t).filter(u => u > anchor && !offered(u))
              rec(t :: cur, size + 1, rest ++ fresh, offered ++ fresh)
              rec(cur, size, rest, offered)
      val ext0                                                                    = nbrs(anchor).filter(_ > anchor).distinct.sorted.toList
      rec(List(anchor), 1, ext0, ext0.toSet + anchor)
    out.result().sorted(using math.Ordering.Implicits.seqOrdering)

  /** The fair k-sets: the assembly substrate of the k-orbit census. Heartbeats per candidate. */
  def fairKSets(k: Int, flags: Flags, log: String => Unit = _ => ()): Vector[KVerdict] =
    val cands = candidateKSets(k)
    log(s"${cands.size} connected candidate $k-sets")
    cands.zipWithIndex.map { (kSet, i) =>
      val v = fairKSet(kSet, flags, log)
      log(s"  [${i + 1}/${cands.size}] ${kSet.map(SpeciesCorona.label).mkString("{", ", ", "}")}: " +
        s"${if v.fair then "FAIR" else "dead"} (${v.edges.size} edges)")
      v
    }
