package io.github.scala_tessella.research_core

import StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}
import Sigma0Assembly.{enumerateSigma0, unionOf, ChamberUnion}

/** The SYMBOL CATALOG — canonical keys, minimality, and the assembly drivers.
  *
  * IDENTITY is exact and radius-free: the canonical key is the minimal BFS traversal code over all start
  * chambers (generators in the fixed order σ₀..σ₃; per visited chamber the images' discovery numbers plus the
  * m₀₁/m₂₃/cell/species decorations) — two decorated symbols are isomorphic iff their keys are equal, the 3D
  * transposition of the 2D program's canonical-key space.
  *
  * MINIMALITY by congruence closure: a symbol is non-minimal iff identifying some pair of chambers propagates
  * through the σ's to a consistent nontrivial congruence (data equal on every merged class). On a connected
  * symbol every nontrivial congruence merges chamber 0 with some other chamber (σ-words act bijectively), so
  * testing the pairs (0, d) is complete. Non-minimal symbols are duplicates of more-folded assemblies and are
  * discarded, exactly as in the two-dimensional programme.
  *
  * THE k = 1 DRIVER sweeps one species' full folding lattice (most-folded first) through the σ₀ enumerator
  * and keeps the minimal symbols deduped by key. Completeness of the sweep: a genuine mono-species
  * honeycomb's minimal symbol has vertex-orbit complex = flags/Stab(v) with Stab(v) a subgroup of the star
  * symmetry, so the sweep meets it at exactly that folding. Realizability is NOT decided here — that is
  * [[SymbolRealization]]'s geometric development; the catalog records combinatorial candidates.
  */
object SymbolCatalog:

  /** A decorated 3D Delaney–Dress symbol candidate. */
  final case class Sym(
      s0: Vector[Int],
      s1: Vector[Int],
      s2: Vector[Int],
      s3: Vector[Int],
      m01: Vector[Int],
      m23: Vector[Int],
      cell: Vector[Int],
      speciesOf: Vector[Int]
  ):
    def size: Int = s0.size

  def symOf(u: ChamberUnion, speciesPerOrbit: Vector[Int], s0: Vector[Int]): Sym =
    Sym(s0, u.s1, u.s2, u.s3, u.m01, u.m23, u.cell, u.orbit.map(speciesPerOrbit))

  /** The symbol axioms, directly (for probes and self-validation): involutions, the three commutations,
    * face-closure divisibility, connectivity.
    */
  def valid(s: Sym): Boolean =
    val n                    = s.size
    val gens                 = Vector(s.s0, s.s1, s.s2, s.s3)
    val inv                  = gens.forall(g => (0 until n).forall(c => g(g(c)) == c))
    val comm                 = (0 until n).forall { c =>
      s.s0(s.s2(c)) == s.s2(s.s0(c)) && s.s0(s.s3(c)) == s.s3(s.s0(c)) && s.s1(s.s3(c)) == s.s3(s.s1(c))
    }
    def faceLen(c: Int): Int =
      // bounded: a malformed (non-injective) sigma0 gives rho-shaped orbits that never return -- report a
      // failing length instead of hanging (the k = 5 derivation lesson)
      var a = s.s1(s.s0(c))
      var k = 1
      while a != c && k <= n do
        a = s.s1(s.s0(a))
        k += 1
      k
    val face                 = (0 until n).forall(c => s.m01(c) % faceLen(c) == 0)
    val conn                 =
      val seen  = collection.mutable.Set(0)
      var front = List(0)
      while front.nonEmpty do
        front = front.flatMap(c => gens.map(_(c)).filter(seen.add))
      seen.size == n
    inv && comm && face && conn

  /** Canonical traversal code: BFS numbering from each start chamber, generators in fixed order; the
    * lexicographic minimum over starts. Equal keys iff isomorphic as decorated symbols.
    */
  def canonicalKey(s: Sym): Vector[Int] =
    val gens                              = Vector(s.s0, s.s1, s.s2, s.s3)
    def codeFrom(start: Int): Vector[Int] =
      val num = Array.fill(s.size)(-1)
      val q   = collection.mutable.ArrayBuffer(start)
      num(start) = 0
      var qi  = 0
      while qi < q.size do
        val c = q(qi)
        qi += 1
        for g <- gens do
          val d = g(c)
          if num(d) < 0 then
            num(d) = q.size
            q += d
      q.toVector.flatMap(c =>
        gens.map(g => num(g(c))) ++ Vector(s.m01(c), s.m23(c), s.cell(c), s.speciesOf(c))
      )
    (0 until s.size).map(codeFrom).min(using math.Ordering.Implicits.seqOrdering)

  /** Minimality: no consistent nontrivial congruence (data constant on classes). */
  def isMinimal(s: Sym): Boolean =
    val gens                        = Vector(s.s0, s.s1, s.s2, s.s3)
    def mergeable(d0: Int): Boolean =
      val parent            = Array.tabulate(s.size)(identity)
      def find(x: Int): Int =
        if parent(x) == x then x
        else
          parent(x) = find(parent(x))
          parent(x)
      var ok                = true
      var pairs             = List((0, d0))
      while pairs.nonEmpty && ok do
        val (a, b)   = pairs.head
        pairs = pairs.tail
        val (ra, rb) = (find(a), find(b))
        if ra != rb then
          if s.m01(ra) != s.m01(rb) || s.m23(ra) != s.m23(rb) ||
            s.cell(ra) != s.cell(rb) || s.speciesOf(ra) != s.speciesOf(rb)
          then ok = false
          else
            parent(ra) = rb
            pairs = gens.map(g => (g(ra), g(rb))).toList ::: pairs
      ok
    !(1 until s.size).exists(mergeable)

  final case class AssemblyStat(
      subOrder: Int,
      chambers: Int,
      solutions: Int,
      minimalNew: Int,
      capped: Boolean
  )

  /** The k = 1 sweep of one species: every folding (most-folded first), every σ₀, minimal symbols deduped by
    * canonical key. `maxChambers` bounds the folded complexes swept (for tiered runs; the full sweep uses no
    * bound).
    */
  def k1Of(
      speciesIdx: Int,
      maxChambers: Int = Int.MaxValue,
      sigma0Cap: Int = Int.MaxValue,
      log: String => Unit = _ => ()
  ): (Vector[Sym], Vector[AssemblyStat]) =
    val sym   = symmetryOf(speciesIdx)
    val subs  = subgroupsOfSpecies(speciesIdx).sortBy(-_.size)
    val out   = collection.mutable.LinkedHashMap.empty[Vector[Int], Sym]
    val stats = Vector.newBuilder[AssemblyStat]
    for (sub, si) <- subs.zipWithIndex do
      val folded = fold(sym, sub)
      if folded.size <= maxChambers then
        val u            = unionOf(Vector(folded))
        val (sols, capd) = enumerateSigma0(u, sigma0Cap, log)
        var fresh        = 0
        for s0 <- sols do
          val s = symOf(u, Vector(speciesIdx), s0)
          if isMinimal(s) then
            val key = canonicalKey(s)
            if !out.contains(key) then
              out(key) = s
              fresh += 1
        stats += AssemblyStat(sub.size, folded.size, sols.size, fresh, capd)
        log(
          s"  folding ${si + 1}/${subs.size} |H|=${sub.size} chambers=${folded.size}: " +
            s"${sols.size} sigma0${if capd then " (CAPPED)" else ""}, $fresh new minimal (total ${out.size})"
        )
    (out.values.toVector, stats.result())

  final case class AssemblyStat2(
      subOrderA: Int,
      subOrderB: Int,
      chambers: Int,
      crossless: Boolean,
      solutions: Int,
      minimalNew: Int,
      capped: Boolean
  )

  /** A census symbol with its assembly provenance: EVERY folding pair (subgroups of the two star symmetry
    * groups) where a σ₀ with this canonical key arose, with the symbol as assembled at the FIRST of them. The
    * realization filter needs the provenance: a symbol is realizable iff some genuine honeycomb carries it,
    * and that honeycomb's own stabilizer pair is one of the recorded assemblies — so realization tries each,
    * and only exhausting all of them refutes.
    */
  final case class K2Entry(
      spA: Int,
      spB: Int,
      sym: Sym,
      folds: Vector[(Set[StarFoldings.Perm], Set[StarFoldings.Perm])]
  )

  /** The k-species census entry: the symbol with its folding-TUPLE provenance (one subgroup per role, every
    * tuple where the key arose). The k-ary generalization of `K2Entry`.
    */
  final case class KEntry(
      sps: Vector[Int],
      sym: Sym,
      folds: Vector[Vector[Set[StarFoldings.Perm]]]
  )

  final case class AssemblyStatK(
      subOrders: Vector[Int],
      chambers: Int,
      crossless: Boolean,
      solutions: Int,
      minimalNew: Int,
      capped: Boolean
  )

  /** The k-orbit sweep of a species k-set: every folding TUPLE (most-folded first, i.e. smallest unions
    * first), every σ₀ on the k-orbit union, minimal symbols deduped by canonical key with full folding-tuple
    * provenance. Completeness, k-ary: a genuine k-orbit Krötenheerdt honeycomb's minimal
    * symbol has chamber set flags/Γ = ⊔_r (flags_r/Stab(v_r)) — each vertex stabilizer acts on its star as a
    * subgroup of the star symmetry group — so the sweep meets it at exactly that folding tuple; the k orbits
    * carry pairwise DISTINCT species (Krötenheerdt), so no congruence merges them and the species decorations
    * stratify the key space by k. Tuples whose PART-COMPATIBILITY GRAPH (parts linked iff some chamber pair
    * matches on (m₀₁, m₂₃, cell)) is disconnected are skipped as CROSSLESS: σ₁..σ₃ preserve parts, σ₀ can
    * pair chambers only within compatibility classes, so the connectivity axiom is unsatisfiable. At k = 2
    * this is exactly the cross-compatible-pair skip.
    */
  def kEntries(
      sps: Vector[Int],
      maxChambers: Int = Int.MaxValue,
      sigma0Cap: Int = Int.MaxValue,
      log: String => Unit = _ => ()
  ): (Vector[KEntry], Vector[AssemblyStatK]) =
    require(sps.distinct.size == sps.size, "Krötenheerdt k-sets carry pairwise distinct species")
    def foldsOf(i: Int)                                          =
      val sym = symmetryOf(i)
      subgroupsOfSpecies(i).map(sub => (sub, fold(sym, sub)))
    val perRole                                                  = sps.map(foldsOf)
    val tuples                                                   = perRole
      .foldLeft(Vector(Vector.empty[(Set[StarFoldings.Perm], StarFoldings.Folded)])) { (acc, fs) =>
        for t <- acc; f <- fs yield t :+ f
      }
      .filter(_.map(_._2.size).sum <= maxChambers)
      .sortBy(_.map(_._2.size).sum)
    def partsConnected(fs: Vector[StarFoldings.Folded]): Boolean =
      def compatible(fa: StarFoldings.Folded, fb: StarFoldings.Folded) =
        fa.cell.indices.exists(c =>
          fb.cell.indices.exists(d =>
            fa.m01(c) == fb.m01(d) && fa.m23(c) == fb.m23(d) && fa.cell(c) == fb.cell(d)
          )
        )
      val reached                                                      = collection.mutable.Set(0)
      var grew                                                         = true
      while grew do
        grew = false
        for
          i <- fs.indices
          if !reached(i) && reached.exists(j => compatible(fs(i), fs(j)))
        do
          reached += i
          grew = true
      reached.size == fs.size
    val out                                                      = collection.mutable.LinkedHashMap.empty[Vector[Int], KEntry]
    val stats                                                    = Vector.newBuilder[AssemblyStatK]
    for (tuple, pi) <- tuples.zipWithIndex do
      val subs  = tuple.map(_._1)
      val fs    = tuple.map(_._2)
      val total = fs.map(_.size).sum
      if !partsConnected(fs) then stats += AssemblyStatK(subs.map(_.size), total, true, 0, 0, false)
      else
        val u            = unionOf(fs)
        val (sols, capd) = enumerateSigma0(u, sigma0Cap, log)
        var fresh        = 0
        for s0 <- sols do
          val s = symOf(u, sps, s0)
          if isMinimal(s) then
            val key = canonicalKey(s)
            out.get(key) match
              case None    =>
                out(key) = KEntry(sps, s, Vector(subs))
                fresh += 1
              case Some(e) =>
                if !e.folds.contains(subs) then out(key) = e.copy(folds = e.folds :+ subs)
        stats += AssemblyStatK(subs.map(_.size), total, false, sols.size, fresh, capd)
        log(
          s"  tuple ${pi + 1}/${tuples.size} |H|=${subs.map(_.size).mkString(",")} " +
            s"chambers=${fs.map(_.size).mkString("+")}: " +
            s"${sols.size} sigma0${if capd then " (CAPPED)" else ""}, $fresh new minimal (total ${out.size})"
        )
    (out.values.toVector, stats.result())

  /** The k = 2 sweep of a species pair — `kEntries` at k = 2, repackaged in the historical pair form (the
    * oracle-gated call sites; behavior including sweep order and stats is preserved exactly).
    */
  def k2Entries(
      spA: Int,
      spB: Int,
      maxChambers: Int = Int.MaxValue,
      sigma0Cap: Int = Int.MaxValue,
      log: String => Unit = _ => ()
  ): (Vector[K2Entry], Vector[AssemblyStat2]) =
    val (entries, stats) = kEntries(Vector(spA, spB), maxChambers, sigma0Cap, log)
    (
      entries.map(e => K2Entry(spA, spB, e.sym, e.folds.map(t => (t(0), t(1))))),
      stats.map(s =>
        AssemblyStat2(
          s.subOrders(0),
          s.subOrders(1),
          s.chambers,
          s.crossless,
          s.solutions,
          s.minimalNew,
          s.capped
        )
      )
    )

  /** The k = 2 sweep, symbols only (see `k2Entries` for the provenance-carrying form). */
  def k2Of(
      spA: Int,
      spB: Int,
      maxChambers: Int = Int.MaxValue,
      sigma0Cap: Int = Int.MaxValue,
      log: String => Unit = _ => ()
  ): (Vector[Sym], Vector[AssemblyStat2]) =
    val (entries, stats) = k2Entries(spA, spB, maxChambers, sigma0Cap, log)
    (entries.map(_.sym), stats)

  /** The k-orbit sweep, symbols only (see `kEntries` for the provenance-carrying form). */
  def kOf(
      sps: Vector[Int],
      maxChambers: Int = Int.MaxValue,
      sigma0Cap: Int = Int.MaxValue,
      log: String => Unit = _ => ()
  ): (Vector[Sym], Vector[AssemblyStatK]) =
    val (entries, stats) = kEntries(sps, maxChambers, sigma0Cap, log)
    (entries.map(_.sym), stats)
