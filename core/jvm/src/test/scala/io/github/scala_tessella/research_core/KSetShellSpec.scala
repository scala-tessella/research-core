package io.github.scala_tessella.research_core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species

/** The fair k-sets, fixture-first. Fast teeth: the candidate k-subsets reproduce `PairShell`'s candidate
  * pairs at k = 2; the edge relation e(S→T | {S,T}) EQUALS `PairShell`'s mixed shell (same domains, same
  * search — witness included) on the Barlow pair; the three genuine slab triples {c,h,e}, {c,e,p}, {h,e,p}
  * are fair — with {c,e,p} connecting through the PATH c–e–p (no c–p substrate edge exists), the structural
  * fact that kills the "every shell contains all k − 1 others" over-strengthening. The full k = 2 equality
  * against `PairShell`'s fair pairs and the k = 3 and k = 4 sweeps are opt-in (-Dksets.sweep, -Dksets.k4).
  *
  * The shared-figure graph on the 34 species decomposes into 11 components — one giant component of 23
  * species (the tet/oct/cube/prism/truncTet world), the rhombicuboctahedron pair, and 9 isolated species. A
  * connected honeycomb's orbit-adjacency graph is connected and adjacent orbits' species share an edge
  * figure, so the species set of any k-uniform Krötenheerdt honeycomb is a connected k-subset of this graph.
  * The decomposition is pinned by an independent BFS; the ESU-side corollary (exactly one candidate 23-set,
  * none at 24) is opt-in.
  */
class KSetShellSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse
  private lazy val elongated              = bySupport("{tet:4 oct:3 p3:6}").head
  private lazy val prisms                 = bySupport("{p3:12}")
  private lazy val prismMix               = prisms.find(i => species(i).figures.exists(_._1.size == 5)).get
  private lazy val prismPar               = prisms.find(_ != prismMix).get

  "the candidate k-sets" should "reproduce PairShell's candidate pairs at k = 2" in:
    KSetShell.candidateKSets(2) shouldBe PairShell.candidatePairs.map((i, j) => Vector(i, j))

  it should "equal the naive growth enumeration at k = 2..5 with the pinned counts" in:
    // the naive order-redundant DFS the canonical ESU growth replaced — kept as the oracle
    def naive(k: Int): Vector[Vector[Int]] =
      val adj                       = SpeciesCorona.analysis.adjacency
      def nbrs(s: Int): Vector[Int] = adj.getOrElse(s, Vector.empty).filter(_ != s)
      val out                       = collection.mutable.LinkedHashSet.empty[Vector[Int]]
      def grow(cur: Set[Int]): Unit =
        if cur.size == k then out += cur.toVector.sorted
        else for { s <- cur.toVector; t <- nbrs(s) if !cur(t) } do grow(cur + t)
      for s <- species.indices do grow(Set(s))
      out.toVector.sorted(using math.Ordering.Implicits.seqOrdering)
    val pinned                             = Map(2 -> 69, 3 -> 276, 4 -> 1003, 5 -> 3002)
    for k <- 2 to 5 do
      val cands = KSetShell.candidateKSets(k)
      withClue(s"k = $k: "):
        cands.size shouldBe pinned(k)
        cands shouldBe naive(k)

  it should "contain the three slab triples at k = 3, all connected in the substrate" in:
    val cands = KSetShell.candidateKSets(3)
    val adj   = SpeciesCorona.analysis.adjacency
    for kSet <- Vector(
                  Vector(octetC, octetH, elongated).sorted,
                  Vector(octetC, elongated, prismPar).sorted,
                  Vector(octetH, elongated, prismPar).sorted
                )
    do withClue(s"$kSet: ")(cands should contain(kSet))
    // canonical, distinct, connected
    cands.distinct.size shouldBe cands.size
    cands.foreach { kSet =>
      kSet shouldBe kSet.sorted
      // connectivity within the shared-figure graph
      val reached = collection.mutable.Set(kSet.head)
      var grew    = true
      while grew do
        grew = false
        for
          s <- kSet
          if !reached(s) && adj.getOrElse(s, Vector.empty).exists(reached)
        do
          reached += s
          grew = true
      reached.size shouldBe kSet.size
    }

  "the edge relation at k = 2" should "equal PairShell's mixed shell on the Barlow pair, witness included" in:
    val flags = MonoShell.Flags()
    for (s, t) <- Vector((octetC, octetH), (octetH, octetC)) do
      val e = KSetShell.shellWithTarget(s, t, Vector(s, t).sorted, flags)
      val m = PairShell.mixedShell(s, t, flags)
      withClue(s"${SpeciesCorona.label(s)} -> ${SpeciesCorona.label(t)}: "):
        e.sat shouldBe m.sat
        e.witness shouldBe m.witness
    flags.items.distinct shouldBe empty

  "the slab triples" should "be fair, {c,e,p} through the path c-e-p (no c-p edge)" in:
    val flags = MonoShell.Flags()
    val adj   = SpeciesCorona.analysis.adjacency
    // c and p share no edge figure: the substrate itself has no c-p edge
    adj.getOrElse(octetC, Vector.empty) should not contain prismPar
    for kSet <- Vector(
                  Vector(octetC, octetH, elongated).sorted,
                  Vector(octetC, elongated, prismPar).sorted,
                  Vector(octetH, elongated, prismPar).sorted
                )
    do
      val v = KSetShell.fairKSet(kSet, flags)
      withClue(s"${kSet.map(SpeciesCorona.label)}: "):
        v.fair shouldBe true
        v.edges.size shouldBe 2 // a spanning tree of 3 species
        // every used edge is a substrate edge
        v.edges.foreach((i, j) => adj.getOrElse(i, Vector.empty) should contain(j))
    flags.items.distinct shouldBe empty

  "the slab quadruple {c,h,e,p}" should "be fair" in:
    val flags = MonoShell.Flags()
    val kSet  = Vector(octetC, octetH, elongated, prismPar).sorted
    KSetShell.candidateKSets(4) should contain(kSet)
    val v     = KSetShell.fairKSet(kSet, flags)
    v.fair shouldBe true
    v.edges.size shouldBe 3 // a spanning tree of 4 species
    flags.items.distinct shouldBe empty

  "the shared-figure graph" should "decompose into the giant 23-component, the rco pair and 9 singletons" in:
    // independent BFS over the substrate adjacency (self-loops ignored) — not ESU
    val adj                         = SpeciesCorona.analysis.adjacency
    def component(s: Int): Set[Int] =
      val comp     = collection.mutable.Set(s)
      var frontier = Set(s)
      while frontier.nonEmpty do
        val next = frontier.flatMap(j => adj.getOrElse(j, Vector.empty).filter(x => x != j && !comp(x)))
        comp ++= next
        frontier = next
      comp.toSet
    val comps                       = species.indices.foldLeft(Vector.empty[Set[Int]]) { (acc, i) =>
      if acc.exists(_.contains(i)) then acc else acc :+ component(i)
    }
    comps.map(_.size).sorted.reverse shouldBe (Vector(23, 2) ++ Vector.fill(9)(1))
    val giant                       = comps.maxBy(_.size)
    giant shouldBe (Set(9) ++ (12 to 33).toSet)
    // every anchor species lives in the giant component; the pair is the rco world
    for i <- Vector(octetC, octetH, elongated, prismPar, prismMix) do giant should contain(i)
    comps.find(_.size == 2).get.map(SpeciesCorona.label) shouldBe
      Set("{cube:2 co:1 rco:2}#1", "{tet:1 cube:1 rco:3}#1")

  it should "admit exactly one candidate 23-set and none at 24 (-Dksets.sweep)" in:
    assume(
      sys.props.contains("ksets.sweep"),
      "heavy: the full ESU sweep of the giant component — enable with -Dksets.sweep"
    )
    KSetShell.candidateKSets(23) shouldBe Vector((Vector(9) ++ (12 to 33)).sorted)
    KSetShell.candidateKSets(24) shouldBe empty

  "the full k = 2 fairness" should "equal PairShell's fair pairs exactly (-Dksets.sweep)" in:
    assume(
      sys.props.contains("ksets.sweep"),
      "heavy: the full 69-candidate double sweep — enable with -Dksets.sweep"
    )
    val flags = MonoShell.Flags()
    val fair2 = KSetShell.fairKSets(2, flags).filter(_.fair).map(v => (v.kSet(0), v.kSet(1)))
    fair2 shouldBe PairShell.fairPairs
    flags.items.distinct shouldBe empty

  "the full k = 4 fairness" should
    "yield exactly 339 fair quadruples of the 1003 candidates (-Dksets.k4)" in:
      assume(
        sys.props.contains("ksets.k4"),
        "heavy: the full 1003-candidate sweep, ~2 h 45 m — enable with -Dksets.k4"
      )
      val flags = MonoShell.Flags()
      val all   = KSetShell.fairKSets(4, flags)
      all should have size 1003
      val fair  = all.filter(_.fair).map(_.kSet)
      fair should have size 339
      fair should contain(Vector(octetC, octetH, elongated, prismPar).sorted)
      flags.items.distinct shouldBe empty

  "the full k = 3 fairness" should "yield exactly 103 fair triples of the 276 candidates (-Dksets.sweep)" in:
    assume(
      sys.props.contains("ksets.sweep"),
      "heavy: the full 276-candidate sweep — enable with -Dksets.sweep"
    )
    val flags = MonoShell.Flags()
    val all   = KSetShell.fairKSets(3, flags)
    all should have size 276
    val fair  = all.filter(_.fair).map(_.kSet)
    fair should have size 103
    for kSet <- Vector(
                  Vector(octetC, octetH, elongated).sorted,
                  Vector(octetC, elongated, prismPar).sorted,
                  Vector(octetH, elongated, prismPar).sorted
                )
    do withClue(s"${kSet.map(SpeciesCorona.label)}: ")(fair should contain(kSet))
    flags.items.distinct shouldBe empty
