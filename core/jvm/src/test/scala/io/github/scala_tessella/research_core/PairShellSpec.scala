package io.github.scala_tessella.research_core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import PairShell.*
import SpeciesEnumerator.species

/** the fair-pair derivation. The cross-gluing atlas must reproduce MonoShell's same-species atlas exactly;
  * the pairs of every KNOWN k = 2 Krötenheerdt honeycomb must survive the mixed-shell filter (Barlow octet
  * pair, the four mixed slab pairs of the slab world, prismatic-lift pairs of the 2D 2-uniform tilings); no
  * gray-zone flags.
  */
class PairShellSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  // the slab species (slab-world labels): octet c/h, elongated e, parallel p, mixed-axis x
  private lazy val Vector(octetH, octetC) = // #1 h (two distinct figures), #2 c (single figure)
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse
  private lazy val elongated              = bySupport("{tet:4 oct:3 p3:6}").head
  private lazy val prisms                 = bySupport("{p3:12}")
  private lazy val prismMix               = prisms.find(i => species(i).figures.exists(_._1.size == 5)).get
  private lazy val prismPar               = prisms.find(_ != prismMix).get

  "the cross-gluing atlas at same species" should "reproduce MonoShell's gluing atlas exactly" in:
    val flags = MonoShell.Flags()
    for i <- Vector(octetC, octetH, elongated, prismPar) do
      val g = geom(i)
      for x <- g.u.indices do
        val mono  = MonoShell.gluings(g, x, flags)
        val cross = crossGluings(g, x, g, flags)
        withClue(s"species $i vertex $x: "):
          cross.size shouldBe mono.size
          mono.foreach(m => assert(cross.exists(c => c.y == m.y && c.rot.sameAs(m.rot))))

  "the candidate pairs" should "be the 69 edges of the certified adjacency graph" in:
    candidatePairs should have size 69
    candidatePairs.foreach { (i, j) =>
      i should be < j
      SpeciesCorona.analysis.adjacency(i) should contain(j)
    }

  "the known k = 2 pairs" should "survive the mixed-shell filter" in:
    val flags                      = MonoShell.Flags()
    def fair(a: Int, b: Int): Unit =
      withClue(s"${species(a).showSupport} ~ ${species(b).showSupport}: "):
        mixedShell(a, b, flags).sat shouldBe true
        mixedShell(b, a, flags).sat shouldBe true
    fair(octetC, octetH)    // the four Barlow stackings (4H, 6H, 9R, 12R)
    fair(octetC, elongated) // the slab pairs {c,e}, {h,e}, {e,p}, {p,x}
    fair(octetH, elongated)
    fair(elongated, prismPar)
    fair(prismPar, prismMix)
    // prismatic lifts of 2D 2-uniform pairs: (3^6; 3^4.6) lifts to {p3:12}par ~ {p3:8 p6:2}snub;
    // the lift is the one WITHOUT the 5-cell all-p3 mixed-axis figure
    val p3Ord                      = HoneycombAlphabet.CellType.P3.ordinal
    val snubTriHex                 = bySupport("{p3:8 p6:2}")
      .find(i => !species(i).figures.exists(f => f._1.size == 5 && f._1.forall(_._1 == p3Ord)))
      .get
    fair(prismPar, snubTriHex)
    // the two headline NEW fair pairs: the gyrated quarter-cubic pair, and cubic ~ elongated triangular
    // prismatic (cube confinement fails in the full alphabet: squares live on prisms too)
    val Vector(quarterA, quarterB) = bySupport("{tet:2 truncTet:6}")
    fair(quarterA, quarterB)
    fair(bySupport("{cube:8}").head, bySupport("{cube:4 p3:6}").head)
    flags.items.distinct shouldBe empty

  "the full sweep" should "yield exactly 33 fair pairs of the 69 candidates (enable with -Dpairs.sweep)" in:
    assume(sys.props.contains("pairs.sweep"), "~15 min sweep — enable with -Dpairs.sweep")
    fairPairs should have size 33
    results._2 shouldBe empty
