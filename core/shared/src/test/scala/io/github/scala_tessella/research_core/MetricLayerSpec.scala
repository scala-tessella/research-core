package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.MetricLayer.*
import io.github.scala_tessella.research_core.Signatures.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ADR-0009 G2/G3 consistency teeth for the metric layer, on the 11 Archimedean oracle minimal symbols —
  * the surface the verification repositories (`31-unit-edge-tilings`, `minimal-uniformity-three`) stand
  * on, previously untested in-repo. Every assertion is a documented INTERNAL identity, not an imported
  * value: the regular point solves the exact linear system and closes every face; the numeric closure
  * track agrees with the exact ℚ(ζ₂₄) track; the exact linear matrix obeys rank–nullity against the Frac
  * RREF nullspace; the tangent basis has the moduli dimension by construction and Gauss–Newton moduli
  * points still close every face; minimal symbols are exact-symmetry realizable vacuously.
  */
class MetricLayerSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  private lazy val oracle: Map[VertexSignature, DSymbol] =
    DelaneySymbols.enumerateSymbols(maxN = 1, maxSize = 12).map((_, sigs, ds) => sigs.head -> ds).toMap

  "the regular point" should "solve the exact linear angle system of every oracle symbol" in:
    for (t, ds) <- oracle do
      withClue(s"$t: ") { satisfies(angleSystem(ds), regularPoint(ds)) shouldBe true }

  it should "close every face numerically (it IS the uniform tiling)" in:
    for (t, ds) <- oracle do
      withClue(s"$t: ") { maxClosureResidual(ds, regularPoint(ds).map(_.toDouble)) should be < 1e-9 }

  "the exact ℚ(ζ₂₄) track" should "agree with the numeric track on closure rank and moduli dimension" in:
    for (t, ds) <- oracle do
      val basis = nullspaceBasis(angleSystem(ds))
      withClue(s"$t: ") {
        closureRankExact(ds, basis) shouldBe closureRank(ds, basis)
        moduliDimensionExact(ds) shouldBe moduliDimension(ds)
      }

  "linearMatrixExact" should "obey rank–nullity against the Frac RREF nullspace" in:
    for (t, ds) <- oracle do
      val sys = angleSystem(ds)
      withClue(s"$t: ") {
        Cyclo24.rank(linearMatrixExact(sys)) shouldBe sys.vars - nullspaceBasis(sys).size
      }

  "tangentBasis" should "have exactly the moduli dimension, with moduli points closing every face" in:
    for (t, ds) <- oracle do
      withClue(s"$t: ") {
        val basis = tangentBasis(ds)
        basis.size shouldBe moduliDimension(ds)
        for dir <- basis.headOption do // only symbols with genuine moduli have a direction to walk
          maxClosureResidual(ds, moduliPoint(ds, dir, 0.05)) should be < 1e-8
      }

  "exactSymmetryRealizable" should "hold vacuously on every (minimal) oracle symbol" in:
    for (t, ds) <- oracle do withClue(s"$t: ") { exactSymmetryRealizable(ds) shouldBe true }

  "chamberAngles" should "give right angles everywhere on 4⁴" in:
    val ds = oracle(sig("4.4.4.4"))
    chamberAngles(ds, regularPoint(ds).map(_.toDouble)).drop(1).foreach(_ shouldBe math.Pi / 2 +- 1e-12)
