package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.DelaneySymbols
import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.Signatures.VertexSignature
import io.github.scala_tessella.research_core.solver.SatSolver.SolverSink
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Using

/** Teeth for the track-C quotient obligation encoding, tool-free: the obligation is solved LIVE through
  * the platform [[SatSolver]] instead of external kissat, so it runs everywhere. On a MINIMAL symbol the
  * generator list is empty and the empty-list obligation certifies minimality itself (scaladoc of
  * [[QuotientCertify]]) — so on every oracle minimal symbol `generators` must be empty and `encode`'s
  * instance must be UNSAT. Plus the structural contract of `closureModel` (a full ± assignment over the
  * pair variables).
  */
class QuotientCertifySpec extends AnyFlatSpec with Matchers:

  private lazy val oracle: List[(VertexSignature, DSymbol)] =
    DelaneySymbols.enumerateSymbols(maxN = 1, maxSize = 12).map((_, sigs, ds) => sigs.head -> ds)

  "generators" should "be empty on every minimal oracle symbol" in:
    for (t, ds) <- oracle do withClue(s"$t: ") { QuotientCertify.generators(ds) shouldBe empty }

  "encode with the empty generator list" should "be UNSAT on every minimal oracle symbol (live solver)" in:
    for (t, ds) <- oracle do
      withClue(s"$t: ") {
        Using.resource(PlatformSolver.default()) { s =>
          val addedCleanly =
            try
              QuotientCertify.encode(ds, Set.empty, SolverSink(s))
              true
            catch case _: SatSolver.Contradiction => false // eagerly-detected UNSAT counts as UNSAT
          if addedCleanly then s.solve() shouldBe false
        }
      }

  "closureModel" should "assign every pair variable a sign" in:
    val ds    = oracle.map(_._2).maxBy(_.size) // the largest oracle symbol has chambers to spare
    val pv    = QuotientCertify.encode(ds, Set.empty, SymbolAssembly.NullSink)
    val model = QuotientCertify.closureModel(ds, 2, pv)
    model.length shouldBe pv.size
    model.map(math.abs).sorted shouldBe pv.values.toArray.sorted
