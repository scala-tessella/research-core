package io.github.scala_tessella.research_core.solver

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** End-to-end teeth for the fs2-io/Cats Effect external-process layer of [[CertifyRunner]] — the plumbing
  * the DRAT certification harness stands on. Cancelled (not failed) when `tools/bin/{kissat,drat-trim}`
  * are not installed, like the guarded probes. Both certification tiers are exercised POSITIVELY: the
  * model-bearing tier on the 4⁴ sweep, the pure-refutation unbroken tier on an angle-valid-but-non-tiling
  * type whose frames carry no σ₀ model at all.
  */
class CertifyRunnerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private val root = Files.createTempDirectory("certify-spec")

  private def deleteRecursively(f: java.io.File): Unit =
    val children = f.listFiles()
    if children != null then children.foreach(deleteRecursively)
    f.delete()

  override def afterAll(): Unit = deleteRecursively(root.toFile)

  private def requireTools(): Unit =
    assume(CertifyRunner.toolsInstalled, "tools/bin/{kissat,drat-trim} not installed")

  "certifyCnf (fs2-io external processes)" should "certify a trivially UNSAT instance" in:
    requireTools()
    val dir                         = Files.createTempDirectory(root, "certify-cnf")
    val cnf                         = dir.resolve("unsat.cnf")
    Files.writeString(cnf, "p cnf 1 2\n1 0\n-1 0\n")
    val (kissatUnsat, dratVerified) = CertifyRunner.certifyCnf(cnf, dir.resolve("proof.drat"))
    kissatUnsat shouldBe true
    dratVerified shouldBe true

  it should "not report UNSAT on a satisfiable instance" in:
    requireTools()
    val dir              = Files.createTempDirectory(root, "certify-cnf-sat")
    val cnf              = dir.resolve("sat.cnf")
    Files.writeString(cnf, "p cnf 2 2\n1 2 0\n-1 2 0\n")
    val (kissatUnsat, _) = CertifyRunner.certifyCnf(cnf, dir.resolve("proof.drat"))
    kissatUnsat shouldBe false

  "certifyFrame" should "fully certify the 4⁴ frame sweep end-to-end" in:
    requireTools()
    val types  = List(List(4, 4, 4, 4))
    val dir    = Files.createTempDirectory(root, "certify-frame")
    val frames = SymbolAssembly.frames(types)
    frames should not be empty
    val certs  = frames.map((frame, chosen) => CertifyRunner.certifyFrame(types, frame, chosen, dir))
    for cert <- certs do withClue(cert.key + ": ") { cert.certified shouldBe true }
    // 4⁴ is realizable, so the sweep must find models somewhere — the obligation is not vacuous
    certs.map(_.models).sum should be > 0

  it should "certify pure-refutation frames through the unbroken tier (3.3.6.6, non-tiling)" in:
    requireTools()
    val types  = List(List(3, 3, 6, 6)) // angle-valid but non-tiling: its frames carry no σ₀ model
    val dir    = Files.createTempDirectory(root, "certify-refute")
    val frames = SymbolAssembly.frames(types)
    frames should not be empty
    val certs  = frames.map((frame, chosen) => CertifyRunner.certifyFrame(types, frame, chosen, dir))
    for cert <- certs do
      withClue(cert.key + ": ") {
        cert.models shouldBe 0
        // the assumption-free tier must have RUN and certified — Some(true), not a vacuous None
        cert.unbrokenKissatUnsat shouldBe Some(true)
        cert.unbrokenDratVerified shouldBe Some(true)
        cert.certified shouldBe true
      }
