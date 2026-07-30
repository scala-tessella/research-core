package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.solver.Certification.*
import io.github.scala_tessella.research_core.Signatures.VertexSignature
import io.github.scala_tessella.research_core.solver.SymbolAssembly.*

import java.nio.file.{Files, Path}
import scala.collection.mutable

/** ADR-0008 D4 — the frame certification runner shared by the gate probes. Per frame: tee the live SAT4J
  * enumeration into DIMACS bodies, assemble the OBLIGATION instance (base CNF + the found models' blocking
  * clauses — pure refutation when there are no models), JVM-check every model against the base CNF, then
  * kissat (expect UNSAT, exit 20) and drat-trim (expect `s VERIFIED`, exit 0) on the obligation. For
  * refutation frames the assumption-free tier also emits and certifies the UNBROKEN CNF (no lex-leader
  * clauses). Artifacts under `dir/<frameKeyHash>/`; one manifest row per frame.
  */
object CertifyRunner:

  val kissat: Path            = Path.of("tools", "bin", "kissat")
  val dratTrim: Path          = Path.of("tools", "bin", "drat-trim")
  def toolsInstalled: Boolean = Files.isExecutable(kissat) && Files.isExecutable(dratTrim)

  final case class FrameCert(
      key: String,
      hash: String,
      vars: Int,
      baseClauses: Int,
      models: Int,
      capped: Boolean,
      jvmViolations: Int,
      kissatUnsat: Boolean,
      dratVerified: Boolean,
      unbrokenKissatUnsat: Option[Boolean],
      unbrokenDratVerified: Option[Boolean],
      millis: Long
  ):
    /** Fully certified: models clean on the JVM, obligation UNSAT with a VERIFIED proof (both tiers where
      * applicable), and the enumeration uncapped (a capped frame's obligation is meaningless).
      */
    def certified: Boolean =
      !capped && jvmViolations == 0 && kissatUnsat && dratVerified &&
        unbrokenKissatUnsat.forall(identity) && unbrokenDratVerified.forall(identity)

    def manifestRow: String =
      s"$hash\t$vars\t$baseClauses\t$models\t$capped\t$jvmViolations\t$kissatUnsat\t$dratVerified\t" +
        s"${unbrokenKissatUnsat.getOrElse("-")}\t${unbrokenDratVerified.getOrElse("-")}\t$millis\t$key"

  private def run(cmd: String*): (Int, String) =
    val p    = new ProcessBuilder(cmd*).redirectErrorStream(true).start()
    val out  = new String(p.getInputStream.readAllBytes())
    val code = p.waitFor()
    (code, out)

  private[research_core] def certifyCnf(cnf: Path, proof: Path): (Boolean, Boolean) =
    val (kc, _)   = run(kissat.toString, cnf.toString, proof.toString)
    val (_, dOut) = run(dratTrim.toString, cnf.toString, proof.toString)
    // verdict by the exact `s VERIFIED` line: drat-trim's exit code is 1 even on its trivial-UNSAT
    // verified path (input already contains the empty clause — the star-cut refutation frames)
    (kc == 20, dOut.linesIterator.exists(_.trim == "s VERIFIED"))

  def certifyFrame(
      types: List[VertexSignature],
      frame: Frame,
      chosen: Vector[Star],
      dir: Path,
      maxModels: Int = 20000
  ): FrameCert =
    val t0               = System.nanoTime()
    val key              = frameKey(types, chosen)
    val hash             = frameKeyHash(key)
    val fdir             = dir.resolve(hash)
    Files.createDirectories(fdir)
    val baseBody         = fdir.resolve("base.body")
    val blockBody        = fdir.resolve("blocking.body")
    val base             = DimacsSink(baseBody)
    val block            = DimacsSink(blockBody)
    val full             = mutable.ListBuffer.empty[Array[Int]]
    val (models, capped) =
      enumerateSigma0(frame, maxModels, frameSymmetries(frame, chosen), base, block, full += _)
    base.close()
    block.close()
    // JVM fidelity: every live model satisfies the emitted base CNF
    val baseCnf          = fdir.resolve("base.cnf")
    assemble(baseCnf, base.maxVar, base.clauseCount, baseBody)
    val baseClauses      = parseCnf(baseCnf) // parse once, check every model in memory
    val violations       = full.iterator.map(m => violatedClauses(baseClauses, m).size).sum
    // the obligation: base + blocking; UNSAT = exhaustiveness (pure refutation when models = 0)
    val instance         = fdir.resolve("instance.cnf")
    assemble(instance, base.maxVar, base.clauseCount + block.clauseCount, baseBody, blockBody)
    val (kUnsat, dVer)   = certifyCnf(instance, fdir.resolve("proof.drat"))
    // assumption-free tier (refutation frames only): the UNBROKEN CNF, no lex-leader clauses
    val (uK, uD)         =
      if models.nonEmpty then (None, None)
      else
        val ubBody = fdir.resolve("unbroken.body")
        val ub     = DimacsSink(ubBody)
        encodeSigma0(frame, Vector.empty, ub)
        ub.close()
        val ubCnf  = fdir.resolve("unbroken.cnf")
        assemble(ubCnf, ub.maxVar, ub.clauseCount, ubBody)
        val (k, d) = certifyCnf(ubCnf, fdir.resolve("unbroken.drat"))
        (Some(k), Some(d))
    FrameCert(
      key,
      hash,
      base.maxVar,
      base.clauseCount,
      models.size,
      capped,
      violations,
      kUnsat,
      dVer,
      uK,
      uD,
      (System.nanoTime() - t0) / 1000000
    )

  val manifestHeader: String =
    "hash\tvars\tbaseClauses\tmodels\tcapped\tjvmViolations\tkissatUnsat\tdratVerified\t" +
      "unbrokenKissat\tunbrokenDrat\tmillis\tkey"
