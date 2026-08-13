package io.github.scala_tessella.research_core.solver

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.parallel.*
import fs2.io.process.ProcessBuilder as ExternalProcess
import fs2.text
import io.github.scala_tessella.research_core.Signatures.VertexSignature
import io.github.scala_tessella.research_core.solver.Certification.*
import io.github.scala_tessella.research_core.solver.SymbolAssembly.*

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Using

/** D4 — the frame certification runner shared by the gate probes. Per frame: tee the live SAT enumeration
  * into DIMACS bodies, assemble the OBLIGATION instance (base CNF + the found models' blocking clauses — pure
  * refutation when there are no models), JVM-check every model against the base CNF, then kissat (expect
  * UNSAT, exit 20) and drat-trim (expect `s VERIFIED`, exit 0) on the obligation. For refutation frames the
  * assumption-free tier also emits and certifies the UNBROKEN CNF (no lex-leader clauses). Artifacts under
  * `dir/<frameKeyHash>/`; one manifest row per frame.
  *
  * Externals run through fs2-io processes on Cats Effect (portable to Scala Native); the `certifyCnf` /
  * `certifyFrame` facades keep the original synchronous signatures over the `IO` variants.
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

  /** Spawn an external process, drain stdout and stderr concurrently (concatenated, matching the old
    * `redirectErrorStream` merge for the line greps), return (exit code, combined output).
    */
  private[solver] def run(cmd: String*): IO[(Int, String)] =
    ExternalProcess(cmd.head, cmd.tail.toList).spawn[IO].use { p =>
      (
        p.stdout.through(text.utf8.decode).compile.string,
        p.stderr.through(text.utf8.decode).compile.string
      ).parTupled.flatMap((out, err) => p.exitValue.map(code => (code, out + err)))
    }

  def certifyCnfIO(cnf: Path, proof: Path): IO[(Boolean, Boolean)] =
    for
      (kc, _)   <- run(kissat.toString, cnf.toString, proof.toString)
      (_, dOut) <- run(dratTrim.toString, cnf.toString, proof.toString)
    // verdict by the exact `s VERIFIED` line: drat-trim's exit code is 1 even on its trivial-UNSAT
    // verified path (input already contains the empty clause — the star-cut refutation frames)
    yield (kc == 20, dOut.linesIterator.exists(_.trim == "s VERIFIED"))

  def certifyCnf(cnf: Path, proof: Path): (Boolean, Boolean) = certifyCnfIO(cnf, proof).unsafeRunSync()

  /** kissat verdict alone (exit 10 = SAT) — the SAT-direction gates need satisfiability, no proof. */
  private[solver] def kissatSatIO(cnf: Path): IO[Boolean] =
    run(kissat.toString, cnf.toString).map(_._1 == 10)

  /** The emitted artifacts and in-memory verdicts of the enumeration tier of one frame — everything the
    * external certification steps consume.
    */
  final private case class Prepared(
      key: String,
      hash: String,
      fdir: Path,
      vars: Int,
      baseClauses: Int,
      models: Int,
      capped: Boolean,
      jvmViolations: Int,
      instance: Path
  )

  def certifyFrameIO(
      types: List[VertexSignature],
      frame: Frame,
      chosen: Vector[Star],
      dir: Path,
      maxModels: Int = 20000
  ): IO[FrameCert] =
    for
      t0             <- IO.monotonic
      prep           <- IO.blocking(prepare(types, frame, chosen, dir, maxModels))
      (kUnsat, dVer) <- certifyCnfIO(prep.instance, prep.fdir.resolve("proof.drat"))
      // assumption-free tier (refutation frames only): the UNBROKEN CNF, no lex-leader clauses
      (uK, uD)       <-
        if prep.models > 0 then IO.pure((None, None))
        else
          IO.blocking {
            val ubBody            = prep.fdir.resolve("unbroken.body")
            val (ubVars, ubCount) = Using.resource(DimacsSink(ubBody)) { ub =>
              encodeSigma0(frame, Vector.empty, ub): Unit
              (ub.maxVar, ub.clauseCount)
            }
            val ubCnf             = prep.fdir.resolve("unbroken.cnf")
            assemble(ubCnf, ubVars, ubCount, ubBody)
            ubCnf
          }.flatMap(ubCnf => certifyCnfIO(ubCnf, prep.fdir.resolve("unbroken.drat")))
            .map((k, d) => (Some(k), Some(d)))
      t1             <- IO.monotonic
    yield FrameCert(
      prep.key,
      prep.hash,
      prep.vars,
      prep.baseClauses,
      prep.models,
      prep.capped,
      prep.jvmViolations,
      kUnsat,
      dVer,
      uK,
      uD,
      (t1 - t0).toMillis
    )

  def certifyFrame(
      types: List[VertexSignature],
      frame: Frame,
      chosen: Vector[Star],
      dir: Path,
      maxModels: Int = 20000
  ): FrameCert = certifyFrameIO(types, frame, chosen, dir, maxModels).unsafeRunSync()

  /** The synchronous enumeration tier: live SAT enumeration tee'd into DIMACS bodies, the JVM fidelity check
    * of every model against the emitted base CNF, and the obligation instance (base + blocking).
    */
  private def prepare(
      types: List[VertexSignature],
      frame: Frame,
      chosen: Vector[Star],
      dir: Path,
      maxModels: Int
  ): Prepared =
    val key                                     = frameKey(types, chosen)
    val hash                                    = frameKeyHash(key)
    val fdir                                    = dir.resolve(hash)
    Files.createDirectories(fdir)
    val baseBody                                = fdir.resolve("base.body")
    val blockBody                               = fdir.resolve("blocking.body")
    val full                                    = mutable.ListBuffer.empty[Array[Int]]
    // bracketed: the sinks must not leak when the enumeration throws (SatSolver.Timeout, IO errors), and
    // must be closed — flushed — BEFORE the bodies are assembled below, hence counts captured on the way out
    val (models, capped, maxVar, nBase, nBlock) =
      Using.resources(DimacsSink(baseBody), DimacsSink(blockBody)) { (base, block) =>
        val (models, capped) =
          enumerateSigma0(frame, maxModels, frameSymmetries(frame, chosen), base, block, full += _)
        (models, capped, base.maxVar, base.clauseCount, block.clauseCount)
      }
    // JVM fidelity: every live model satisfies the emitted base CNF
    val baseCnf                                 = fdir.resolve("base.cnf")
    assemble(baseCnf, maxVar, nBase, baseBody)
    val baseClauses                             = parseCnf(baseCnf) // parse once, check every model in memory
    val violations                              = full.iterator.map(m => violatedClauses(baseClauses, m).size).sum
    // the obligation: base + blocking; UNSAT = exhaustiveness (pure refutation when models = 0)
    val instance                                = fdir.resolve("instance.cnf")
    assemble(instance, maxVar, nBase + nBlock, baseBody, blockBody)
    Prepared(key, hash, fdir, maxVar, nBase, models.size, capped, violations, instance)

  val manifestHeader: String =
    "hash\tvars\tbaseClauses\tmodels\tcapped\tjvmViolations\tkissatUnsat\tdratVerified\t" +
      "unbrokenKissat\tunbrokenDrat\tmillis\tkey"
