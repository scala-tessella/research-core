package io.github.scala_tessella.research_core.solver

import io.github.scala_tessella.research_core.Signatures.VertexSignature
import io.github.scala_tessella.research_core.solver.SymbolAssembly.{canonicalStarKey, ClauseSink, Star}

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable
import scala.util.Using

/** DRAT certification support (ADR-0008 D2): DIMACS emission of the σ₀ CNF, the pure-JVM model-evaluation
  * check, and deterministic frame keys. Dev/CI machinery — the enumeration itself stays on SAT4J; the
  * external kissat/drat-trim steps consume what is emitted here.
  */
object Certification:

  // ---- clause-expanding sinks ---------------------------------------------------------------------------

  /** A sink that expands exactly-one to plain clauses PAIRWISE: at-least-one + every binary at-most-one, NO
    * auxiliary variables — so variable numbering is identical to the native-cardinality SAT4J instance (the
    * cross-sink contract blocking-clause transfer and per-model checks rely on).
    */
  trait ExpandingSink extends ClauseSink:
    final def exactlyOne(lits: Array[Int]): Unit =
      clause(lits.toSeq)
      for i <- lits.indices; j <- i + 1 until lits.length do clause(List(-lits(i), -lits(j)))

  /** In-memory sink — blocking clauses (≤ #chambers wide, ≤ maxModels many) and tests. */
  final class BufferSink extends ExpandingSink:
    val clauses: mutable.ArrayBuffer[Seq[Int]] = mutable.ArrayBuffer.empty
    def clause(lits: Seq[Int]): Unit           = clauses += lits

  /** Detection-only sink: no storage, no expansion (`exactlyOne` is O(1) — it can never be the empty clause).
    * `sawEmptyClause` marks a TRIVIAL refutation: an instance carrying the empty clause is UNSAT on sight,
    * needs no solver and no proof — and at n=8 scale most frames die exactly this way (an uncrossable star
    * cut), so skipping their emission is what makes certification runs tractable. The empty clause is
    * symmetry-independent (star-cut emission ignores the lex-leader targets), so a trivial verdict covers the
    * unbroken tier too.
    */
  final class CountingSink extends ClauseSink:
    private var vars                       = 0
    private var count                      = 0
    private var empty                      = false
    def maxVar: Int                        = vars
    def clauseCount: Int                   = count
    def sawEmptyClause: Boolean            = empty
    def clause(lits: Seq[Int]): Unit       =
      if lits.isEmpty then empty = true
      else for l <- lits do vars = vars.max(math.abs(l))
      count += 1
    def exactlyOne(lits: Array[Int]): Unit =
      for l <- lits do vars = vars.max(math.abs(l))
      count += 1

  /** Streams a DIMACS clause BODY to `body` (one `l₁ … l_k 0` line per clause), tracking the variable and
    * clause counts the header needs. Bodies are separate from headers so obligations can be assembled by
    * concatenation (base body + blocking body) without buffering — buffering clauses OOM'd n=4-sized frames
    * long ago, and certification-sized frames are bigger.
    */
  final class DimacsSink(body: Path) extends ExpandingSink with AutoCloseable:
    private val w                    = Files.newBufferedWriter(body)
    private var vars                 = 0
    private var nClauses             = 0
    def maxVar: Int                  = vars
    def clauseCount: Int             = nClauses
    def clause(lits: Seq[Int]): Unit =
      for l <- lits do vars = vars.max(math.abs(l))
      w.write(lits.mkString("", " ", " 0\n"))
      nClauses += 1
    def close(): Unit                = w.close()

  /** Write a complete DIMACS file: `p cnf nVars nClauses` header + the given clause bodies, in order. */
  def assemble(target: Path, nVars: Int, nClauses: Int, bodies: Path*): Unit =
    Using.resource(
      Files.newOutputStream(target, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    ) { out =>
      out.write(s"p cnf $nVars $nClauses\n".getBytes)
      for b <- bodies do Files.copy(b, out)
    }

  // ---- the pure-JVM model-evaluation check --------------------------------------------------------------

  /** True when the clause is satisfied under the model given as SAT4J's `model()` literal array (positive
    * literal = true variable). Variables absent from the model count as false.
    */
  def clauseSatisfied(lits: Seq[Int], model: Array[Int]): Boolean =
    val pos = model.iterator.filter(_ > 0).toSet
    lits.exists(l => if l > 0 then pos(l) else !pos(-l))

  /** Parse a DIMACS file ONCE into primitive literal arrays (comment/header lines skipped, clause order
    * preserved, terminating 0 dropped) — the per-frame form of the fidelity check: checking thousands of
    * models against [[violatedClauses]]'s path overload re-reads and re-tokenizes the file per model.
    */
  def parseCnf(cnf: Path): Array[Array[Int]] =
    Using.resource(Files.newBufferedReader(cnf)) { r =>
      val out  = Array.newBuilder[Array[Int]]
      var line = r.readLine()
      while line != null do // scalafix:ok DisableSyntax.null
        if !(line.isBlank || line.startsWith("c") || line.startsWith("p")) then
          out += line.trim.split("\\s+").iterator.map(_.toInt).takeWhile(_ != 0).toArray
        line = r.readLine()
      out.result()
    }

  /** The 0-based indices of the clauses of a DIMACS file the model VIOLATES — the fidelity check (ADR-0008
    * Gate 1): a genuine model of the live SAT4J instance must violate NOTHING in the emitted base CNF, and
    * exactly its own blocking clause in a full obligation file. One-shot form; parse once via [[parseCnf]]
    * when checking many models against the same file.
    */
  def violatedClauses(cnf: Path, model: Array[Int]): Vector[Int] =
    violatedClauses(parseCnf(cnf), model)

  /** [[violatedClauses]] against a pre-parsed CNF: primitive truth table, no per-model re-parse. */
  def violatedClauses(clauses: Array[Array[Int]], model: Array[Int]): Vector[Int] =
    // truth of variable a = pos(a); a literal beyond the model's variables is unassigned = false,
    // matching the Set-of-positives semantics of the streaming version
    var maxVar = 0
    var i      = 0
    while i < model.length do
      val a = math.abs(model(i))
      if a > maxVar then maxVar = a
      i += 1
    val pos    = new Array[Boolean](maxVar + 1)
    i = 0
    while i < model.length do
      val l = model(i)
      if l > 0 then pos(l) = true
      i += 1
    val out    = Vector.newBuilder[Int]
    var c      = 0
    while c < clauses.length do
      val lits      = clauses(c)
      var satisfied = false
      var j         = 0
      while j < lits.length && !satisfied do
        val l = lits(j)
        satisfied = if l > 0 then l <= maxVar && pos(l) else -l > maxVar || !pos(-l)
        j += 1
      if !satisfied then out += c
      c += 1
    out.result()

  // ---- deterministic frame keys -------------------------------------------------------------------------

  /** The full, human-readable identity of an assembly frame: the (already sorted) type multiset + the
    * canonical key of each chosen folded star. Reproducible across runs — the manifest row.
    */
  def frameKey(types: Seq[VertexSignature], chosen: Seq[Star]): String =
    types.map(_.mkString(".")).mkString("[", ";", "]") + "|" + chosen.map(canonicalStarKey).mkString(";")

  /** Filesystem-safe short form of [[frameKey]] (16 hex chars of SHA-256) — the artifact directory name; the
    * manifest maps it back to the full key.
    */
  def frameKeyHash(key: String): String =
    Sha256
      .digest(key.getBytes("UTF-8"))
      .take(8)
      .map(b => f"$b%02x")
      .mkString
