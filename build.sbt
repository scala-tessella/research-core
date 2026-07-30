// research-core — shared substrate for the scala-tessella research programme. No theorems live here:
// individual results (their verification repos) depend on this library.
//
//   core   [package research_core]        — pure combinatorics + exact arithmetic: Frac, Cyclo24,
//            Signatures (vertex signatures), TypeCompatibility (polygon-alphabet support), the Delaney–Dress
//            symbol engine (DelaneySymbols), the exact angle/moduli layer (MetricLayer), exported rank
//            witnesses (RankWitness), reference data (TilingReference).
//   solver [package research_core.solver] — the SAT assembler (SymbolAssembly) + DRAT/UNSAT certification
//            harness (Certification, K1Certify, QuotientCertify, CertifyRunner; SAT4J in-process, external
//            kissat/drat-trim optional). A subpackage of core (no split package); needed only for the
//            combinatorial-completeness (exhaustiveness) surface, not for the metric specs.

ThisBuild / scalaVersion   := "3.8.4"
ThisBuild / organization   := "io.github.scala-tessella"
ThisBuild / versionScheme  := Some("early-semver")
ThisBuild / scalafmtOnCompile := true
ThisBuild / homepage       := Some(url("https://github.com/scala-tessella/research-core"))
ThisBuild / licenses       := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / developers     := List(
  Developer("scala-tessella", "scala-tessella", "mario.callisto@gmail.com", url("https://github.com/scala-tessella"))
)
ThisBuild / scmInfo        := Some(ScmInfo(
  url("https://github.com/scala-tessella/research-core"),
  "scm:git:git@github.com:scala-tessella/research-core.git"
))

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "io.github.scala-tessella" %% "ring-seq"        % "0.9.0",
    "org.scalatest"            %% "scalatest"       % "3.2.20"   % Test,
    "org.scalacheck"           %% "scalacheck"      % "1.19.0"   % Test,
    "org.scalatestplus"        %% "scalacheck-1-19" % "3.2.20.0" % Test
  )
)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings*)
  .settings(name := "research-core")

lazy val solver = project
  .in(file("solver"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings*)
  .settings(
    name := "research-core-solver",
    libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.6"
  )

lazy val root = project
  .in(file("."))
  .aggregate(core, solver)
  .settings(name := "research-core-root", publish / skip := true)
