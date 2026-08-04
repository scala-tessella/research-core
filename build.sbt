// research-core — shared substrate for the scala-tessella research programme. No theorems live here:
// individual results (their verification repos) depend on this library.
//
//   core   [package research_core]        — pure combinatorics + exact arithmetic: Frac, Cyclo24,
//            Signatures (vertex signatures), TypeCompatibility (polygon-alphabet support), the Delaney–Dress
//            symbol engine (DelaneySymbols), the exact angle/moduli layer (MetricLayer), exported rank
//            witnesses (RankWitness), the U(z) class machinery (UClass), reference data (TilingReference).
//   solver [package research_core.solver] — the SAT assembler (SymbolAssembly) + DRAT/UNSAT certification
//            harness (Certification, K1Certify, QuotientCertify, CertifyRunner; SAT4J in-process on the JVM,
//            external kissat/drat-trim optional). A subpackage of core (no split package); needed only for
//            the combinatorial-completeness (exhaustiveness) surface, not for the metric specs.
//
// Both modules cross-build for the JVM and Scala Native (CrossType.Full: shared/ + jvm/ + native/ source
// dirs). SAT4J is JVM-only; the Native live solver is the CaDiCaL IPASIR binding behind PlatformSolver.

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion   := "3.8.4"
ThisBuild / organization   := "io.github.scala-tessella"
ThisBuild / versionScheme  := Some("early-semver")
ThisBuild / scalafmtOnCompile := true
ThisBuild / homepage       := Some(url("https://github.com/scala-tessella/research-core"))
ThisBuild / licenses       := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / developers     := List(
  Developer("scala-tessella", "Mario Càllisto", "mario.callisto@gmail.com", url("https://github.com/scala-tessella"))
)
ThisBuild / scmInfo        := Some(ScmInfo(
  url("https://github.com/scala-tessella/research-core"),
  "scm:git:git@github.com:scala-tessella/research-core.git"
))

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "io.github.scala-tessella" %%% "ring-seq"        % "0.9.0",
    "org.typelevel"            %%% "cats-effect"     % "3.7.0",
    "org.scalatest"            %%% "scalatest"       % "3.2.20"   % Test,
    "org.scalacheck"           %%% "scalacheck"      % "1.19.0"   % Test,
    "org.scalatestplus"        %%% "scalacheck-1-19" % "3.2.20.0" % Test
  )
)

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(commonSettings*)
  .settings(name := "research-core")

lazy val solver = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("solver"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings*)
  .settings(
    name := "research-core-solver",
    libraryDependencies += "co.fs2" %%% "fs2-io" % "3.13.0"
  )
  .jvmSettings(
    libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.6"
  )
  .nativeSettings(
    // libcadical (Homebrew) provides the IPASIR symbols; it is C++, hence the C++ runtime
    nativeConfig ~= { c =>
      val brewLibs = Seq("/usr/local/lib", "/opt/homebrew/lib")
        .filter(p => java.nio.file.Files.isDirectory(java.nio.file.Path.of(p)))
      c.withLinkingOptions(c.linkingOptions ++ brewLibs.map("-L" + _) :+ "-lc++")
    }
  )

lazy val root = project
  .in(file("."))
  .aggregate(core.jvm, core.native, solver.jvm, solver.native)
  .settings(name := "research-core-root", publish / skip := true)
