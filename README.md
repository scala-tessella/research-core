# research-core

[![Maven Central](https://img.shields.io/maven-central/v/io.github.scala-tessella/research-core_3?label=Maven%20Central)](https://central.sonatype.com/artifact/io.github.scala-tessella/research-core_3)

Shared substrate for the `scala-tessella` research programme on tilings.

**No theorems live here.** This library holds only reusable machinery; individual results live in their own
verification repositories and depend on a pinned release of `research-core`.

## Modules

| Module | Artifact | Package | Contents |
|---|---|---|---|
| `core`   | `research-core`        | `io.github.scala_tessella.research_core`        | Pure combinatorics + exact arithmetic. Scala.js-clean (no JVM-only IO). |
| `solver` | `research-core-solver` | `io.github.scala_tessella.research_core.solver` | SAT assembler + DRAT/UNSAT certification harness (SAT4J, JVM-only). |

`solver` is a subpackage of `core` (no split package). This keeps `core`'s `private[research_core]` members
reachable from `solver` while they stay hidden from library users.

### `core` public surface

- `Frac` — exact rationals.
- `Cyclo24` — exact arithmetic in the 24th cyclotomic field (the angles that arise for `{3,4,6,8,12}` faces).
- `Signatures.VertexSignature` — vertex configurations. (`TypeCompatibility` provides polygon-alphabet
  support used internally by the engine.)
- `DelaneySymbols` — the Delaney–Dress symbol engine: types `DSymbol`, `Orbit`, `Tiling`; enumeration
  (`enumerateSymbols`, `enumerateRelaxedDetailed`); minimality/quotients (`isMinimal`, `properQuotients`,
  `canonicalKey`); `regularPolygonVertices`.
- `MetricLayer` — the exact linear angle theory and moduli: `angleSystem`, `regularPoint`, `satisfies`,
  `maxClosureResidual`, `moduliDimension`, `nullspaceBasis`, `closureRank`, `exactSymmetryRealizable`.
- `TilingReference` — reference data (e.g. `n1`, the 11 Archimedean vertex configurations).

## Build

```bash
sbt core/compile          # the pure library
sbt compile               # library + solver
sbt test
```

## Release & local use

Publishing to Maven Central is tag-driven via `sbt-ci-release` (same flow as `scala-tessella/ring-seq`).

For a **local** cross-repo loop (so a verification repo can resolve `research-core` before a Central release):

```bash
git init && git add -A && git commit -m "research-core 0.1.0"
git tag v0.1.0            # sbt-dynver derives version 0.1.0 from the tag
sbt core/publishLocal     # publishes research-core_3 0.1.0 to ~/.ivy2/local
# add `sbt solver/publishLocal` if the dependant needs the completeness surface
```

Once released to Central, dependants declare:

```scala
libraryDependencies += "io.github.scala-tessella" %% "research-core" % "0.1.0"
```

## Versioning

`early-semver`. The `core` public surface above is the compatibility contract; keep it stable across a major
line so every archived verification repo keeps resolving.
