# research-core

[![Maven Central](https://img.shields.io/maven-central/v/io.github.scala-tessella/research-core_3?label=Maven%20Central)](https://central.sonatype.com/artifact/io.github.scala-tessella/research-core_3)

Shared substrate for the `scala-tessella` research programme on tilings.

**No theorems live here.** This library holds only reusable machinery; individual results live in their own
verification repositories and depend on a pinned release of `research-core`.

## Modules

| Module | Artifact | Package | Contents |
|---|---|---|---|
| `core`   | `research-core`        | `io.github.scala_tessella.research_core`        | Pure combinatorics + exact arithmetic. Scala.js-clean (no JVM-only IO). |
| `solver` | `research-core-solver` | `io.github.scala_tessella.research_core.solver` | SAT assembler (`SymbolAssembly`) + DRAT/UNSAT certification harness (`Certification`, `K1Certify`, `K2Certify`, `QuotientCertify`, `CertifyRunner`; live solver in-process per platform — SAT4J on the JVM, CaDiCaL on Scala Native — external kissat/drat-trim optional). |

`solver` is a subpackage of `core` (no split package). This keeps `core`'s `private[research_core]` members
reachable from `solver` while they stay hidden from library users.

### `core` public surface

> **Reading this from a verification repository?** This page describes the current development line, which
> moves ahead of the archived releases that verification repositories pin. `UClass` and the widened
> `DelaneySymbols` entry points, for instance, arrived in 0.3.0 and are **absent from 0.2.1**. Check the
> pinned version's own archive and the [CHANGELOG](CHANGELOG.md) rather than this list. The newest archived
> release is 0.3.1, [doi:10.5281/zenodo.21739112](https://doi.org/10.5281/zenodo.21739112).

- `Frac` — exact rationals.
- `Cyclo24` — exact arithmetic in the 24th cyclotomic field (the angles that arise for `{3,4,6,8,12}` faces).
- `Signatures.VertexSignature` — vertex configurations. (`TypeCompatibility` provides polygon-alphabet
  support used internally by the engine.)
- `DelaneySymbols` — the Delaney–Dress symbol engine: types `DSymbol`, `Orbit`, `Tiling`, `DSet`; enumeration
  (`enumerateSymbols`, `enumerateRelaxedDetailed`, `relaxedDSets`, `relaxedOrbitBoundedDSets`,
  `bfsRelabelings`, `euclideanSymbolsOf`); the tier-1 curvature relaxation `tier1Feasible` (the
  euclidean-feasible ⇒ tier-1 lemma is proved in its scaladoc); minimality/quotients (`isMinimal`,
  `properQuotients`, `canonicalKey`); `regularPolygonVertices`.
- `MetricLayer` — the exact linear angle theory and moduli: `angleSystem`, `regularPoint`, `satisfies`,
  `maxClosureResidual`, `moduliDimension`, `nullspaceBasis`, `closureRank`, `exactSymmetryRealizable`.
- `RankWitness` — exported algebraic rank witnesses (pivot minors, kernel bases): `produce`, `verify`, `det`,
  `minor`, re-checkable by any computer algebra system.
- `UClass` — the U(z) class machinery (unit-edge tilings around a vertex figure): `candidates`,
  `designations`, `forcedRegular`, `noneForcedRegular`, `cyclicSubset`, `targets`.
- `SymbolRenderer` — barycentric development of a tiling straight from its minimal symbol, and SVG
  emission: `develop`, `toSvg`, `apothem`, `circumradius`, `reflect`. IO-free (the caller writes the
  returned string).
- `TilingReference` — reference data (e.g. `n1`, the 11 Archimedean vertex configurations).

The three-dimensional substrate (unit-edge honeycombs of `E³` by convex uniform cells):

- `HoneycombAlphabet` — the 13 core cells, their exact dihedrals in the two-tier lattice `15°ℤ + αℤ`
  (`α = arctan √2`), and the edge-figure enumerator.
- `CertifiedDihedrals` — interval-certified dihedrals of an arbitrary unit-edge uniform polyhedron from its
  vertex configuration alone: `Iv` interval arithmetic, circumradius by certified bisection, corner
  reconstruction.
- `SpeciesSupports` — the area equation for vertex figures (corner excesses, the parity condition), bounding
  the cell multiset of a vertex species.
- `SpeciesEnumerator` — the species table: edge-to-edge tilings of the sphere of directions by the rigid
  corner figures, assembled geometrically and deduplicated by canonical combinatorial-map key.
- `SpeciesCorona` — figure hosting, species adjacency, the face-cycle filter.
- `MonoShell` — the star-gluing atlas and the mono-species shell filter, on cell descriptors.
- `TransitivePatterns` — gluing patterns, the single-coset forcing theorem, coset skeletons, collision-free
  development, canonical fingerprints.
- `CompletenessAudit` — periodization, class coherence, fingerprint separation and germ forcing: the finite
  certificates a classification argument consumes.
- `StarChambers`, `StarFoldings`, `Sigma0Assembly`, `SymbolCatalog`, `SymbolRealization` — the Delaney–Dress
  side of a vertex star: chamber complex, subgroup lattice and foldings, the σ₀ assembly, canonical keys and
  minimality with the sweep drivers, and the minimal symbol of a certified honeycomb.

## Platforms

Both modules cross-build for the **JVM** and **Scala Native** 0.5 (`crossProject`, `CrossType.Full`), on
Cats Effect 3.7 / fs2 3.13 as the portable concurrency and process substrate. The whole test suite —
including the count-exact oracle gates and the kissat/drat-trim certification path — is green on both
platforms in CI.

The live SAT solver is per-platform behind `PlatformSolver`: SAT4J on the JVM, **CaDiCaL in-process
through an IPASIR binding** on Native (no process spawn, no DIMACS round-trip; `exactlyOne` expands
pairwise there, so the live encoding is literally the certified one). Enumeration parity between the two
solvers is asserted by running the same gate specs on both platforms.

Practical guidance from measurement: SAT-bound work (the certification harness) runs close to JVM speed
on Native; pure-combinatorics enumeration is markedly slower under Native debug builds — the JVM remains
the primary target for enumeration-heavy runs.

**Release-mode caveat (Scala Native 0.5.12)**: LLVM-optimized builds (`release-fast`/`release-full`)
of the multithreaded enumeration crash or hang under the default **Immix** GC (SIGSEGV at garbage
addresses or livelock; debug builds are correct) — observed on clang 14/macOS and clang 18/Linux alike,
with Scala Native's own optimizer on or off. The same optimized build runs correctly and fast under
**Boehm** (`SCALANATIVE_GC=boehm`, needs `libgc-dev`): use Boehm for any Native release build of the
enumeration until the upstream Immix interaction is fixed. The `Native benchmark` workflow encodes the
evidence ladder.

Native prerequisites: clang ≥ 16 (CI pins LLVM 18; older clang works with a deprecation warning) and, for
`solver`, `libcadical` on the library path (`brew install cadical`, or built from source with `-fPIC` as
in `.github/workflows/ci.yml`). The external certification tools live in `tools/bin/{kissat,drat-trim}`
(gitignored); specs cancel gracefully when they are absent.

## Build

```bash
sbt coreJVM/compile       # the pure library (JVM)
sbt compile               # library + solver, all platforms
sbt coreJVM/test solverJVM/test      # the JVM suite
sbt coreNative/test solverNative/test # the Scala Native suite (needs the prerequisites above)
```

## Release & local use

Publishing to Maven Central is tag-driven via `sbt-ci-release` (same flow as `scala-tessella/ring-seq`).

For a **local** cross-repo loop (so a verification repo can resolve `research-core` before a Central release):

```bash
git init && git add -A && git commit -m "research-core 0.1.0"
git tag v0.1.0            # sbt-dynver derives version 0.1.0 from the tag
sbt coreJVM/publishLocal  # publishes research-core_3 0.1.0 to ~/.ivy2/local
# add `sbt solverJVM/publishLocal` if the dependant needs the completeness surface;
# coreNative/solverNative publish the _native0.5_3 artifacts
```

Once released to Central, dependants declare:

```scala
libraryDependencies += "io.github.scala-tessella" %% "research-core" % "0.1.0"
```

## Versioning

`early-semver`. The `core` public surface above is the compatibility contract; keep it stable across a major
line so every archived verification repo keeps resolving. Release-by-release changes are in
[CHANGELOG.md](CHANGELOG.md).

### Archived releases

Each release is archived with its own version DOI, which is what a verification repository should cite (not
the all-versions concept DOI, which resolves to whatever is newest):

| Version | DOI |
|---|---|
| 0.3.1 | [10.5281/zenodo.21739112](https://doi.org/10.5281/zenodo.21739112) |
| 0.2.1 | [10.5281/zenodo.21708011](https://doi.org/10.5281/zenodo.21708011) |
