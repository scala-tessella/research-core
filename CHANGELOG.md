# Changelog

All notable changes to `research-core` are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). The `core` public surface
listed in the README is the compatibility contract.

## [Unreleased]

The cross-platform release: `core` and `solver` cross-build for the JVM and Scala Native
(`crossProject`, `CrossType.Full`), with Cats Effect 3.7 / fs2 3.13 as the portable concurrency and
process substrate. Both platforms are fully green on the whole suite, including the count-exact oracle
gates (G1/G2/G3 and the Krotenheerdt ladder) — on Native with CaDiCaL as the live solver, which is the
strongest cross-solver enumeration-parity check the library can express.

### Added

- **`SatSolver`** (`solver`, shared) — the incremental CDCL surface the enumerators actually use
  (`addClause`/`exactlyOne`/`solve`/`model`), extracted behind a platform-neutral trait with the
  solver-agnostic `SatSolver.Contradiction` replacing SAT4J's `ContradictionException` in control flow.
  The live solver is the per-platform `PlatformSolver`: SAT4J on the JVM (`Sat4jSolver`), CaDiCaL
  in-process through an IPASIR `@extern` binding on Scala Native (`CadicalSolver` — pairwise
  `exactlyOne`, so the live encoding is literally the certified one there; `timeoutSeconds` enforced via
  the IPASIR terminate callback). `enumerateSigma0` gains a `newSolver` factory parameter, and solver
  timeouts surface as the platform-neutral `SatSolver.Timeout` on both platforms (translated from SAT4J's
  `TimeoutException` on the JVM).
- **`Sha256`** (`solver`, shared) — pure-Scala SHA-256 for `frameKeyHash`
  (`java.security.MessageDigest` is absent from Scala Native's javalib), NIST-vector tested and
  property-checked against `MessageDigest` on the JVM, so frame hashes stay bit-identical across
  platforms.
- **`BackTracker.parallelForeachCE`** (`core`) — a Cats Effect twin of the ForkJoin work-stealing walk
  (fibers only at branch points, forking budgeted): at parity with ForkJoin on the JVM and ~1.5× faster
  than it on Native, where it is the intended engine. Callers stay on ForkJoin on the JVM.

### Fixed

- **`TransitivePatterns.searchPatterns` under-reported `capped`**: the `Some(v)` backtracking branch (the
  one every real cap-hit unwinds through — measured 43/43 on the species corpus) never set the flag, so
  truncated searches were reported as complete; the exhaustion certificate (`!capped && allKnown`)
  consumed that flag. Both branches now report truncation precisely (candidates remaining when the cap
  broke the loop), verified exact — zero false negatives AND zero false positives — against ground-truth
  re-runs at cap + 1 over the whole corpus (`CappedProbeSpec`, kept as regression teeth). `analyze` no
  longer counts the forced cap-1 truncation as capped (by the forcing theorem it misses no honeycomb), so
  `Report.capped` semantics for forced species are unchanged.

### Changed

- **`CertifyRunner`** externals (kissat, drat-trim) run through fs2-io processes on `IO`;
  `certifyCnf`/`certifyFrame` keep their synchronous signatures as facades over new `IO` variants.
- **`Sat4jSink`** is now a top-level JVM-only class (formerly `SymbolAssembly.Sat4jSink`), kept for
  downstream source compatibility; `SymbolAssembly` itself is SAT4J-free and cross-compiles.
- Native linking requires `libcadical` (e.g. Homebrew) on the library path.
- Known Scala Native 0.5.12 limitation (documented in the README, measured by the `Native benchmark`
  workflow): LLVM-optimized builds of the multithreaded enumeration fault under the default Immix GC —
  use `SCALANATIVE_GC=boehm` for Native release builds; debug builds are unaffected.

## [0.5.0] — 2026-08-02

The honeycomb release: the three-dimensional substrate joins the library — the cell alphabet and its
interval-certified dihedrals, the vertex-species assembly on the sphere of directions, the star-gluing
atlas, the pattern/development engine and its audit certificates, and the Delaney–Dress symbol side of a
vertex star (chambers, foldings, the σ₀ assembly, the symbol catalog and the k = 1 realization). These are
the engines the `convex-uniform-honeycombs` verification repository asserts against, and every one of them
is also on the path of the k ≥ 2 programme, which is why they belong here rather than in a result's own
repository. No results are stated here: every enumeration COUNT is asserted by the verification repository
of the paper that states it. IO-free and free of JVM-only concurrency, so `core` stays Scala.js-clean.

### Added

- **`HoneycombAlphabet`** (`core`) — the core cell alphabet of the three-dimensional programme: the 13
  unit-edge convex uniform polyhedra, their exact dihedrals in the two-tier lattice `15°ℤ + αℤ`
  (`α = arctan √2`, irrational in degrees by Niven), and the edge-figure enumerator. Because the edge
  equation `Σθ = 360°` splits into `Σn = 0` and `Σr = 360`, edge figures are a finite linear-diophantine
  enumeration — no transcendence theory inside the alphabet.
- **`CertifiedDihedrals`** (`core`) — interval-certified dihedrals for an ARBITRARY unit-edge uniform
  polyhedron, from its vertex configuration alone (no coordinate tables trusted): certified bisection for
  the vertex-figure circumradius, corner reconstruction, interval propagation. An interval MISS certifies
  non-equality, so exclusions proved with these intervals are genuine proofs.
- **`SpeciesSupports`** (`core`) — the area equation for vertex figures: corner solid angles live in the
  same two-tier lattice, so "corners tile the 720° sphere" splits exactly as the edge equation does, and
  with the per-face-size parity condition bounds the cell multiset of every possible vertex species.
- **`SpeciesEnumerator`** (`core`) — the species table: all edge-to-edge tilings of the sphere of
  directions by the rigid corner figures of the alphabet, assembled by geometric depth-first search with
  interval rotations, pruned by the exact area/support/vertex-sum constraints, deduplicated by the
  canonical key of the labeled combinatorial map.
- **`SpeciesCorona`** (`core`) — corona structure over the species: the figure-hosting table, the species
  adjacency graph, and the face-cycle filter (the planar odd-face walk, one dimension up).
- **`MonoShell`** (`core`) — the star-gluing atlas and the mono-species shell filter, decided on CELL
  DESCRIPTORS (type plus the two face germs at an edge), so ring agreement is descriptor-multiset equality
  and candidate motions need no case analysis.
- **`TransitivePatterns`** (`core`) — gluing patterns, the forcing theorem for single-coset species, coset
  skeletons, collision-free breadth-first development, and canonical development fingerprints.
- **`CompletenessAudit`** (`core`) — the four finite certificates that upgrade a finite-radius pattern
  enumeration to a classification statement: periodization (translation words, ball periodicity, lattice
  invariance, coverage), class coherence at the determination radius, fingerprint separation, and germ
  forcing for cap closure.
- **`SpeciesCorona.ringAt`**, **`SpeciesCorona.pathExists`** and **`StarChambers.orbitSize`** are public
  rather than `private[research_core]`. All three are what a spec needs to check the engines against
  INDEPENDENT data: `ringAt` walks the corner ring at a tiling vertex purely combinatorially, so a spec can
  confront the chamber complex's `(σ₂σ₃)`-orbit sizes with ring sizes derived a different way; `pathExists`
  is the face-cycle relation's reachability, checkable on hand-built relations; `orbitSize` is the orbit
  length of an alternating composition. Same signatures, no behaviour change.
- **`StarChambers`**, **`StarFoldings`**, **`Sigma0Assembly`**, **`SymbolCatalog`**,
  **`SymbolRealization`** (`core`) — the Delaney–Dress side of a vertex star: the chamber complex of a
  species and its flag laws, the full subgroup lattice and the folded complexes, the exhaustive σ₀
  assembly joining k folded stars into a symbol candidate, canonical keys with minimality by congruence
  closure plus the k = 1 and k-orbit sweep drivers, and the derivation of a certified honeycomb's minimal
  symbol from its certified pattern.

## [0.4.0] — 2026-08-02

The atlas release: the symbol developer joins the library, and the frame sweep the certification drivers
iterate becomes public API. Both are what the `krotenheerdt-tilings` verification repository needs from
here; no behaviour changes.

### Added

- **`SymbolRenderer`** (`core`) — barycentric development of a euclidean regular-polygon tiling directly
  from its (minimal) Delaney–Dress symbol, plus SVG emission: `develop` (BFS over σ₀/σ₁/σ₂ placing each
  chamber's flag triangle, returning the complete faces), `toSvg` (self-contained document with a banner,
  in two forms — from developed faces, or from a symbol in one call), and the geometry leaves `apothem`,
  `circumradius`, `reflect`. Developing the QUOTIENT symbol unfolds the orbifold, so branchings and mirror
  chains need no special handling. IO-free by design — the document is returned as a string and writing it
  is the caller's business — so `core` stays Scala.js-clean.

### Changed

- **`SymbolAssembly.frames`** and **`SymbolAssembly.frameSymmetries`** (`solver`) are now public. Both were
  `private[research_core]`, hence reachable only from inside the library, while a per-frame certification
  driver living in a verification repository needs exactly this pair: `frames` to iterate the sweep, and
  `frameSymmetries` to rebuild the lex-leader targets of a frame it wants to encode both with and without
  symmetry breaking. Same signatures, same order, no behaviour change.

## [0.3.1] — 2026-08-01

Archived as [doi:10.5281/zenodo.21739112](https://doi.org/10.5281/zenodo.21739112) — the version DOI to cite
when pinning this release.

The tier-1 certification release: the machinery behind the k ≤ 2 completeness certificate of the
minimal-uniformity paper (certification track A2) joins the library — a curvature relaxation local enough
for SAT, its orbit-bounded generation universe, and the two-orbit completeness encoding.

### Added

- **`DelaneySymbols.tier1Feasible`** (`core`) — the tier-1 curvature relaxation, exact integer arithmetic
  in twelfths, with THE LEMMA proved in its scaladoc: every euclidean-feasible D-set satisfies
  `#good ≥ 3C − 12·vSum`, where `good(d)` ⟺ `(σ₀σ₁)³(d) = d` ⟺ d's tile orbit has branching r ∈ {1, 3}
  (the alternating orbit's π-period equals r for chains and cycles alike), and `12·vSum` is the
  vertex-orbit side of the curvature sum. The condition is local (no orbit-length machinery), which is
  what lets curvature into a completeness CNF; it is maximally tight at the top chamber counts (at
  C = 24 with two vertex orbits it forces every tile orbit to r ∈ {1, 3} and both vertex orbits to
  cycles of length ≥ 6).
- **`DelaneySymbols.relaxedOrbitBoundedDSets`** (`core`) — streaming, parallel generation of the
  ≤ maxN-vertex-orbit certification universe (canonically labeled, NO curvature pruning), with the
  monotone closed-vertex-orbit tree prune, and — under `tier1 = true` — the tier-1 filter plus its
  matching monotone tree prune (closed bad-tile chambers > 48 − 2·size). At maxN = 2, maxSize = 24 the
  raw universe is ~10⁸ D-sets at the top slices; the tier-1 universe is 2,710.
- **`K2Certify`** (`solver`) — the SAT encoding behind the k ≤ 2 completeness obligation: the `K1Certify`
  core (pair variables, (σ₀σ₂)² = id, BFS-consistent numbering) plus an exact ≤ 2-vertex-orbit layer
  (σ₁/σ₂-invariant 2-coloring, anchored level-reachability — the 2^{C−1} cut clauses of the k = 1 track
  do not scale) and the tier-1 layer (one-directional witness chains for `good`, exact unary counters,
  per-class contribution selectors). Models can never overstate their case; every tier-1 labeling
  extends to a model. `enumerate` mirrors `K1Certify.enumerate` (blocking on pair variables).
- `K2CertifySpec` — agreement with the generator at C ≤ 8 op-for-op; negative controls for both new
  layers (a valid 3-vertex-orbit D-set excluded; tier-1-infeasible ≤ 2-orbit D-sets excluded,
  non-vacuously).

### Notes

- The consuming campaign (universe, per-C agreement/fidelity/DRAT obligations, euclidean tail) lives in
  the `minimal-uniformity-three` verification repository, which pins this release; the certificate's
  verdict is recorded there.

## [0.3.0] — 2026-07-30

The U(z) class release: the machinery for deciding U-class membership joins the `core` public surface,
together with the `DelaneySymbols` entry points a downstream U-class scan needs.

### Added

- **`UClass`** (`core`, `io.github.scala_tessella.research_core`) — the U(z) class of unit-edge tilings,
  formalized on Delaney–Dress symbols: a tiling belongs to U(z) iff some vertex is all-regular with configuration
  exactly `z`, every vertex's regular sub-configuration (RVS) is non-empty, and every RVS is a cyclic subset
  of `z`. Public members:
  - `cyclicSubset(w, z)` — `w` is a contiguous cyclic subword of `z`, up to rotation and reflection.
  - `designations(ds, z)` — all designations (sets of face-orbit indices declared regular) under which a
    symbol satisfies U(z) *combinatorially*.
  - `candidates(ds, z)` — the designations that additionally survive the pinned linear layer (angle system
    plus `γ = (p−2)/p` for every corner of a regular-designated face orbit).
  - `forcedRegular(ds, regular, f)` — whether the pinned affine system already forces an irregular-designated
    face orbit to its regular angles.
  - `noneForcedRegular(ds, regular, irregular)` — the same test over every irregular face at once, with the
    invariant base system and its nullity computed once; a designation is genuine only if it passes.
  - `targets` — the ten conjecture targets (raw cyclic sequences) with their claimed minimal uniformities.
- `UClassUnitSpec` — unit tests for the above.

### Changed

- `DelaneySymbols` members widened from `private[research_core]` to public, so a U-class scan can be driven
  from outside the library: `collectOrbits`, `vertexConfigOrbits`, `enumerateRelaxedParallel`, and the
  `DSymbol.isEuclidean` extension.
- README and the `build.sbt` module header document `UClass` as part of the `core` public surface.

### Compatibility

Source- and binary-compatible with 0.2.1 for library users: every change is an addition or a widening of
visibility. No behaviour of existing public members changed.

## [0.2.1] — 2026-07-30

Archived as [doi:10.5281/zenodo.21708011](https://doi.org/10.5281/zenodo.21708011) — the version DOI to cite
when pinning this release.

### Changed

- `CertifyRunner.certifyCnf` (`solver`) widened from `private[research_core]` to public, so a verification
  repository can certify a CNF/DRAT pair directly (external `kissat` + `drat-trim`, verdict taken from the
  exact `s VERIFIED` line) without going through the bundled runner entry points.

[0.4.0]: https://github.com/scala-tessella/research-core/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/scala-tessella/research-core/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/scala-tessella/research-core/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/scala-tessella/research-core/compare/v0.2.0...v0.2.1
