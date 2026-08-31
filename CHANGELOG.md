# Changelog

All notable changes to `research-core` are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). The `core` public surface
listed in the README is the compatibility contract.

## [0.8.1] — 2026-08-31

**A sound proof that failed to verify.** `CertifyRunner.certifyCnf` reported `s NOT VERIFIED` on a
three-orbit certification obligation whose proof was perfectly good, and would do so again on any
sufficiently small instance.

### Fixed

- `CertifyRunner.certifyCnf` / `certifyCnfIO` retry with a text proof when the binary one fails to verify.
  kissat writes binary DRAT by default and drat-trim decides the format by sniffing the opening bytes rather
  than being told. Every proof starts with `0x64`, the deletion marker — which is also the ASCII `d` that
  opens a text deletion line — so the format hinges on the byte after it, the first literal's variable-length
  encoding. Large instances encode literals above `0x80` and sniff as binary; a small one can encode a
  literal into printable ASCII (`0x2d`, a `-`, on the three-orbit C = 4 obligation), and the binary file is
  then parsed as text, giving `ERROR: no conflict` on a sound refutation. The fallback re-emits the proof
  with `--no-binary` and re-checks it, only on that second pass — forcing text everywhere would roughly
  double proofs that already run to gigabytes in the mid-window.

  The misdetection **fails closed**: it reports NOT VERIFIED, never a false VERIFIED. So no verdict this
  library has ever recorded is in doubt, and the fallback can only turn a false negative into the right
  answer — drat-trim re-checks the freshly written text proof from scratch, so an unsound proof is never
  rescued.

Source- and binary-compatible with 0.8.0.

## [0.8.0] — 2026-08-31

**The minimal-uniformity walk layer and the exact plane engine.** Everything a verification repository needs
to re-derive the minimal-uniformity results now lives here: the three-orbit-and-beyond walk with its
curvature filters and its sharding, and the exact `ℤ[ζ_N]` plane engine that develops a symbol, splits tiles
and reads a symbol back out. Before this release those pieces sat in the development monorepo, so the claim
specs that use them could not be archived at all.

### Added

- **The exact plane layer** (`core`), floating point nowhere in it: `CycloRing` (the ring `ℤ[ζ_N]`, exact
  zero tests, signs decided algebraically at zero and otherwise only outside a certified error bound),
  `ExactPlane` (`UnitPolygon` as a cyclic direction word, with the embedded-polygon certificate that catches
  proper crossings a vertex-touch test misses), `ExactDeveloper` (development at an angle point, closure and
  holonomy re-checked per tile), `Defusion` (splitting a regular polygon off an irregular tile as word
  surgery, with pinch decomposition), `TilePatch` (patch state, class-wide moves, exhaustion to a saturated
  endpoint, `shapeKey`), `Periodicity` (translation lattices accepted only against the exact cell-area
  identity) and `SymbolExtractor` (geometry back to a minimal symbol).
- `DelaneySymbols`: two further curvature filters beside `tier1Feasible`, nesting as
  `euclid ⊆ staircase ⊆ tier-1` — `euclideanFeasibleExact` (the euclidean-feasible slice itself, the sharpest
  sound cut) and `staircaseFeasible` (the sharpest a SAT encoding can express: per chamber the bin floor of
  its exact tile-deficit rate). Each comes with its matching MONOTONE partial-D-set prune, so the walk is cut
  in the tree rather than at the leaves.
- `DelaneySymbols`: the orbit-bounded walk grows the species-level prunes `vertexCap` (a cap on the chambers
  of every (1,2)-orbit, open ones included) and `threeLetterShapesOk`, a `valence2` mode across the walk, the
  symbol enumeration and the curvature tables (vertex floors relax to `⌈2/r⌉`; `Orbit.minV2`), and
  deterministic canonical-prefix SHARDING — `orbitBoundedFrontier` and `orbitBoundedShardWalk`, whose shard
  indices are stable across runs and machines, so a walk of days is resumable and its shards checkpoint
  independently. `BackTracker.parallelForeachFrom` walks a shard's subtree on the same work-stealing pool.
- `UClass`: the two sharper readings of the around-every-vertex condition — `isArc` / `strictArcLegal` (the
  regular corners form one contiguous run, and that run is an arc of z, not a rotation of one) and
  `isolatedLegal` (the arc clause plus at most one irregular tile per vertex). `designations` and
  `candidates` take them as flags. The readings part exactly on species with a repeated letter.
- `MetricLayer`: the rigid-designation solver — `designatedRows` (the rows a regular/irregular designation
  forces linearly) and `particularSolution` (exact RREF, free variables at zero).
- `KCertify` (`solver`): `K2Certify` generalized from two vertex-orbit classes to k, with the staircase layer
  as an optional refinement of the tier-1 curvature side. Same `SatSolver` abstraction as the rest of the
  harness, so it runs on SAT4J and on CaDiCaL alike.
- The walk entry points, the curvature filters and `collectOrbits` are public rather than
  `private[research_core]`: the specs that consume them live in another repository.

### Changed

- `tier1Feasible`, `euclideanFeasible` and the walk entry points take a `valence2: Boolean = false`
  parameter. Source-compatible for ordinary calls; a bare method reference passed where a function is
  expected (`xs.filterNot(DelaneySymbols.tier1Feasible)`) no longer eta-expands and needs
  `tier1Feasible(_)`.
- `Periodicity.partitionBy` widened from `private[research_core]` to public.
- The scaladoc of the sunk modules no longer points at the development repository's notes and probes.

Otherwise source- and binary-compatible with 0.7.3 for library users: every other change is an addition or a
widening of visibility.

## [0.7.3] — 2026-08-30

**Irregular faces, lighter; the 15- and 16-gons coloured.** `render.FaceShape.isRegular` tells a regular face from an irregular one by
its boundary alone (equal turns at every corner, on unit edges); `render.Palette.rgbTinted` /
`fillTinted` push a size's colour towards white; `PdfFigure.toPdf` and `SvgFigure.toSvg` take a `tint`
parameter (default `0`, so every published panel regenerates byte-identical) that lightens the irregular
faces by that amount, `FigurePolicy.IrregularTint = 0.45` being the value the minimal-uniformity figures
use. The occasion: a star dodecagon among regular dodecagons, and a fused hexagon among regular ones,
were indistinguishable in the published panels; the fix keeps the hue — the lighter tile still says what
size it is fused from — and marks the shape. The 15-gon joins the canonical palette (`15 -> "#3f5f8a"`,
dark denim): in the (3.10.15) figure it is not the biggest polygon, so it could not stay default grey once
the irregular 36-gon beside it became a lighter grey. The 16-gon too (`16 -> "#3a7d44"`, forest green):
no regular 16-gon is drawn anywhere, but the (4.5.20) figure's reflex 16-gons are irregular tiles whose
lightened tone needed to separate from the orange squares, rose pentagons and grey 20-gons.

## [0.7.2] — 2026-08-19

**The published palette release.** The 0.7.1 tag's pipeline failed on a stale test fixture:
`RenderSpec`'s fallback-to-grey probe used the 7-gon as its "unknown size", which 0.7.1 itself had just
made a known colour. 0.7.2 is 0.7.1 plus that test fix — the fallback probe is now the 42-gon (a giant
that stays deliberately unmapped) and the components round-trip test covers all nine fills. No library
code changes; 0.7.1 was never published, so 0.7.2 is the artifact carrying the palette below.

## [0.7.1] — 2026-08-19

**Pentagons get a canonical fill.** `render.Palette` gains `5 -> "#c9366f"` (deep rose). The palette's own
revisit clause fired: the minimal-uniformity figures for (5².10) and (4.5.20) pair pentagons with 10- and
20-gons, and all three sizes fell to the default grey — indistinguishable by fill between convex near-round
shapes. The large gons stay default grey (each is the unique biggest polygon of its figure). Greyscale
separation: 5 vs default grey ΔL* ≈ 35, 5 vs 4 ≈ 14. The 7-, 9- and 10-gons join in the same release —
teal `#2a9d8f`, violet-blue `#5e60ce` and ochre `#bc6c25` — so every regular size the minimal-uniformity
species use below the giant gons now has a canonical fill; the giants (15, 18, 20, 24, 42) stay default
grey, each being the unique biggest polygon of its figure. Figures without pentagons, 7-, 9- or 10-gons
regenerate byte-identical.

The release also carries the post-0.7.0 housekeeping — no further change to the `core` or `solver` public
surface, and no behaviour change: the entries below touch compiler flags, one test import and comment prose.

### Changed

- **The ADR citations are gone from the code.** The ADRs live in `uniform-tilings`, a private repository,
  so every `ADR-0009`/`ADR-0022`/`ADR-0041` in this library's scaladoc was a pointer a reader cannot
  follow — 61 of them across 19 files, most in the published API documentation. The surrounding prose was
  already carrying the explanation, so in nearly every case the citation was the one part of the sentence
  adding nothing; where the ADR number *was* the subject, the description is promoted to lead. The gate,
  track, phase and stage labels (`G1`–`G4`, tracks `A`/`A2`/`B`/`C`/`D`, `D2`/`D4`, `Phase 1`/`2`,
  `Stage 1`) are kept: they are this codebase's own vocabulary, cross-referenced between files, and mean
  the same thing without a document number attached.
- **`-Wvalue-discard` and `-Wnonunit-statement` are off in `Test`**, and stay FATAL in `Compile`.
  ScalaTest's matchers return an `Assertion`, so every assertion that is not the last statement of its test
  tripped them: 148 warnings across 13 spec files, 130 of which predated 0.7.0. The build already conceded
  the point by dropping `-Werror` in `Test`; this finishes that decision rather than half-making it, and
  takes the run to zero warnings on JVM and Native alike. `-Wunused:imports` stays on in both scopes.

### Fixed

- **`RenderSpec` imported `FigureCanvas.Figure` without using it** — caught by CI on 0.7.0, which reports
  its warning count.

## [0.7.0] — 2026-08-13

**The figure renderers are consolidated.** Rendering machinery for the research programme's paper figures was
duplicated across two repositories and *six* emitters, carrying three unrelated line-width policies. The
format-level parts now live in one new package, `research_core.render`, and the stroke is a single shared
constant.

Why it was worth doing: on 2026-08-11 a set of paper figures was found to violate a journal artwork guide
requiring printed line weights of 0.35–1.5 pt — they printed at ~0.31 pt. The one-line fix had to be applied
in two repositories and still missed four further emitters. That cost is now paid once.

**No rendered geometry changed.** The 20 published panels and the 14 artwork files regenerate
**byte-identical** — verified by re-running their probes against the committed files.

### Added

- **`research_core.render`** — the format layer, listed in the README. Every emitter takes developed faces
  rather than a symbol, so a developer typed on another repository's `DSymbol` reaches all of them, and every
  one is IO-free as the rest of `core` is.
  - **`FigurePolicy`** — the one stroke policy: `StrokeUnits` (0.05 tiling units), `PtPerUnit` (22.0),
    `StrokePt` (0.8, for artwork emitted at final size), the printed band `MinPt`/`MaxPt`, and
    `printedPt`/`inBand` so a placement can be *checked* rather than commented.
  - **`PdfFigure.toPdf`** — moved here from `uniform-tilings`, and **refactored to return a `String`**: it
    called `java.nio.file.Files.writeString`, which this module's Native cross-build could not carry.
  - **`FigureCanvas`** — moved here from `uniform-tilings`; a page-description IR with PDF **and** EPS
    backends, so a figure is described once and emitted in both. The design the other emitters are special
    cases of, and where new ones should start. Its `write` stayed behind as caller-side IO.
  - **`PdfDocument`** — the hand-written PDF 1.4 container, extracted from the four copies of the same
    object/xref/trailer boilerplate that had accumulated.
  - **`SvgFigure`**, **`Palette`**, **`Pt`** — the SVG emitter, the face-fill colours (`fillOf` was
    `private[research_core]`, now public because the composites need it) and the shared point type.
- **`RenderSpec`** — the format layer tested as a format: the xref offsets point at the objects they claim,
  both backends describe the same drawing, the EPS stays pure ASCII — and, the point of the package, **the
  printed line-weight band is an assertion over `FigurePolicy`** at both documented placements, with the two
  widths that prompted all this asserted out of band.

### Changed

- **`SymbolRenderer` keeps the development and hands off the format.** `toSvg` now forwards to
  `render.SvgFigure`; `develop`, `apothem`, `circumradius` and `reflect` are unchanged. `Pt` is an alias for
  `render.Pt` — the same type, since named tuples are structural — so callers see no change.
- **`PdfDocument` formats its xref offsets with `Locale.ROOT`.** They were built with the `f` interpolator,
  which formats in the *default* locale; identical bytes under every Latin-digit locale, so nothing deposited
  moves, but the table can no longer be written in digits a reader cannot parse.

### Notes for `uniform-tilings`

Its `SymbolRenderer` fork is **not** deleted outright, because `develop` there is typed on that repository's
own `DelaneySymbols.DSymbol` — a genuinely divergent engine carrying the rung-4a walk machinery this one does
not have. The dividing line held instead: format-level machinery moved, domain projection stayed with its
types. What remains there is `develop` plus `export` clauses re-exporting this package under the names its
call sites already used.

## [0.6.1] — 2026-08-06

Hardening plus one additive surface change: the 3D substrate's numeric helper layer goes public (below),
the test suite grows from 104 to 130 JVM tests, and the build gains dcel's formatting and linting guard
rails.

### Added

- **`PlatformSolverSpec`** (shared, replacing the native-only `CadicalSolverSpec`) — the [`SatSolver`]
  contract against the platform's live solver on BOTH platforms; SAT4J's previously untested
  `Timeout` translation now runs on the JVM.
- **`K1CertifySpec`** — the K1 encoding vs the single-orbit tier-free universe generator, op for op at
  C ≤ 8, plus non-vacuous exclusion of the strictly-2-orbit slice (`K1Certify` had zero in-repo
  coverage).
- **`CertificationSpec`** — direct units for the certification leaves, previously reachable only through
  the tools-guarded e2e path: sink contracts (incl. `CountingSink.sawEmptyClause`), the DIMACS
  emit/assemble/parse round-trip, both `violatedClauses` overloads, and golden pins for the frame-key
  format and 16-hex-char hash.
- **`MetricLayerSpec`** — the metric layer the verification repositories stand on, tested through its
  documented internal identities on the 11 oracle minimal symbols (regular point solves the system and
  closes every face, numeric ≡ exact ℚ(ζ₂₄) closure track, rank–nullity, tangent basis of moduli
  dimension, vacuous exact-symmetry realizability of minimal symbols).
- **`QuotientCertifySpec`** — the track-C obligation solved LIVE through the platform `SatSolver`
  (no external kissat): `generators` empty and the empty-list obligation UNSAT on every minimal oracle
  symbol — the minimality certificate, tool-free and cross-platform.
- **`HoneycombGateSmokeSpec`** (JVM-only) — the full completeness audit (26 species, 28 classes, zero
  flags) as an in-repo guard for `CompletenessAudit`/`TransitivePatterns`/`MonoShell`, plus
  `SymbolRealization` deriving its self-checked minimal symbols with the audit's class counts.
- **`Sha256VectorsSpec`** (shared) — the NIST vectors now run on Scala Native too; the unbroken
  pure-refutation tier of `CertifyRunner` is now positively exercised (3.3.6.6 frames: 0 models,
  `Some(true)` on both unbroken verdicts — previously vacuously passable).

### Changed

- **The 3D substrate's helper layer widened from `private[research_core]` to public**, so a downstream
  repository can build its own pair/k-set/realization machinery on the same primitives instead of
  duplicating them: `MonoShell`'s vector algebra (`add`, `sub`, `neg`, `scale`, `dot`, `cross`, `norm`,
  `unit`, `dist`), its tolerances (`matchTol`, `grayTol`) and star-ring probes (`ringDescriptors`,
  `descriptorsMatch`, `compatible`); `TransitivePatterns`' development kit (`idMat`, `round4`,
  `cornerData`, `starSig`, `inStab`, `walkFace` — previously fully private — and `searchPatterns`);
  and `CompletenessAudit`'s lattice recognizers (`latticeBasis`, `inLattice`). The `debug*` hooks stay
  package-private.
- **Formatting and linting guard rails, ported from `dcel`**: `.scalafmt.conf` (scalafmt 3.11.2 — the
  build had claimed `scalafmtOnCompile` with no config, so the formatter had never actually run; the
  whole tree is now formatted) and `.scalafix.conf` (`DisableSyntax`: `null`, `asInstanceOf`,
  `isInstanceOf`, XML banned — `noWhile` deliberately OFF for this codebase's hot-loop style —
  plus `LeakingImplicitClassVal`), with sbt-scalafix/semanticdb wiring, a `qa` alias, and CI running
  `scalafmtCheckAll` + `scalafixAll --check` before the tests.
- **Compiler hygiene**: `-feature -unchecked -Wvalue-discard -Wnonunit-statement -Wunused:imports`,
  fatal (`-Werror`) in main sources and non-fatal in tests; ~35 discarded non-Unit values now carry
  explicit `: Unit` ascriptions, six unused imports removed, and the legitimate `null`/cast sites carry
  scoped scalafix suppressions with reasons.
- **`CertifyRunner` brackets its DIMACS sinks** (`scala.util.Using`): no leaked file handles or
  unflushed writers when the enumeration throws (e.g. `SatSolver.Timeout`).
- Property checks run 100 cases (was 10); suites run serially (`Test / parallelExecution := false`) so
  core-saturating suites stop contaminating each other's timings; `K2CertifySpec`'s universe sink is
  actually synchronized; `CertifyRunnerSpec` cleans its temp trees; `BackTrackerCESpec` pins the
  maxSize-18 counts unconditionally; `CappedProbeSpec` also asserts zero false positives.

## [0.6.0] — 2026-08-05

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

  Downstream impact assessed: none realized. `31-unit-edge-tilings` (0.2.1) and
  `minimal-uniformity-three` (0.3.1) pin versions that predate `searchPatterns` entirely (it shipped in
  0.5.0) and use no honeycomb API. `convex-uniform-honeycombs` (0.5.0) is the sole consumer of the
  affected exhaustion certificate — re-running the full completeness audit with the truthful flag
  reproduces its every assertion (26 species, 28 classes, all skeletons closed, zero flags): every
  exhaustion search genuinely completed under the 500000 cap, so the pre-fix certificates were
  materially valid and the defect was latent on that path.

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

[Unreleased]: https://github.com/scala-tessella/research-core/compare/v0.8.1...HEAD
[0.8.1]: https://github.com/scala-tessella/research-core/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/scala-tessella/research-core/compare/v0.7.3...v0.8.0
[0.7.0]: https://github.com/scala-tessella/research-core/compare/v0.6.1...v0.7.0
[0.4.0]: https://github.com/scala-tessella/research-core/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/scala-tessella/research-core/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/scala-tessella/research-core/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/scala-tessella/research-core/compare/v0.2.0...v0.2.1
