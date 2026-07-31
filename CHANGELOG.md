# Changelog

All notable changes to `research-core` are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). The `core` public surface
listed in the README is the compatibility contract.

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

[0.3.0]: https://github.com/scala-tessella/research-core/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/scala-tessella/research-core/compare/v0.2.0...v0.2.1
