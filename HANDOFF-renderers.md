# Handoff: consolidate the figure renderers into research-core

*Written 2026-08-11. Disposable — delete it once the work is done.*

Paste the section below into a session started in this folder
(`~/IdeaProjects/scala-tessella/research-core`). It is written to be self-contained: assume
the reader knows nothing about how it came up.

---

## The task

Rendering machinery for the research programme's paper figures is duplicated across two repos
and four emitters, with three unrelated line-width policies. Consolidate the **format-level**
parts into a new `io.github.scala_tessella.research_core.render` package, leave domain
projection logic where its types live, and make the stroke policy a single shared constant.

## Why this is worth doing

On 2026-08-11 the figures of the paper *The 31 types of vertex-transitive tilings of the plane
by convex unit-edge polygons* were found to violate the IUCr Artwork guide, which requires
printed line weights of **0.35–1.5 pt**. The strokes printed at ~0.31 pt. The one-line fix had
to be applied in **two** repos, and still missed a third and fourth emitter. That is the cost
of the duplication, already paid once.

## Current state (verified 2026-08-11)

Four renderers, three stroke policies:

| File | Lines | Emits | Stroke |
|---|---|---|---|
| `research-core` `core/shared/.../research_core/SymbolRenderer.scala` | 120 | SVG | `stroke-width="0.05"` |
| `uniform-tilings` `core/.../uniform_tilings/SymbolRenderer.scala` | 118 | SVG | `stroke-width="0.05"` |
| `uniform-tilings` `core/.../uniform_tilings/PdfFigure.scala` | 68 | PDF | `StrokeUnits = 0.05`, `PtPerUnit = 22.0` |
| `uniform-tilings` `core/.../uniform_tilings/HoneycombRenderer.scala` | 203 | SVG + **its own inline PDF writer** + OBJ + JS | `0.012` and `0.012 * s` |
| `uniform-tilings` `core/.../uniform_tilings/FigureCanvas.scala` | ~190 | **PDF + EPS**, from one shape IR | `strokePt`, in printed points |
| `uniform-tilings` `core/.../uniform_tilings/CompositeFigure.scala` | ~105 | layout only (grids, line art) | `StrokePt = 0.8` |

**Added 2026-08-12, after this handoff was written**, so the count is now *five* emitters —
which sharpens the case rather than weakening it. `FigureCanvas` was written because Acta
Cryst refuses PDF artwork and the alternative was a Ghostscript dependency; it is the first
emitter here to (a) describe a drawing once and render it in two formats, (b) express stroke
width in **printed points** rather than in domain units scaled by an unknown placement factor,
and (c) carry text. **Treat it as the design the consolidation should generalize**, not as one
more thing to fold in: `PdfFigure` is a special case of it, and `HoneycombRenderer`'s inline
PDF writer is another. Its PDF output is deliberately *not* byte-compatible with `PdfFigure`'s
(different content, and it emits a `/Font` resource), so folding `PdfFigure` into it must be
checked against the byte-identity requirement below.

- The two `SymbolRenderer`s are a fork: they differ only in package name, a doc paragraph, one
  `: Unit` ascription, a visibility qualifier — and nothing else. **This repo's copy is the more
  advanced** (it has the extra `toSvg(ds, banner, radius)` overload and the IO-free contract).
- `uniform-tilings` **already depends on `research-core` 0.6.1**, so deleting the fork is an
  import change, not a port.
- `PdfFigure` is a hand-written PDF 1.4 emitter — deliberately no toolchain dependency. It is
  the format the papers actually `\includegraphics`.
- Ten files in `uniform-tilings` reference `SymbolRenderer` (probes, specs, `DeformedRenderer`,
  `PdfFigure`).

## What to do

1. **Create `research_core.render`** as the home for format-level machinery, with a single
   figure-policy object holding the stroke constant (currently `0.05` tiling units) and the
   `PtPerUnit` scale. One place, so the next journal rule lands once.
2. **Move `PdfFigure` here — but refactor it first.** It currently calls
   `java.nio.file.Files.writeString`. This repo's `core` is a `crossProject(JVMPlatform,
   NativePlatform)` and is documented *"IO-free by design (the `core` module is
   Scala.js-clean)"*. So it must **return the document as a `String`**, exactly as `toSvg` does,
   and leave writing to the caller. Moving it unchanged would break that contract.
3. **Delete `uniform_tilings.SymbolRenderer`** and repoint its ten call sites at this repo's copy.
4. **Leave `HoneycombRenderer` in `uniform-tilings`.** It imports `HoneycombGeometry.*` from
   that repo; moving it means moving the geometry too. Instead have it consume the shared
   stroke policy and the PDF helper. The dividing line to hold: *format-level machinery moves,
   domain projection logic stays with its types.*
5. **Bump to 0.7.0** and update `CHANGELOG.md`. Papers pin their own versions (the 31 paper
   pins 0.2.1), so archived releases are unaffected — do not attempt to revise them.

## Constraints — read before changing anything

- **Do not change rendered geometry.** The figures are published (Zenodo
  10.5281/zenodo.21873687) and one paper is under submission. Only stroke width was ever meant
  to change.
- **Do not "tidy" the stroke back to 0.03.** `0.05` is set by the IUCr rule, and the arithmetic
  is in `PdfFigure`'s scaladoc: a stroke prints at
  `StrokeUnits × PtPerUnit × (placed width ÷ intrinsic width)`. A panel is ~447 pt wide
  intrinsically and is placed at 75 mm, so 0.03 gave 0.31 pt — under the floor.
- **The 31 paper is mid-submission.** If regenerated output changes at all, the submission set
  at `papers-publication/31-unit-edge-tilings/iucr-version/figures/` must be refreshed and the
  composites rebuilt. Prefer landing this work *after* that submission.

## How to verify

The figure generator is deterministic; use that.

```
cd ../uniform-tilings
sbt "core/testOnly *FamilyFiguresProbe"        # writes paper/figures/, ~14 s
# every regenerated PDF must be byte-identical to its counterpart:
for f in paper/figures/*.pdf; do diff -q "$f" "papers/uniform-tilings/figures/$(basename $f)"; done
```

Acceptance:

- [ ] `sbt compile` green here (JVM **and** Native — the Native target is what catches an
      accidental IO or JVM-only dependency)
- [ ] `uniform-tilings` compiles against the new version with the fork deleted
- [ ] `FamilyFiguresProbe` output **byte-identical** to the current figures
- [ ] no `stroke-width="0.0…"` or bare stroke literal survives outside the policy constant
      (`grep -rn 'stroke-width=\|0\.012\|StrokeUnits' --include='*.scala'` across both repos)
- [ ] `CHANGELOG.md` updated

## One open question for whoever does this

`HoneycombRenderer` uses `0.012` (SVG) and `0.012 * s` (its PDF path) — a different scale
system, so whether it satisfies 0.35–1.5 pt printed depends on `s` and on final placement.
**Measure it** and fold the result into the shared policy. It serves the honeycombs paper
(*The 28 convex uniform honeycombs*), bound for Discrete & Computational Geometry, which
states no line-weight rule — but hairlines print badly everywhere.

## Where the background lives

`papers-publication/31-unit-edge-tilings/08 Acta Cryst A submission.md` — the submission gates,
the IUCr artwork requirements, and the reasoning behind each decision above.
`…/07 Timeline.md` — the event log, entries of 2026-08-11.
