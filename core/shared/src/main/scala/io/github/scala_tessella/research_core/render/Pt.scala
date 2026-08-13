package io.github.scala_tessella.research_core.render

/** A point of a figure, in whatever coordinate system the emitter around it works in — tiling units for the
  * developers ([[io.github.scala_tessella.research_core.SymbolRenderer.develop]]), printed points for
  * [[FigureCanvas]].
  *
  * NAMED, unlike the hot geometric tuples of this codebase (`MonoShell.Vec`, `CertifiedDihedrals.V3`), which
  * are positional because named access routes through NamedTuple glue that Scala Native debug builds cannot
  * optimize away. Rendering is cold — it runs once per figure — so `.x`/`.y` is free here, and the emitters
  * index these constantly.
  *
  * Because named tuples are structural, an alias written in another object — `type Pt = render.Pt` — denotes
  * THIS type, which is what lets a developed figure cross a repository boundary without a conversion.
  */
type Pt = (x: Double, y: Double)
