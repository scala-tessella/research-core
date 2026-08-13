package io.github.scala_tessella.research_core.render

/** The hand-written PDF 1.4 container every PDF emitter here shares: object bodies in, a complete one-page
  * document out, with the cross-reference table's byte offsets computed as the objects are laid down.
  *
  * DELIBERATELY NO TOOLCHAIN DEPENDENCY. These figures are what the papers `\includegraphics`, and a PDF
  * writer for path-and-text content is a hundred lines; a library would be a build dependency, a licence and
  * a version to pin for no gain. The three emitters that used to carry their own copy of the boilerplate
  * ([[PdfFigure]], [[FigureCanvas]] and `uniform_tilings.HoneycombRenderer`) now call this.
  *
  * IO-free, as the whole `core` module is: [[apply]] returns the document as a string and writing it is the
  * caller's business.
  */
object PdfDocument:

  private def num(v: Double): String = String.format(java.util.Locale.ROOT, "%.2f", v)

  /** A content-stream object wrapping `stream`. Always object 4 in the documents built here. */
  def contents(stream: String): String =
    s"<< /Length ${stream.length} >>\nstream\n$stream\nendstream"

  /** A one-page object of the given size in points. `resources` is the complete resource dictionary including
    * its delimiters — `"<< >>"` for pure path content, `"<< /Font << … >> >>"` when there is text.
    *
    * The (possibly empty) `/Resources` dict is REQUIRED by the spec even for pure path content: without it,
    * pdfTeX warns "/Resources missing" on every `\includegraphics` of these figures.
    */
  def page(widthPt: Double, heightPt: Double, resources: String): String =
    s"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ${num(widthPt)} ${num(heightPt)}] " +
      s"/Resources $resources /Contents 4 0 R >>"

  /** The catalog and page-tree objects that open every document here — objects 1 and 2, so a caller's own
    * objects start at 3 (the page) and 4 (the contents).
    */
  val preamble: Vector[String] = Vector(
    "<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
  )

  /** Lay `objs` down as objects `1 0 obj` … `n 0 obj`, then the xref table and trailer. */
  def apply(objs: Vector[String]): String =
    val sb      = StringBuilder("%PDF-1.4\n")
    val offsets = Vector.newBuilder[Int]
    for (o, i) <- objs.zipWithIndex do
      offsets += sb.length
      sb ++= s"${i + 1} 0 obj\n$o\nendobj\n"
    val xrefAt  = sb.length
    sb ++= s"xref\n0 ${objs.size + 1}\n0000000000 65535 f \n"
    // Locale.ROOT, not f"$off%010d": the f-interpolator formats in the DEFAULT locale, and a locale with a
    // non-ASCII numbering system would write an xref table no reader can parse. Identical bytes for every
    // locale that uses Latin digits, which is why the deposited figures are unaffected.
    // (a literal "\n", never "%n" — that is the PLATFORM separator, and CRLF here breaks the offsets)
    for off <- offsets.result() do sb ++= String.format(java.util.Locale.ROOT, "%010d 00000 n \n", off)
    sb ++= s"trailer\n<< /Size ${objs.size + 1} /Root 1 0 R >>\nstartxref\n$xrefAt\n%%EOF\n"
    sb.toString
