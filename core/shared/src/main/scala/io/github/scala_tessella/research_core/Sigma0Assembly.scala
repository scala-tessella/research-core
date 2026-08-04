package io.github.scala_tessella.research_core

import StarFoldings.Folded

/** The σ₀ ASSEMBLY — one unknown involution joining k folded star complexes into a three-dimensional
  * Delaney–Dress symbol candidate, the transposition of the two-dimensional engine with one more internal
  * involution.
  * Chambers are the disjoint union of the orbits' folded complexes; σ₀ crosses the flag's edge to the
  * neighbor vertex, fixed points allowed (edge-midpoint mirrors). The conditions:
  *
  *   - MATCHING COMPATIBILITY: σ₀-paired chambers share their face and the cell across the edge, so the m₀₁
  *     (face size), m₂₃ (ring size) and cell-type data must agree;
  *   - COMMUTATION: (σ₀σ₂)² = (σ₀σ₃)² = id — crossing the edge commutes with changing the face within the
  *     cell and the cell across the face; this also transports the edge-ring correspondence step by step, so
  *     with the per-chamber equalities the two ends agree along the whole ring;
  *   - FACE CLOSURE: (σ₀σ₁)^m₀₁ = id — the composition's orbit length divides the face size (proper divisors
  *     are faces folded by symmetry);
  *   - CONNECTIVITY: one component under {σ₀, σ₁, σ₂, σ₃} (each star is internally connected, so this asks σ₀
  *     to join the k orbits).
  *
  * The enumerator is an exhaustive backtracker: σ₀ is propagated over the ⟨σ₂, σ₃⟩-orbit of every choice (the
  * commutation conditions make those assignments forced), and face cycles are checked as soon as they close.
  * No symmetry breaking and no caps unless requested — completeness is the point; canonical-key dedup,
  * minimality ([[SymbolCatalog]]) and geometric realization ([[SymbolRealization]]) are separate layers, as
  * is any SAT/DRAT industrialization the census sizes may demand. Deeper cell-corner compatibility across σ₀
  * is deliberately left to the rigid development.
  */
object Sigma0Assembly:

  /** The disjoint union of folded complexes: the assembly's chamber set with all decoration data. */
  final case class ChamberUnion(
      s1: Vector[Int],
      s2: Vector[Int],
      s3: Vector[Int],
      m01: Vector[Int],
      m23: Vector[Int],
      cell: Vector[Int],
      orbit: Vector[Int] // which folded part each chamber came from
  ):
    def size: Int = s1.size

  /** AUTOMORPHISMS of a k-part union for the deck-conjugation lex-leader: star symmetries that NORMALIZE the
    * folding subgroup descend to the folded complex; the union automorphisms are the block-diagonal products
    * (the k species are pairwise distinct, so no automorphism can exchange parts). Each returned perm
    * commutes with σ₁..σ₃ and preserves all decorations (asserted), so conjugation permutes the σ₀ solution
    * set — enumerating lex-minimal orbit representatives only is sound and complete up to isomorphism.
    */
  def unionAutosOf(parts: Vector[(StarFoldings.StarSymmetry, Set[StarFoldings.Perm], Folded)])
      : Vector[Vector[Int]] =
    def gInverse(g: StarFoldings.Perm): StarFoldings.Perm                                 =
      val inv = Array.fill(g.size)(0)
      for c <- g.indices do inv(g(c)) = c
      inv.toVector
    def descended(sym: StarFoldings.StarSymmetry, sub: Set[StarFoldings.Perm], f: Folded) =
      val reps = (0 until f.size).map(q => f.orbitOf.indexOf(q))
      sym.perms
        .filter(g => sub.forall(h => sub.contains(g.map(h).map(gInverse(g)))))
        .map(g => reps.map(c => f.orbitOf(g(c))).toVector)
        .distinct
    val off                                                                               =
      parts.map(_._3.size).scanLeft(0)(_ + _)
    parts
      .map((sym, sub, f) => descended(sym, sub, f))
      .zipWithIndex
      .foldLeft(Vector(Vector.empty[Int])) { case (acc, (da, i)) =>
        for a <- acc; d <- da yield a ++ d.map(_ + off(i))
      }

  /** The two-part form (the k = 2 call sites). */
  def unionAutosOf(
      symA: StarFoldings.StarSymmetry,
      subA: Set[StarFoldings.Perm],
      foldA: Folded,
      symB: StarFoldings.StarSymmetry,
      subB: Set[StarFoldings.Perm],
      foldB: Folded
  ): Vector[Vector[Int]] =
    unionAutosOf(Vector((symA, subA, foldA), (symB, subB, foldB)))

  def unionOf(parts: Vector[Folded]): ChamberUnion =
    val off = parts.scanLeft(0)(_ + _.size)
    ChamberUnion(
      parts.zipWithIndex.flatMap((p, i) => p.s1.map(_ + off(i))),
      parts.zipWithIndex.flatMap((p, i) => p.s2.map(_ + off(i))),
      parts.zipWithIndex.flatMap((p, i) => p.s3.map(_ + off(i))),
      parts.flatMap(_.m01),
      parts.flatMap(_.m23),
      parts.flatMap(_.cell),
      parts.zipWithIndex.flatMap((p, i) => Vector.fill(p.size)(i))
    )

  /** All σ₀ involutions satisfying the axioms on the union; `cap` for honest partial runs (capped flag
    * returned). Each solution is the full σ₀ as a permutation vector.
    *
    * Search order is MOST-CONSTRAINED-FIRST: branch on an unassigned chamber whose compatibility class (equal
    * m₀₁/m₂₃/cell — the exact partner condition) has the fewest unassigned members, over exactly those
    * members (self-pairing included). The variable order changes nothing about the solution SET — at every
    * node the branches partition the completions by σ₀(c) — but fail-first collapses the barely-folded search
    * trees that first-unassigned order left astronomically deep (the k = 2 sizing run: 1567/1568 assemblies
    * in 66 min, the trivial×trivial octet union alone past 939M nodes unfinished).
    */
  def enumerateSigma0(
      u: ChamberUnion,
      cap: Int = Int.MaxValue,
      log: String => Unit = _ => (),
      autos: Vector[Vector[Int]] =
        Vector.empty // union automorphisms: enumerate lex-min conjugation orbit reps
  ): (Vector[Vector[Int]], Boolean) =
    val n        = u.size
    val sigma    = Array.fill(n)(-1)
    val out      = Vector.newBuilder[Vector[Int]]
    var found    = 0
    var capped   = false
    var nodes    = 0L // heartbeat: a silent assembly is indistinguishable from a hang without it
    val autoInvs = autos.map { g =>
      val inv = Array.fill(n)(0)
      for c <- g.indices do inv(g(c)) = c
      inv
    }

    /** DECK-CONJUGATION LEX-LEADER: keep σ₀ only when it is
      * lexicographically ≤ every conjugate g·σ₀·g⁻¹. Partial version, sound and representative-complete: scan
      * positions in order over the contiguous both-defined prefix; the first strict difference decides; an
      * undefined side leaves the comparison undecided (no prune). The orbit's lex-min solution is never
      * pruned — its partial states agree with the full vector on defined positions, so every decided
      * comparison returns the full vector's verdict.
      */
    def lexLeaderOk(): Boolean =
      autos.indices.forall { gi =>
        val g       = autos(gi)
        val gInv    = autoInvs(gi)
        var c       = 0
        var verdict = true
        var decided = false
        while c < n && !decided do
          val a   = sigma(c)
          val pre = sigma(gInv(c))
          val b   = if pre < 0 then -1 else g(pre)
          if a < 0 || b < 0 then decided = true
          else if a != b then
            verdict = a < b
            decided = true
          else c += 1
        verdict
      }

    def compatible(c: Int, d: Int): Boolean =
      u.m01(c) == u.m01(d) && u.m23(c) == u.m23(d) && u.cell(c) == u.cell(d)

    // compatibility classes: partners of c are exactly the members of c's class
    val classIdx          = collection.mutable.Map.empty[(Int, Int, Int), Int]
    val classOf           = Array.tabulate(n) { c =>
      classIdx.getOrElseUpdate((u.m01(c), u.m23(c), u.cell(c)), classIdx.size)
    }
    val classMembers      = Vector.tabulate(classIdx.size)(k => (0 until n).filter(classOf(_) == k).toVector)
    val unassignedInClass = Array.tabulate(classIdx.size)(classMembers(_).size)

    /** Walk the (σ₀σ₁)-cycle from c through assigned chambers; false iff it CLOSES at a length not dividing
      * m₀₁ (an open walk constrains nothing yet).
      */
    def faceOk(c: Int): Boolean =
      var a     = c
      var steps = 0
      while true do
        val b = sigma(a)
        if b < 0 then return true
        a = u.s1(b)
        steps += 1
        if a == c then return u.m01(c) % steps == 0
        if steps > u.m01(c) then return false
      false

    def undoAll(done: List[Int]): Unit =
      done.foreach { x =>
        sigma(x) = -1
        unassignedInClass(classOf(x)) += 1
      }

    /** Assign σ₀(c0) = d0 and propagate over the ⟨σ₂, σ₃⟩-orbit (commutation forces those); returns the newly
      * assigned chambers for undo, or None on any conflict.
      */
    def assign(c0: Int, d0: Int): Option[List[Int]] =
      var todo            = List((c0, d0))
      var done: List[Int] = Nil
      var ok              = true
      while todo.nonEmpty && ok do
        val (c, d) = todo.head
        todo = todo.tail
        if sigma(c) == d && sigma(d) == c then ()
        else if sigma(c) != -1 || sigma(d) != -1 then ok = false
        else if !compatible(c, d) then ok = false
        else
          sigma(c) = d
          sigma(d) = c
          unassignedInClass(classOf(c)) -= 1
          if d != c then unassignedInClass(classOf(d)) -= 1
          done = if c == d then c :: done else c :: d :: done
          todo = (u.s2(c), u.s2(d)) :: (u.s3(c), u.s3(d)) :: todo
      if ok then
        // the face cycle through any newly assigned chamber must not close at a bad length
        if done.forall(faceOk) then Some(done)
        else { undoAll(done); None }
      else { undoAll(done); None }

    def connected(): Boolean =
      val seen  = collection.mutable.Set(0)
      var front = List(0)
      while front.nonEmpty do
        front = front.flatMap(c =>
          List(sigma(c), u.s1(c), u.s2(c), u.s3(c)).filter(x => x >= 0 && seen.add(x))
        )
      seen.size == n

    /** CONNECTIVITY LOOK-AHEAD: a component of the current graph (σ₁..σ₃ edges plus assigned σ₀ edges) whose
      * chambers ALL have σ₀ assigned is final — future assignments pair two unassigned chambers, so nothing
      * can attach to it. If such a closed component is proper, no completion is connected; refuting it here
      * avoids enumerating the full product of the parts' internal solutions before the final connectivity
      * check rejects them all (the σ₀-complete sub-symbols the barely-folded assemblies keep re-deriving).
      */
    def deadClosedComponent(): Boolean =
      val seen = Array.fill(n)(false)
      var c0   = 0
      var dead = false
      while c0 < n && !dead do
        if !seen(c0) then
          var open  = false
          var size  = 0
          var front = List(c0)
          seen(c0) = true
          while front.nonEmpty do
            val x = front.head
            front = front.tail
            size += 1
            if sigma(x) < 0 then open = true
            for y <- List(u.s1(x), u.s2(x), u.s3(x), sigma(x)) do
              if y >= 0 && !seen(y) then
                seen(y) = true
                front = y :: front
          dead = !open && size < n
        c0 += 1
      dead

    def bt(): Unit =
      if found >= cap then { capped = true; return }
      nodes += 1
      if nodes % 1000000 == 0 then log(s"    ... sigma0 search: ${nodes / 1000}k nodes, $found solutions")
      if deadClosedComponent() then return
      if autos.nonEmpty && !lexLeaderOk() then return
      // the most-constrained unassigned chamber: fewest unassigned members in its compatibility class
      var c    = -1
      var best = Int.MaxValue
      var i    = 0
      while i < n do
        if sigma(i) == -1 && unassignedInClass(classOf(i)) < best then
          best = unassignedInClass(classOf(i))
          c = i
        i += 1
      if c == -1 then
        if connected() then
          found += 1
          out += sigma.toVector
      else
        for d <- classMembers(classOf(c)) if sigma(d) == -1 do
          assign(c, d) match
            case Some(done) =>
              bt()
              undoAll(done)
              if found >= cap then { capped = true; return }
            case None       => ()

    bt()
    (out.result(), capped)
