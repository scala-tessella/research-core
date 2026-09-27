package io.github.scala_tessella.research_core

import Sigma0Assembly.ChamberUnion

/** The ⟨σ₂,σ₃⟩-orbits of a chamber union (each an edge ring's chambers) and their decoration-preserving
  * equivariant intertwiners — the propagation blocks of the σ₀ assembly and the atoms of its
  * orbit-connectivity prune.
  */
object ChamberRings:

  /** The ⟨σ₂,σ₃⟩-orbits of the union (the forced-propagation blocks: each is one edge-ring's chambers). */
  def orbitsOf(u: ChamberUnion): Vector[Vector[Int]] =
    val seen = Array.fill(u.size)(false)
    val out  = Vector.newBuilder[Vector[Int]]
    for c <- 0 until u.size if !seen(c) do
      var front = List(c)
      val acc   = Vector.newBuilder[Int]
      seen(c) = true
      while front.nonEmpty do
        val x = front.head
        front = front.tail
        acc += x
        for y <- List(u.s2(x), u.s3(x)) if !seen(y) do
          seen(y) = true
          front = y :: front
      out += acc.result()
    out.result()

  /** All decoration-preserving ⟨σ₂,σ₃⟩-intertwiners oA → oB as chamber maps. Equivariance forces the whole
    * map from one seed image, so each candidate is a propagation from a decoration-matching seed.
    */
  def intertwinersOf(u: ChamberUnion, oA: Vector[Int], oB: Vector[Int]): Vector[Map[Int, Int]] =
    if oA.size != oB.size then Vector.empty
    else
      val c0 = oA.head
      oB.flatMap { d0 =>
        val phi  = collection.mutable.Map.empty[Int, Int]
        var todo = List((c0, d0))
        var ok   = true
        while todo.nonEmpty && ok do
          val (x, y) = todo.head
          todo = todo.tail
          phi.get(x) match
            case Some(y0) => ok = y0 == y
            case None     =>
              if u.m01(x) != u.m01(y) || u.m23(x) != u.m23(y) || u.cell(x) != u.cell(y) then ok = false
              else
                phi(x) = y
                todo = (u.s2(x), u.s2(y)) :: (u.s3(x), u.s3(y)) :: todo
        if ok && phi.size == oA.size && phi.values.toSet.size == oA.size then Some(phi.toMap) else None
      }

  def intertwines(u: ChamberUnion, oA: Vector[Int], oB: Vector[Int]): Boolean =
    intertwinersOf(u, oA, oB).nonEmpty
