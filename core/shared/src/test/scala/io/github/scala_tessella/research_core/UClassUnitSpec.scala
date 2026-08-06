package io.github.scala_tessella.research_core

import io.github.scala_tessella.research_core.DelaneySymbols.{DSymbol, Tiling}
import io.github.scala_tessella.research_core.Signatures.VertexSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[UClass]]: the pure cyclic-subword combinatorics in isolation, the target data, and a
  * structural invariant of the designation/candidate methods on an enumerated symbol. (The paper-specific
  * minimal-uniformity verdicts live in the minimal-uniformity verification repo.)
  */
class UClassUnitSpec extends AnyFlatSpec with Matchers:

  behavior of "UClass.cyclicSubset (contiguous cyclic subword, up to rotation and reflection)"

  it should "accept a contiguous run and reject a split one (the documented fixture)" in:
    UClass.cyclicSubset(List(3, 3), List(3, 3, 6, 6)) shouldBe true
    UClass.cyclicSubset(List(3, 3), List(3, 6, 3, 6)) shouldBe false

  it should "respect rotation and cyclic wrap-around" in:
    UClass.cyclicSubset(List(6, 6), List(3, 3, 6, 6)) shouldBe true
    UClass.cyclicSubset(List(6, 3), List(3, 3, 6, 6)) shouldBe true // wraps the boundary

  it should "respect reflection and full-length match" in:
    UClass.cyclicSubset(List(4, 3), List(3, 4, 5)) shouldBe true // reversed
    UClass.cyclicSubset(List(3, 4, 5), List(3, 4, 5)) shouldBe true

  it should "reject a word longer than the type" in:
    UClass.cyclicSubset(List(3, 3, 3, 3, 3), List(3, 3, 6, 6)) shouldBe false

  behavior of "UClass.targets"

  it should "list the ten non-Archimedean figures with their claimed minima" in:
    UClass.targets should have size 10
    UClass.targets.toMap.apply(List(3, 8, 24)) shouldBe 3
    UClass.targets.toMap.apply(List(3, 4, 3, 12)) shouldBe 3

  behavior of "UClass designations / candidates on an enumerated symbol"

  it should "keep candidates a subset of designations (candidates filter designations)" in:
    val relaxed: List[(Tiling, DSymbol)] = DelaneySymbols.enumerateRelaxedDetailed(maxN = 1, maxSize = 12)
    relaxed should not be empty
    val (t, ds)                          = relaxed.head
    val z: VertexSignature               = t.vertices.head
    UClass.candidates(ds, z).toSet.subsetOf(UClass.designations(ds, z).toSet) shouldBe true
    UClass.noneForcedRegular(ds, Set.empty, Seq.empty) shouldBe true // empty irregular short-circuits
