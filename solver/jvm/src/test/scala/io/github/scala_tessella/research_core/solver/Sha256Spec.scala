package io.github.scala_tessella.research_core.solver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The pure-Scala [[Sha256]] property-checked against the JVM's `MessageDigest` — the cross-platform
  * frame-key hashes must be bit-identical to what the harness produced before (artifact directory names
  * are part of the recorded manifests). The platform-neutral NIST vectors live in the SHARED
  * `Sha256VectorsSpec`, so Scala Native verifies the digest too.
  */
class Sha256Spec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  "Sha256.digest" should "agree with java.security.MessageDigest on arbitrary byte arrays" in:
    forAll { (bytes: Array[Byte]) =>
      Sha256.digest(bytes) shouldBe java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
    }

  it should "agree with MessageDigest across the block-boundary lengths (55..73, 119..130)" in:
    for n <- (55 to 73) ++ (119 to 130) do
      val bytes = Array.tabulate[Byte](n)(i => (i * 31 + 7).toByte)
      Sha256.digest(bytes) shouldBe java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
