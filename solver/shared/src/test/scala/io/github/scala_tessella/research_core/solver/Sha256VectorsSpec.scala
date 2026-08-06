package io.github.scala_tessella.research_core.solver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The FIPS 180-4 vectors for the pure-Scala [[Sha256]] — SHARED, so both platforms verify the digest (frame
  * hashes must be bit-identical across platforms). The JVM-only `Sha256Spec` adds the `MessageDigest`
  * property comparison on top.
  */
class Sha256VectorsSpec extends AnyFlatSpec with Matchers:

  private def hex(bytes: Array[Byte]): String = bytes.map(b => f"$b%02x").mkString

  "Sha256.digest" should "reproduce the NIST test vectors" in:
    hex(Sha256.digest(Array.emptyByteArray)) shouldBe
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    hex(Sha256.digest("abc".getBytes("UTF-8"))) shouldBe
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    hex(Sha256.digest("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes("UTF-8"))) shouldBe
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    hex(Sha256.digest(Array.fill[Byte](1000000)('a'))) shouldBe
      "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
