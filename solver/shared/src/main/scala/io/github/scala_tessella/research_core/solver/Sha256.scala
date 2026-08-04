package io.github.scala_tessella.research_core.solver

/** Pure-Scala SHA-256 (FIPS 180-4) — `java.security.MessageDigest` is not in Scala Native's javalib, and
  * the frame-key hashes must be IDENTICAL on every platform (they name the shared artifact directories).
  * Verified against the NIST vectors and, on the JVM, property-checked against `MessageDigest` (Sha256Spec).
  */
private[solver] object Sha256:

  // fractional parts of the cube roots of the first 64 primes
  private val K = Array(0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
    0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7,
    0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc,
    0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351,
    0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e,
    0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585,
    0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f,
    0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2)

  def digest(message: Array[Byte]): Array[Byte] =
    // fractional parts of the square roots of the first 8 primes
    var h0 = 0x6a09e667; var h1 = 0xbb67ae85; var h2 = 0x3c6ef372; var h3 = 0xa54ff53a
    var h4 = 0x510e527f; var h5 = 0x9b05688c; var h6 = 0x1f83d9ab; var h7 = 0x5be0cd19

    // padding: 0x80, zeros, 64-bit big-endian bit length, to a multiple of 64 bytes
    val bitLen = message.length.toLong * 8
    val padded = new Array[Byte](((message.length + 8) / 64 + 1) * 64)
    System.arraycopy(message, 0, padded, 0, message.length)
    padded(message.length) = 0x80.toByte
    var i      = 0
    while i < 8 do
      padded(padded.length - 1 - i) = ((bitLen >>> (8 * i)) & 0xff).toByte
      i += 1

    val w     = new Array[Int](64)
    var block = 0
    while block < padded.length do
      var t = 0
      while t < 16 do
        val o = block + t * 4
        w(t) = ((padded(o) & 0xff) << 24) | ((padded(o + 1) & 0xff) << 16) |
          ((padded(o + 2) & 0xff) << 8) | (padded(o + 3) & 0xff)
        t += 1
      while t < 64 do
        val s0 = Integer.rotateRight(w(t - 15), 7) ^ Integer.rotateRight(w(t - 15), 18) ^ (w(t - 15) >>> 3)
        val s1 = Integer.rotateRight(w(t - 2), 17) ^ Integer.rotateRight(w(t - 2), 19) ^ (w(t - 2) >>> 10)
        w(t) = w(t - 16) + s0 + w(t - 7) + s1
        t += 1
      var a = h0; var b = h1; var c = h2; var d = h3
      var e = h4; var f = h5; var g = h6; var h = h7
      t = 0
      while t < 64 do
        val s1   = Integer.rotateRight(e, 6) ^ Integer.rotateRight(e, 11) ^ Integer.rotateRight(e, 25)
        val ch   = (e & f) ^ (~e & g)
        val tmp1 = h + s1 + ch + K(t) + w(t)
        val s0   = Integer.rotateRight(a, 2) ^ Integer.rotateRight(a, 13) ^ Integer.rotateRight(a, 22)
        val maj  = (a & b) ^ (a & c) ^ (b & c)
        val tmp2 = s0 + maj
        h = g; g = f; f = e; e = d + tmp1
        d = c; c = b; b = a; a = tmp1 + tmp2
        t += 1
      h0 += a; h1 += b; h2 += c; h3 += d
      h4 += e; h5 += f; h6 += g; h7 += h
      block += 64

    val out = new Array[Byte](32)
    for (v, j) <- Array(h0, h1, h2, h3, h4, h5, h6, h7).zipWithIndex do
      out(j * 4) = (v >>> 24).toByte
      out(j * 4 + 1) = (v >>> 16).toByte
      out(j * 4 + 2) = (v >>> 8).toByte
      out(j * 4 + 3) = v.toByte
    out
