package io.github.scala_tessella.research_core

import io.github.scala_tessella.ring_seq.RingSeq.bracelet

/** Vertex signatures: the cyclic sequence of polygon side-counts around a fully surrounded vertex, taken up
  * to rotation and reflection (bracelet normal form). Everything about WHICH signatures are valid is DERIVED
  * in [[TypeCompatibility]] — nothing is hardcoded here.
  */
object Signatures:

  type VertexSignature = List[Int]

  def normalize(signature: VertexSignature): VertexSignature = signature.bracelet
