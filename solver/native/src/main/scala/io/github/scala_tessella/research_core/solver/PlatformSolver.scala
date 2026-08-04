package io.github.scala_tessella.research_core.solver

/** The Scala Native default live solver behind the shared enumerators: CaDiCaL through IPASIR. */
private[solver] object PlatformSolver:
  def default(timeoutSeconds: Int = 3600): SatSolver = CadicalSolver(timeoutSeconds)
