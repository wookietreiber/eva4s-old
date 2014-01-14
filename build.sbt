import eva4s.build._

import Dependencies._

lazy val root = (
  Eva4sProject("eva4s", ".")
  aggregate(core, benchmark, examples)
)

lazy val core = (
  Eva4sProject("eva4s-core", "core")
  settings(
    libraryDependencies ++= Seq ( extras, scalaz ),
    initialCommands in Compile += """
      import scalay.collection._
      import scalaz._
      import Scalaz._
    """
  )
)

lazy val benchmark = (
  Eva4sProject("eva4s-benchmark", "benchmark")
  dependsOn(core)
  settings(
    libraryDependencies += chart,
    initialCommands in Compile in console += """
      import org.eva4s.benchmark._
    """
  )
)

lazy val examples = (
  Eva4sProject("eva4s-examples", "examples")
  aggregate(template, tsp, simple, solver)
)

lazy val template = (
  Eva4sProject("eva4s-example-template", "examples/template")
  dependsOn(core)
)

lazy val simple = (
  Eva4sProject("eva4s-example-simple-solver", "examples/simple-solver")
  dependsOn(core)
)

lazy val tsp = (
  Eva4sProject("eva4s-example-tsp", "examples/tsp")
  dependsOn(core)
  settings(
    libraryDependencies += graph,
    initialCommands in Compile += """
      import scalax.collection._
      import scalax.collection.GraphPredef._
      import scalax.collection.GraphEdge._
      import scalax.collection.edge._
      import scalax.collection.edge.Implicits._
    """,
    initialCommands in Compile in console += """
      import org.eva4s.util.graph._
      import org.eva4s.tsp._
    """
  )
)

lazy val solver = (
  Eva4sProject("eva4s-example-solver", "examples/solver")
  dependsOn(core)
  settings(
    libraryDependencies += chart,
    initialCommands in Compile += """
      import scalax.chart.Charting._
    """,
    initialCommands in Compile in console += """
      import org.eva4s.solver._
    """
  )
)
