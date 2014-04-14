import eva4s.build._

import Dependencies._

lazy val root = (
  Eva4sProject("eva4s", ".")
  aggregate(core, examples)
)

lazy val core = (
  Eva4sProject("eva4s-core", "core")
  settings(
    libraryDependencies ++= Seq(chart, scalaz) ++ Akka(scalaVersion.value),
    initialCommands in Compile += """
      import scalax.chart.api._
    """
  )
)

lazy val examples = (
  Eva4sProject("eva4s-examples", "examples")
  aggregate(template, tsp, simple/*, solver*/)
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
      import eva4s.util.graph._
      import eva4s.tsp._
    """
  )
)

lazy val solver = (
  Eva4sProject("eva4s-example-solver", "examples/solver")
  dependsOn(core)
  settings(
    initialCommands in Compile in console += """
      import eva4s.solver._
    """
  )
)
