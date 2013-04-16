import sbt._
import Keys._

import BuildSettings._
import Dependencies._

object BuildSettings {
  lazy val baseSettings = Defaults.defaultSettings ++ Seq (
    organization   := "org.eva4s",
    version        := "0.1.0-SNAPSHOT",
    scalaVersion   := "2.10.1",
    initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile,
    initialCommands in Compile in console += """
      import org.eva4s._
    """
  )
}

object eva4s extends Build {

  // -----------------------------------------------------------------------------------------------
  // core
  // -----------------------------------------------------------------------------------------------

  lazy val root = Project (
    id        = "eva4s",
    base      = file ("."),
    aggregate = Seq ( core, evolvers, examples ),
    settings  = baseSettings
  )

  lazy val core = Project (
    id        = "core",
    base      = file ("core"),
    settings  = baseSettings ++ Seq (
      name := "eva4s-core",
      libraryDependencies ++= Seq ( extras, scalaz ),
      initialCommands in Compile += """
        import scalay.collection._
        import scalaz._
        import Scalaz._
      """
    )
  )

  // -----------------------------------------------------------------------------------------------
  // examples
  // -----------------------------------------------------------------------------------------------

  lazy val examples = Project (
    id        = "examples",
    base      = file ("examples"),
    aggregate = Seq ( template, tsp, solver ),
    settings  = baseSettings ++ Seq (
      name := "eva4s-examples"
    )
  )

  lazy val template = Project (
    id           = "example-template",
    base         = file ("examples/template"),
    dependencies = Seq ( core ),
    settings     = baseSettings ++ Seq (
      name := "eva4s-example-template"
    )
  )

  lazy val tsp = Project (
    id           = "tsp",
    base         = file ("examples/tsp"),
    dependencies = Seq ( core ),
    settings     = baseSettings ++ Seq (
      name := "eva4s-tsp",
      libraryDependencies += graph,
      initialCommands in Compile += """
        import scalax.collection._
        import scalax.collection.GraphPredef._
        import scalax.collection.GraphEdge._
        import scalax.collection.edge._
        import scalax.collection.edge.Implicits._
        import scalaz._
        import Scalaz._
      """,
      initialCommands in Compile in console += """
        import org.eva4s.util.graph._
        import org.eva4s.tsp._
      """
    )
  )

  lazy val solver = Project (
    id        = "solver",
    base      = file ("examples/solver"),
    settings  = baseSettings ++ Seq (
      name := "eva4s-equation-solver",
      libraryDependencies += chart,
      initialCommands in Compile += """
        import org.sfree.chart.Charting._
      """,
      initialCommands in Compile in console += """
        import org.eva4s.solver._
      """
    ),
    dependencies = Seq ( core )
  )

  // -----------------------------------------------------------------------------------------------
  // evolvers
  // -----------------------------------------------------------------------------------------------

  lazy val evolvers = Project (
    id        = "evolvers",
    base      = file ("evolvers"),
    aggregate = Seq ( akka ),
    settings  = baseSettings ++ Seq (
      name := "eva4s-evolvers"
    )
  )

  lazy val akka = Project (
    id           = "evolver-akka",
    base         = file ("evolvers/akka"),
    dependencies = Seq ( core ),
    settings     = baseSettings ++ Seq (
      libraryDependencies ++= Seq ( actor ),
      name := "eva4s-evolver-akka"
    )
  )

}

object Dependencies {
  lazy val chart  = "com.github.wookietreiber"           %% "scala-chart"       % "0.2.0"
  lazy val graph  = "com.assembla.scala-incubator"       %% "graph-core"        % "1.6.1"
  lazy val extras = "com.github.scala-collection-extras" %% "collection-extras" % "latest.integration"
  lazy val scalaz = "org.scalaz"                         %% "scalaz-core"       % "7.0.0-RC2"
  lazy val actor  = "com.typesafe.akka"                  %% "akka-actor"        % "2.1.2"
}
