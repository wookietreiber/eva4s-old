import sbt._
import Keys._

import BuildSettings._
import Dependencies._

object BuildSettings {
  lazy val buildOrganization = "com.github.scalevalgo"
  lazy val buildVersion      = "0.1.0-SNAPSHOT"
  lazy val buildScalaVersion = "2.9.2"

  lazy val baseSettings = Defaults.defaultSettings ++ Seq (
    organization   := buildOrganization,
    version        := buildVersion,
    scalaVersion   := buildScalaVersion,
    initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile,
    initialCommands in Compile in console += """
      import ea._
    """
  )
}

object ScalEvAlgoBuild extends Build {

  lazy val root = Project (
    id        = "scalevalgo",
    base      = file ("."),
    aggregate = Seq ( core, examples ),
    settings  = baseSettings
  )

  lazy val core = Project (
    id        = "scalevalgo-core",
    base      = file ("core"),
    settings  = baseSettings ++ Seq (
      libraryDependencies += scalaz,
      crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.10.0-M4")
    )
  )

  lazy val examples = Project (
    id        = "scalevalgo-examples",
    base      = file ("examples"),
    aggregate = Seq ( template, tsp ),
    settings  = baseSettings
  )

  lazy val template = Project (
    id           = "scalevalgo-example-template",
    base         = file ("examples/template"),
    settings     = baseSettings,
    dependencies = Seq ( core )
  )

  lazy val tsp = Project (
    id        = "scalevalgo-tsp",
    base      = file ("examples/tsp"),
    settings  = baseSettings ++ Seq (
      libraryDependencies += graph,
      initialCommands in Compile += """
        import scalax.collection._
        import scalax.collection.GraphPredef._
        import scalax.collection.GraphEdge._
        import scalax.collection.edge.Implicits._
        import scalaz._
        import Scalaz._
      """,
      initialCommands in Compile in console += """
        import ea.util.graph._
        import ea.tsp._
      """
    ),
    dependencies = Seq ( core )
  )

}

object Dependencies {
  lazy val graph  = "com.assembla.scala-incubator" %% "graph-core"  % "1.5.0"
  lazy val scalaz = "org.scalaz"                   %% "scalaz-core" % "6.0.4"
  lazy val specs2 = "org.specs2"                   %% "specs2"      % "1.11"  % "test"
}
