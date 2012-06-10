import sbt._
import Keys._

import BuildSettings._
import Dependencies._
import Resolvers._

object BuildSettings {
  lazy val buildOrganization = "com.github.scalevalgo"
  lazy val buildVersion      = "0.1.0-SNAPSHOT"
  lazy val buildScalaVersion = "2.9.1"

  lazy val baseSettings = Defaults.defaultSettings ++ Seq (
    organization   := buildOrganization,
    version        := buildVersion,
    scalaVersion   := buildScalaVersion,
    resolvers     ++= Seq ( sonatype )
  )
}

object ScalEvAlgoBuild extends Build {
  lazy val root = Project (
    id        = "scalevalgo",
    base      = file ("."),
    settings  = baseSettings ++ Seq (
      libraryDependencies ++= Seq ( graph, scalaz ),
      scalacOptions ++= Seq ( "-Xexperimental", "-Ydependent-method-types" ),
      initialCommands in Compile += """
        import scalax.collection._
        import scalax.collection.GraphPredef._
        import scalax.collection.GraphEdge._
        import scalax.collection.edge.Implicits._
        import scalaz._
        import Scalaz._
      """,
      initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile,
      initialCommands in Compile in console += """
        import ea._
      """
    )
  )
}

object Dependencies {
  lazy val graph  = "com.assembla.scala-incubator" %% "graph-core"  % "1.4.3"
  lazy val scalaz = "org.scalaz"                   %% "scalaz-core" % "6.0.4"
  lazy val specs2 = "org.specs2"                   %% "specs2"      % "1.11"  % "test"
}

object Resolvers {
  lazy val sonatype = "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
}

