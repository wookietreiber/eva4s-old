package eva4s
package build

import sbt._

object Dependencies {
  val chart  = "com.github.wookietreiber"     %% "scala-chart" % "0.4.2"
  val graph  = "com.assembla.scala-incubator" %% "graph-core"  % "1.8.1"
  val scalaz = "org.scalaz"                   %% "scalaz-core" % "7.0.6"

  val logger = "ch.qos.logback" % "logback-classic" % "1.1.2"

  def Akka(scalaVersion: String) = CrossVersion.partialVersion(scalaVersion) match {
    case Some((2,11)) => List (
      "com.typesafe.akka" %% "akka-actor" % "2.3.2",
      "com.typesafe.akka" %% "akka-slf4j" % "2.3.2",
      logger
    )

    case Some((2,10)) => List (
      "com.typesafe.akka" %% "akka-actor" % "2.3.2",
      "com.typesafe.akka" %% "akka-slf4j" % "2.3.2",
      logger
    )

    case _ => Nil
  }
}
