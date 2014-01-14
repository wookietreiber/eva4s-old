package eva4s
package build

import sbt._

object Dependencies {
  val chart  = "com.github.wookietreiber"           %% "scala-chart"       % "0.3.0"
  val graph  = "com.assembla.scala-incubator"       %% "graph-core"        % "1.6.1"
  val extras = "com.github.scala-collection-extras" %% "collection-extras" % "latest.integration"
  val scalaz = "org.scalaz"                         %% "scalaz-core"       % "7.0.5"
}
