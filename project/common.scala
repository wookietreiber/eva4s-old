package eva4s

import sbt._
import Keys._

package object build {
  val commonSettings = Seq (
    organization := "org.eva4s",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "2.10.3",
    sourceDirectory <<= baseDirectory(identity),
    initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile,
    initialCommands in Compile in console += """
      import org.eva4s._
    """
  )
}
