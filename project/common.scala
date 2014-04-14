package eva4s

import sbt._
import Keys._

package object build {
  def Eva4sProject(name: String, path: String) = (
    Project(name, file(path))
    settings (
      sourceDirectory <<= baseDirectory(identity),
      initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile,
      initialCommands in Compile in console += """
        import eva4s._
      """
    )
  )
}
