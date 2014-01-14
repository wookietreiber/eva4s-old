package eva4s
package build

import sbt._
import Keys._

object Eva4sProject {
  def apply(name: String, path: String) = (
    Project(name, file(path))
    settings((commonSettings): _*)
  )
}
