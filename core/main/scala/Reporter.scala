package eva4s

import util._

trait Reporter {

  def report(generation: Int, oldGen: Seq[Individual[_]], nextGen: Seq[Individual[_]]): Unit

  def report(generation: Int, result: Individual[_]): Unit

}

/** Contains simple [[Reporter]] implementations. */
object Reporter {

  /** Does nothing. */
  object None extends Reporter {
    final def report(generation: Int, oldGen: Seq[Individual[_]], nextGen: Seq[Individual[_]]) = ()
    final def report(generation: Int, result: Individual[_]) = ()
  }

  /** Prints everything to the [[http://en.wikipedia.org/wiki/Stdout standard output stream]]. */
  object Console extends reporting.StreamReporter(scala.Console.out)

  /** Reports to all subordinate reporters. */
  final case class Composite(reporters: Reporter*) extends Reporter {
    def report(generation: Int, oldGen: Seq[Individual[_]], nextGen: Seq[Individual[_]]) =
      reporters.foreach(_.report(generation, oldGen, nextGen))

    def report(generation: Int, result: Individual[_]) =
      reporters.foreach(_.report(generation, result))
  }

}
