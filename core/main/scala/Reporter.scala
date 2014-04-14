package eva4s

import util._

/** Reports on the status of the evolution. */
trait Reporter extends DocMacros {

  /** Reports statistics of an evolutionary step.
    *
    * @param generation the current generation
    * @param parents $parents
    * @param offspring $offspring
    */
  def report(generation: Int, parents: Seq[Individual[_]], offspring: Seq[Individual[_]]): Unit

  /** Reports the fittest individual at the end of evolution.
    *
    * @param generation the final generation at which the evolution has ended
    * @param fittest the fittest individual of the final generation
    */
  def report(generation: Int, fittest: Individual[_]): Unit

}

/** Contains simple [[Reporter]] implementations. */
object Reporter {

  /** Does nothing. */
  object None extends Reporter {
    def report(generation: Int, parents: Seq[Individual[_]], offspring: Seq[Individual[_]]): Unit = ()
    def report(generation: Int, fittest: Individual[_]): Unit = ()
  }

  /** Prints everything to the [[http://en.wikipedia.org/wiki/Stdout standard output stream]]. */
  object Console extends reporting.StreamReporter(scala.Console.out)

  /** Reports to a list of subordinate reporters.
    *
    * @param reporters Returns the list of subordinate reporters.
    */
  case class Composite private (reporters: List[Reporter]) extends Reporter {

    def report(generation: Int, parents: Seq[Individual[_]], offspring: Seq[Individual[_]]) =
      reporters.foreach(_.report(generation, parents, offspring))

    def report(generation: Int, fittest: Individual[_]) =
      reporters.foreach(_.report(generation, fittest))

  }

  /** Factory for composite reporters. */
  object Composite {

    /** Returns a new composite reporter. */
    def apply(reporters: Reporter*) = new Composite(reporters.toList)

  }

}
