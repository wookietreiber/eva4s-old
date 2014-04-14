package eva4s
package reporting

import java.io.PrintStream

import eva4s.util._

/** Reports by printing to a given stream.
  *
  * @see [[Reporter.Console]]
  */
case class StreamReporter(stream: PrintStream) extends Reporter with Deviation with SelectionIntensity {
  def report(generation: Int, parents: Seq[Individual[_]], offspring: Seq[Individual[_]]): Unit = {
    val (fittest, average, unfittest) = deviation(offspring)
    val si = selectionIntensity(parents, offspring)

    stream.println(s"""gen: $generation fit: $fittest avg: $average unfit: $unfittest selInt: $si""")
  }

  def report(generation: Int, fittest: Individual[_]): Unit = {
    stream.println(s"""\nAfter $generation generations, the fittest individual is:\n\t$fittest""")
  }
}
