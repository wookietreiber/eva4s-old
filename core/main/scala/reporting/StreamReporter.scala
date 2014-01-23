package eva4s
package reporting

import java.io.PrintStream

import eva4s.util._

case class StreamReporter(stream: PrintStream) extends Reporter with Deviation with SelectionIntensity {
  def report(generation: Int, oldGen: Seq[Individual[_]], nextGen: Seq[Individual[_]]): Unit = {
    val (fittest, average, unfittest) = deviation(nextGen)
    val si = selectionIntensity(oldGen, nextGen)

    stream.println(s"""gen: $generation fit: $fittest avg: $average unfit: $unfittest selInt: $si""")
  }

  def report(generation: Int, result: Individual[_]): Unit = {
    stream.println(s"""\nAfter $generation generations, the fittest individual is:\n\t$result""")
  }
}
