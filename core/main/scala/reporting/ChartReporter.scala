package eva4s
package reporting

import scalax.chart.XYChart
import scalax.chart.api._

/** Reporter visualizing its data using a chart. */
trait ChartReporter extends Reporter {

  /** Returns the generated chart. */
  def chart: XYChart

}

/** Factory for chart reporter instances. */
object ChartReporter {

  /** Displays a simple line chart. */
  case class Line(name: String) extends ChartReporter with reporting.Deviation with SelectionIntensity {

    private val fittestSeries = Seq[(Int,Double)]().toXYSeries("fittest")
    private val unfittestSeries = Seq[(Int,Double)]().toXYSeries("unfittest")
    private val averageSeries = Seq[(Int,Double)]().toXYSeries("geometric mean")
    private val selectionIntensitySeries = Seq[(Int,Double)]().toXYSeries("selection intensity")
    private val dataset = Seq(fittestSeries, unfittestSeries, averageSeries, selectionIntensitySeries)

    override val chart = XYLineChart(dataset)
    chart.plot.domain.axis.label = "generation"
    chart.show(title = name)

    def report(generation: Int, parents: Seq[Individual[_]], offspring: Seq[Individual[_]]): Unit = {
      val (fittest, average, unfittest) = deviation(offspring)

      fittestSeries.add(generation, fittest)
      unfittestSeries.add(generation, unfittest)
      averageSeries.add(generation, average)

      selectionIntensitySeries.add(generation, selectionIntensity(parents, offspring))
    }

    def report(generation: Int, fittest: Individual[_]): Unit = {
      fittestSeries.add(generation, fittest.fitness)
    }

  }

  /** Displays a deviation chart ranging from the fittest to the unfittest individual. */
  case class Deviation(name: String) extends ChartReporter with reporting.Deviation {

    private val dataset = Seq[(Int,Double,Double,Double)]().toYIntervalSeries(name)

    override val chart = XYDeviationChart(dataset)
    chart.plot.domain.axis.label = "generation"
    chart.show(title = name)

    def report(generation: Int, parents: Seq[Individual[_]], offspring: Seq[Individual[_]]): Unit = {
      val (fittest, average, unfittest) = deviation(offspring)

      dataset.add(generation, average, fittest, unfittest)
    }

    def report(generation: Int, fittest: Individual[_]): Unit = {
      val f = fittest.fitness
      dataset.add(generation, f, f, f)
    }

  }

}
