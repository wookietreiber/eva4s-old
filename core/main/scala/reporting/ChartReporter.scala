package eva4s
package reporting

import scalax.chart.Charting._

object ChartReporter {

  case class Line(name: String) extends Reporter with reporting.Deviation with SelectionIntensity {

    private val fittestSeries = Seq[(Int,Double)]().toXYSeries("fittest")
    private val unfittestSeries = Seq[(Int,Double)]().toXYSeries("unfittest")
    private val averageSeries = Seq[(Int,Double)]().toXYSeries("geometric mean")
    private val selectionIntensitySeries = Seq[(Int,Double)]().toXYSeries("selection intensity")
    private val dataset = Seq(fittestSeries, unfittestSeries, averageSeries, selectionIntensitySeries).toXYSeriesCollection

    val chart = XYLineChart(dataset)
    chart.domainAxisLabel = "generation"
    chart.show(title = name)

    def report(generation: Int, oldGen: Seq[Individual[_]], nextGen: Seq[Individual[_]]): Unit = {
      val (fittest, average, unfittest) = deviation(nextGen)

      fittestSeries.add(generation, fittest)
      unfittestSeries.add(generation, unfittest)
      averageSeries.add(generation, average)

      selectionIntensitySeries.add(generation, selectionIntensity(oldGen, nextGen))
    }

    def report(generation: Int, result: Individual[_]): Unit = {
      fittestSeries.add(generation, result.fitness)
    }

  }

  case class Deviation(name: String) extends Reporter with reporting.Deviation {

    private val generationSeries = Seq[(Int,Double,Double,Double)]().toYIntervalSeries(name)
    private val dataset = Seq(generationSeries).toYIntervalSeriesCollection

    val chart = XYDeviationChart(dataset)
    chart.domainAxisLabel = "generation"
    chart.show(title = name)

    def report(generation: Int, oldGen: Seq[Individual[_]], nextGen: Seq[Individual[_]]): Unit = {
      val (fittest, average, unfittest) = deviation(nextGen)

      generationSeries.add(generation, average, fittest, unfittest)
    }

    def report(generation: Int, result: Individual[_]): Unit = {
      val f = result.fitness
      generationSeries.add(generation, f, f, f)
    }

  }

}
