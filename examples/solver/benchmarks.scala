/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2012  Nils Foken, Christian Krause                                             *
 *                                                                                               *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                                          *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>                                    *
 *                    <kizkizzbangbang@googlemail.com>                                           *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  This file is part of 'eva4s'.                                                                *
 *                                                                                               *
 *  This project is free software: you can redistribute it and/or modify it under the terms      *
 *  of the GNU General Public License as published by the Free Software Foundation, either       *
 *  version 3 of the License, or any later version.                                              *
 *                                                                                               *
 *  This project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;    *
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
 *  See the GNU General Public License for more details.                                         *
 *                                                                                               *
 *  You should have received a copy of the GNU General Public License along with this project.   *
 *  If not, see <http://www.gnu.org/licenses/>.                                                  *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


package org.eva4s
package solver

import language.postfixOps
import language.reflectiveCalls

import scala.collection.mutable.ListBuffer
import scala.swing.Swing

import org.jfree.chart.JFreeChart
import org.sfree.chart.Charting._

object Benchmark {

  def xyChartMod(chart: JFreeChart): JFreeChart = {
    Swing onEDT {
      chart.domainAxisLabel = "generations"
      chart.rangeAxisLabel = "geometric mean fitness"

      val plot = chart.getXYPlot

      for { i ← 0 until plot.getDataset.getSeriesCount }
        plot.getRenderer.setSeriesStroke(i, new java.awt.BasicStroke(1.5f))
    }

    chart
  }

  def plotter(solvers: EvolutionarySolver[_]*): JFreeChart = {
    val dataset = solvers map { solver ⇒
      val buf = ListBuffer[(Int,Double)]()

      SplitEvolver(solver)()(debugger = charter(buf))

      buf toXYSeries {
        if (solver.isInstanceOf[RealSolver])
          solver.problem.toString + " real"
        else if (solver.isInstanceOf[BinarySolver])
          solver.asInstanceOf[BinarySolver].p.toString + " binary"
        else ""
      }
    } toXYSeriesCollection

    xyChartMod(XYLineChart(dataset))
  }

  def vectorSize(f: BoundedEquation, ns: Seq[Int] = Seq(5,10,15,20,25,30,40)): JFreeChart = {
    val dataset = ns map { n ⇒
      val solver = new RealSolver(n, f)
      val buf = ListBuffer[(Int,Double)]()
      SplitEvolver(solver)()(debugger = charter(buf))
      buf.toXYSeries(n.toString)
    } toXYSeriesCollection

    xyChartMod(XYLineChart(dataset, title = f.toString))
  }

  def population(f: BoundedEquation, ps: Seq[Int] = Seq(5,10,25,50,100,200,500)): JFreeChart = {
    val dataset = ps map { p ⇒
      val solver = new RealSolver(5, f)
      val buf = ListBuffer[(Int,Double)]()
      SplitEvolver(solver)(individuals = p)(debugger = charter(buf))
      buf.toXYSeries(p.toString)
    } toXYSeriesCollection

    xyChartMod(XYLineChart(dataset, title = f.toString))
  }

  def matchmaker(f: BoundedEquation): JFreeChart = {
    val dataset = matchmakers[Vector[Double]] map { case(name,m) ⇒
      val solver = new RealSolver(5, f)
      val buf = ListBuffer[(Int,Double)]()
      SplitEvolver(solver)()(matchmaker = m, debugger = charter(buf))
      buf.toXYSeries(name)
    } toXYSeriesCollection

    xyChartMod(XYLineChart(dataset, title = f.toString))
  }

  def crossover(f: BoundedEquation): JFreeChart = {
    val dataset = realCrossovers map { case (name,cross) ⇒
      val solver = new RealSolver(5, f, cross)
      val buf = ListBuffer[(Int,Double)]()
      SplitEvolver(solver)()(debugger = charter(buf))
      buf.toXYSeries(name)
    } toXYSeriesCollection

    xyChartMod(XYLineChart(dataset, title = f.toString))
  }

  def mutagen(f: BoundedEquation): JFreeChart = {
    val dataset = mutagens map { case m ⇒
      val solver = new RealSolver(5, f)
      val buf = ListBuffer[(Int,Double)]()
      SplitEvolver(solver)(generations = 2000)(mutagen = m, debugger = charter(buf))
      buf.toXYSeries(m.toString)
    } toXYSeriesCollection

    xyChartMod(XYLineChart(dataset, title = f.toString))
  }

  import Mutagens._

  def mutagens: Seq[Mutagen] = Seq (
    ConstantMutagen(0.2),
    ConstantMutagen(0.8),
    PolynomialMutagen( 1.0, 2000),
    PolynomialMutagen( 2.0, 2000),
    PolynomialMutagen( 3.0, 2000),
    PolynomialMutagen(16.0, 2000),
    ExponentialMutagen(2000)
  )

  import RealSolver._

  def realCrossovers: Map[String,(Vector[Double],Vector[Double]) ⇒ Seq[Vector[Double]]] = Map (
    // TODO include after NaN error is fixed "Arithmetic Crossover" → ArithmeticCrossover,
    "Intermediate Crossover" → IntermediateCrossover,
    "Line Crossover" → LineCrossover
  )

  import BinarySolver._

  def binaryCrossovers: Map[String,(Vector[Boolean],Vector[Boolean]) ⇒ Seq[Vector[Boolean]]] = Map (
    "One Point Crossover" → OnePointCrossover,
    "Two Point Crossover" → TwoPointCrossover,
    "Uniform Crossover"   → UniformCrossover
  )

  import Matchmaking._

  def matchmakers[G]: Map[String,Matchmaker[G]] = Map (
    "Tournament" → TournamentMatchmaker[G](10) _,
    "Multiple Tournament" → MultipleTournamentMatchmaker[G](10) _,
    "Rank Based" → RankBasedMatchmaker[G] _
  )

}
