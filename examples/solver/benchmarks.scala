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

import org.sfree.chart.Charting._

object Benchmark {

  def plotter(solvers: EvolutionarySolver[_]*): org.jfree.chart.JFreeChart = {
    val dataset = solvers map { solver ⇒
      val buf = collection.mutable.ListBuffer[(Int,Double)]()

      val charter: Option[(Int,Double) ⇒ Unit] = Some { (g: Int, f: Double) ⇒
        buf += (g → f)
      }

      buf.toXYSeries {
        if (solver.isInstanceOf[RealSolver])
          solver.problem.toString + " real"
        else if (solver.isInstanceOf[BinarySolver])
          solver.asInstanceOf[BinarySolver].p.toString + " binary"
        else ""
      }
    } toXYSeriesCollection

    val chart = createLineChart(dataset)

    val plot = chart.getPlot.asInstanceOf[org.jfree.chart.plot.XYPlot]
    plot.getDomainAxis.setLabel("generations")
    plot.getRangeAxis.setLabel("average fitness")

    chart
  }

  def plotter(fs: Seq[BoundedEquation], binary: Boolean = false): org.jfree.chart.JFreeChart = {
    val solvers = if (binary)
      Seq(fs map { new BinarySolver(5, _) } : _*)
    else
      Seq(fs map { new RealSolver(5, _) } : _*)
    plotter(solvers: _*)
  }

  def funcs(ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = Map (
    "real" → (ns map { n ⇒
      n → (fs map { f ⇒
        val solver = new RealSolver(n, f)
        val individual = SplitEvolver(solver)()
        (f.toString,individual.fitness)
      } toMap)
    } toMap),
    "binary" → (ns map { n ⇒
      n → (fs map { f ⇒
        val solver = new BinarySolver(n, f)
        val individual = SplitEvolver(solver)()
        (f.toString,individual.fitness)
      } toMap)
    } toMap)
  ) toCombinedDomainBarChart

  val fs = Seq(Equation.griewank, Equation.ackley, Equation.alphans)

  def npopBinary(problem: BoundedEquation,
                 is: Seq[Int] = Seq(10,25,50,100,200),
                 generations: Int = 200,
                 ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    is map { i ⇒
      val solver = new BinarySolver(n, problem)
      val individual = SplitEvolver(solver)(generations, i)
      (i,individual.fitness)
    } toXYSeries(n.toString)
  } toXYSeriesCollection

  def npopReal(problem: BoundedEquation,
               is: Seq[Int] = Seq(10,25,50,100,200),
               generations: Int = 200,
               ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    is map { i ⇒
      val solver = new RealSolver(n, problem)
      val individual = SplitEvolver(solver)(generations, i)
      (i,individual.fitness)
    } toXYSeries(n.toString)
  } toXYSeriesCollection

  def matchmakerBinary(problem: BoundedEquation,
                       generations: Int = 500,
                       ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    val x = matchmakers[Boolean] map { case (name,m) ⇒
      val solver = new BinarySolver(n, problem)
      val individual = SplitEvolver(solver)(generations, 200)
      (name,individual.fitness)
    } toMap

    n → x
  } toCategoryDataset

  def matchmakerReal(problem: BoundedEquation,
                     generations: Int = 500,
                     ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    val x = matchmakers[Double] map { case (name,m) ⇒
      val solver = new RealSolver(n, problem)
      val individual = SplitEvolver(solver)(generations, 200)
      (name,individual.fitness)
    } toMap

    n → x
  } toCategoryDataset

  def crossoversBinary(problem: BoundedEquation,
                       generations: Int = 500,
                       ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    val x = binaryCrossovers map { case (name,cross) ⇒
      val solver = new BinarySolver(n, problem, cross)
      val individual = SplitEvolver(solver)(generations, 200)
      (name,individual.fitness)
    } toMap

    n → x
  } toCategoryDataset

  def crossoversReal(problem: BoundedEquation,
                     generations: Int = 500,
                     ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    val x = realCrossovers map { case (name,cross) ⇒
      val solver = new RealSolver(n, problem, cross)
      val individual = SplitEvolver(solver)(generations, 200)
      (name,individual.fitness)
    } toMap

    n → x
  } toCategoryDataset

  def mutagensReal(problem: BoundedEquation,
                   ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    val x = mutagens map { case (name,mutagen) ⇒
      val solver = new RealSolver(n, problem)
      val individual = SplitEvolver(solver)(generations = 500, individuals = 200)
      (name,individual.fitness)
    } toMap

    n → x
  } toCategoryDataset

  def mutagensBinary(problem: BoundedEquation,
                     ns: Seq[Int] = Seq(5,10,15,20,25,30,40)) = ns map { n ⇒
    val x = mutagens map { case (name,mutagen) ⇒
      val solver = new BinarySolver(n, problem)
      val individual = SplitEvolver(solver)(generations = 500, individuals = 200)
      (name,individual.fitness)
    } toMap

    n → x
  } toCategoryDataset

  import Mutagens._

  def mutagens: Map[String,Mutagen] = Map (
    "Constant(0.1)" → ConstantMutagen(0.1),
    "Constant(0.3)" → ConstantMutagen(0.3),
    "Constant(0.8)" → ConstantMutagen(0.8),
    "Linear Decreasing (0.8, 0.01)" → LinearDecreasingMutagen(0.8, 0.01)(500),
    "Exponential Decreasing (0.8, 0.01)" → ExponentialDecreasingMutagen(0.8, 0.01)(500)
  )

  import RealSolver._

  def realCrossovers: Map[String,(Vector[Double],Vector[Double]) ⇒ Iterable[Vector[Double]]] = Map (
    "Arithmetic Crossover" → ArithmeticCrossover,
    "Intermediate Crossover" → IntermediateCrossover,
    "Line Crossover" → LineCrossover
  )

  import BinarySolver._

  def binaryCrossovers: Map[String,(Vector[Boolean],Vector[Boolean]) ⇒ Iterable[Vector[Boolean]]] = Map (
    "One Point Crossover" → OnePointCrossover,
    "Two Point Crossover" → TwoPointCrossover,
    "Uniform Crossover"   → UniformCrossover
  )

  import Matchmaking._

  def matchmakers[G]: Map[String,Matchmaker[G]] = Map (
    "Random" → RandomForcedMatchmaker[G] _,
    "Tournament" → TournamentMatchmaker[G](10) _,
    "Multiple Tournament" → MultipleTournamentMatchmaker[G](10) _,
    "Rank Based" → RankBasedMatchmaker[G] _
  )

}
