package org.example
package evo

import org.eva4s.api._

object Template extends EvolutionaryApp.Sequential {

  type Genome  = Nothing
  type Problem = Nothing

  val problem: Problem =
    ???

  def fitness(genome: Genome) =
    ???

  def creator: Genome =
    ???

  def mutator(genome: Genome): Genome =
    ???

  def recombinator(g1: Genome, g2: Genome): Genome =
    ???

}
