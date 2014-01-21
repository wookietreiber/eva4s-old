package org.example
package evo

import eva4s.api.app._

object Template extends EvolutionaryApp.Sequential {

  type Genome  = Nothing
  type Problem = Nothing

  val problem: Problem = ???

  def fitness(genome: Genome): Double = ???

  def create: Genome = ???

  def mutate(genome: Genome): Genome = ???

  def recombine(g1: Genome, g2: Genome): Genome = ???

}
