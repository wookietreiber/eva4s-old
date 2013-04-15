/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2013  Christian Krause                                                         *
 *                                                                                               *
 *  Christian Krause  <kizkizzbangbang@googlemail.com>                                           *
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

import akka.actor._

trait AkkaEvolver[Genome,Problem] extends Evolver {

  def evolutionary: Evolutionary[Genome,Problem]

  // -----------------------------------------------------------------------------------------------
  // messages
  // -----------------------------------------------------------------------------------------------

  case class BlindDate(dater: Dater, datee: Datee)
  case class Datee(villager: ActorRef, genome: Genome, fitness: Double, pickiness: Double)
  case class Dater(villager: ActorRef, genome: Genome, fitness: Double)
  case object DIAF
  case class GammaRayBurst(exposure: Double)
  case object GetHorny
  case object GotLucky
  case class Leave(villager: ActorRef)
  case class Migrant(villager: ActorRef)
  case object Mutate
  case class Offspring(offspring: Seq[Genome])
  case class PickUp(dater: Dater)
  case class Populate(ancestors: Int)
  case object Rejected
  case class RitualMating(dater: Dater, datee: Datee)
  case class WelcomeGift(village: ActorRef)

  // -----------------------------------------------------------------------------------------------
  // actors
  // -----------------------------------------------------------------------------------------------

  class Villager(
    var genome: Genome,
    var wantedChildren: Int,
    var pickiness: Double,
    var village: ActorRef)
      extends Actor with ActorLogging {

    var rejections = 0
    var children = 0

    def datee = Datee(self, genome, evolutionary.fitness(genome), pickiness)
    def dater = Dater(self, genome, evolutionary.fitness(genome))

    def receive = {
      case GetHorny ⇒
        log.debug("Looking for a blind date.")
        village ! datee

      case GotLucky ⇒
        log.debug("Found someone to mate with.")
        pickiness += 0.01

      case Offspring(offspring) ⇒
        children += offspring.size
        log.debug("Now have {} of {} children.", offspring.size, children)

      case PickUp(dater) ⇒
        log.debug("{} is knocking on my door.", dater)
        village ! BlindDate(dater, datee)

      case Rejected ⇒
        log.debug("Was rejected.")
        rejections += 1
        wantedChildren -= 1
        pickiness -= 0.01

      case WelcomeGift(newHome) ⇒
        log.debug("Found a new home at {}.", village)
        village = newHome
    }
  }

  class Village(val huts: Int) extends Actor with ActorLogging {
    var population = Vector[ActorRef]()
    var neighbours = Vector[ActorRef]()

    val templeOfLove = context.actorOf(Props(new TempleOfLove(self)), name = "temple-of-love")
//  val templeOfDoom = context.actorOf(..., name = "temple-of-doom")
    val tavern = context.actorOf(Props(new Tavern(templeOfLove)), name = "tavern")

    val random = new util.Random()
    import random.shuffle

    def receive = {
      case date @ BlindDate(_,_) ⇒
        tavern ! date

      case dater @ Dater(villager,_,_) ⇒
        val mate = shuffle(population filter { _ != villager }).take(1)(0)
        log.debug("Chose {} as the blind date for {}.", mate, villager)
        mate ! PickUp(dater)

      case GammaRayBurst(exposure) ⇒
        log.debug("A gamma ray burst is hitting the community.")
        population foreach { villager ⇒
          if (random.nextDouble < exposure) {
            log.debug("{} is hit by gamma rays.", villager)
            villager ! Mutate
          } else {
            log.debug("{} is missed by gamma rays.", villager)
          }
        }

      case Leave(villager) ⇒
        log.debug("{} is leaving the community.", villager)
        population = population filter { _ != villager }
        val destination = shuffle(neighbours).take(1)(0)
        destination ! Migrant(villager)

      case Migrant(villager) ⇒
        log.debug("{} is joining the community.", villager)
        population :+= villager
        villager ! WelcomeGift(self)

      case Offspring(children) ⇒
        log.debug("Welcoming {} new children in our community.", children.size)
        for {
          child ← children
          villager = context.actorOf(Props(new Villager(child, 42, 42.0, self)))
        } population :+= villager

      case Populate(ancestors) ⇒
        log.debug("Populating village with {} ancestors.", ancestors)
        population = Vector.fill(ancestors) {
          context.actorOf(Props(new Villager(evolutionary.ancestor, 42, 42.0, self)))
        }

      case couple @ RitualMating(_,_) ⇒
        templeOfLove ! couple
    }
  }

  class Tavern(templeOfLove: ActorRef) extends Actor with ActorLogging {
    def receive = {
      case BlindDate(dater, datee) ⇒
        log.debug("{} is meeting {} on a blind date.", dater, datee)

        ((datee.fitness / dater.fitness) - 1) match {
          case x if x < datee.pickiness ⇒
            log.debug("{} likes {}.", datee, dater)
            dater.villager ! GotLucky
            templeOfLove ! RitualMating(dater, datee)

          case x ⇒
            log.debug("{} rejects {}.", datee, dater)
            dater.villager ! Rejected
        }
    }
  }

  class TempleOfLove(village: ActorRef) extends Actor with ActorLogging {
    def receive = {
      case RitualMating(dater, datee) ⇒
        log.debug("{} is mating with {}.", dater, datee)
        // point mutate both parent genomes
        val offspring = Offspring(evolutionary.recombine(dater.genome, datee.genome))
        dater.villager ! offspring
        datee.villager ! offspring
        village ! offspring
    }
  }

}
