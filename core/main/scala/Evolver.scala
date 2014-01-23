package eva4s

import language.higherKinds

/** Executes an evolutionary algorithm. An evolver is to an evolutionary what an executor is to a
  * thread. How an evolutionary is executed (sequential, parallel, distributed) depends on the
  * actual [[Evolver]] implementation.
  */
trait Evolver[G,P] extends DocDummy {

  def reporter: Reporter

  def creator: Creator[G]

  def apply(problem: P): Individual[G]

}
