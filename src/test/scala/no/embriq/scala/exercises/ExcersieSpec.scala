package no.embriq.scala.exercises

import no.embriq.scala.exercises.Excersise._
//import no.embriq.scala.solution.Solutions._
import org.scalatest._


class ExcersieSpec extends FlatSpec with Matchers with PendingIfUnimplemented {

  "P01" should "return last element of list" in {
    last(List(1,2,3)) should be === 3
    last(List(1, 1, 2, 3, 5, 8)) should be === 8

  }

  "P02" should "return second to last element of list" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be === 5

  }

  "P03" should "return nth element of list" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be === 2
  }
}

trait PendingIfUnimplemented extends SuiteMixin { this: Suite =>

  abstract override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case Failed(ex: NotImplementedError) => Failed("Oppgaven er ikke implementert enda")
      case other => other
    }
  }
}
