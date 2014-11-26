package no.embriq.scala.solution

import java.util.NoSuchElementException

object Solutions {

  // P01 (*) Find the last element of a list.
  //     Example:
  //     scala> last(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 8
  def last[A](l: List[A]): A = l match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  // P02 (*) Find the last but one element of a list.
  //     Example:
  //     scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 5
  def penultimate[A](l: List[A]): A = l match {
    case x :: _ :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  // P03 (*) Find the Kth element of a list.
  //     By convention, the first element in the list is element 0.
  //
  //     Example:
  //     scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 2
  def nth[A](n: Int, l: List[A]): A = (n, l) match {
    case (0, x :: xs) => x
    case (n, _ :: xs) => nth(n - 1, xs)
    case (_, Nil) => throw new NoSuchElementException
  }

}


