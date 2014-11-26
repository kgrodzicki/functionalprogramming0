package no.embriq.scala.exercises

object Excersise {

  // P01 (*) Find the last element of a list.
  //     Example:
  //     scala> last(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 8
  def last[A](l: List[A]): A = ???

  // P02 (*) Find the last but one element of a list.
  //     Example:
  //     scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 5
  def penultimate[A](l: List[A]): A = ???

  // P03 (*) Find the Kth element of a list.
  //     By convention, the first element in the list is element 0.
  //
  //     Example:
  //     scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 2
  def nth[A](n: Int, l: List[A]): A = ???

}
