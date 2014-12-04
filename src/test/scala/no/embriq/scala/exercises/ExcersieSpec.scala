package no.embriq.scala.exercises

import no.embriq.scala.exercises.Excersise._

import org.scalatest._

class ExcersieSpec extends FlatSpec with Matchers with PendingIfUnimplemented {

  "P01" should "return last element of list" in {
    last(List(1, 2, 3)) shouldBe 3
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
  }

  "P02" should "return second to last element of list" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
  }

  "P03" should "return nth element of list" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
  }

  "P04" should "Find the number of elements of a list." in {
    lengthRecursive(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

  "P05" should "Reverse a list." in {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  "P06" should "Find out whether a list is a palindrome." in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 3, 2, 2)) shouldBe false
  }

  "P07" should "Flatten a nested list structure." in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "P08" should "Eliminate consecutive duplicates of list elements." in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "P09" should "Pack consecutive duplicates of list elements into sublists." in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "P10" should "Run-length encoding of a list." in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  "P11" should "Modified run-length encoding." in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  "P12" should "Decode a run-length encoded list." in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "P13" should "Run-length encoding of a list." in {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  "P14" should "Duplicate the elements of a list." in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "P15" should "Duplicate the elements of a list a given number of times." in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "P16" should "Drop every Nth element from a list." in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "P17" should "Split a list into two parts." in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "P18" should "Extract a slice from a list." in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g)
  }

  "P19" should "Rotate a list N places to the left." in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "P20" should "Remove the Kth element from a list." in {
    removeAt(1, List('a, 'b, 'c, 'd)) shouldBe(List('a, 'c, 'd), 'b)
  }

  "P21" should "Insert an element at a given position into a list." in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldBe List('a, 'new, 'b, 'c, 'd)
  }

  "P22" should "Create a list containing all integers within a given range." in {
    range(4, 9) shouldBe List(4, 5, 6, 7, 8, 9)
  }

//  "P23" should "Extract a given number of randomly selected elements from a list." in {
//    randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) shouldBe List('e, 'd, 'a)
//  }

//  "P24" should "Lotto: Draw N different random numbers from the set 1..M." in {
//    lotto(6, 49) shouldBe List(23, 1, 17, 33, 21, 37)
//  }

//  "P25" should "Generate a random permutation of the elements of a list." in {
//    randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) shouldBe List('b, 'a, 'd, 'c, 'e, 'f)
//  }

//  "P26" should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
//    // todo: sammenlingingen er ikke komplett
//    combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) shouldBe List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e))
//  }

//  "P27" should "Group the elements of a set into disjoint subsets. a)" in {
//    // todo: sammenlingingen er ikke komplett
//    group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) shouldBe List(List(List("Aldo", "Beat"), List("Carla", "David", "Evi"), List("Flip", "Gary", "Hugo", "Ida")))
//  }
//
//  "P27" should "Group the elements of a set into disjoint subsets. b)" in {
//    // todo: sammenlingingen er ikke komplett
//    group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) shouldBe List(List(List("Aldo", "Beat"), List("Carla", "David"), List("Evi", "Flip", "Gary", "Hugo", "Ida")))
//  }
//
//  "P28" should "Sorting a list of lists according to length of sublists. a)" in {
//    lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) shouldBe List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
//  }
//
//  "P28" should "Sorting a list of lists according to length of sublists. b)" in {
//    lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) shouldBe List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
//  }

}

trait PendingIfUnimplemented extends SuiteMixin {
  this: Suite =>

  abstract override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case Failed(ex: NotImplementedError) => Failed("Oppgaven er ikke implementert enda")
      case other => other
    }
  }
}
