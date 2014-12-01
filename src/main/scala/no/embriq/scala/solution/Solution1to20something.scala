package no.embriq.scala.solution

object Solution1to20something {
  // P01 (*) Find the last element of a list.
  //     Example:
  //     scala> last(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 8

  // The start of the definition of last should be
  //     def last[A](l: List[A]): A = ...
  // The `[A]` allows us to handle lists of any type.

  // The standard functional approach is to recurse down the list until we hit
  // the end.  Scala's pattern matching makes this easy.
  def last[A](ls: List[A]): A = ls match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def lastWithOption[A](ls: List[A]): Option[A] = ls match {
    case h :: Nil => Some(h)
    case _ :: tail => lastWithOption(tail)
    case _ => None
  }

  // P02 (*) Find the last but one element of a list.
  //     Example:
  //     scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 5

  def penultimate[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def lastNth[A](n: Int, ls: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil => resultList.head
        case _ :: tail =>
          lastNthR(count - 1,
            if (count > 0) resultList else resultList.tail,
            tail)
      }
    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }

  // P03 (*) Find the Kth element of a list.
  //     By convention, the first element in the list is element 0.
  //
  //     Example:
  //     scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 2

  def nth[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (n, h :: _) if n == 0 => h
    case (n, _ :: tail) => nth(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  // P04 (*) Find the number of elements of a list.
  //     Example:
  //     scala> length(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 6

  def length[A](ls: List[A]): Int = ls match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  // Tail recursive solution.  Theoretically more efficient; with tail-call
  // elimination in the compiler, this would run in constant space.
  // Unfortunately, the JVM doesn't do tail-call elimination in the general
  // case.  Scala *will* do it if the method is either final or is a local
  // function.  In this case, `lengthR` is a local function, so it should
  // be properly optimized.
  // For more information, see
  // http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
  def lengthTailRecursive[A](ls: List[A]): Int = {
    def lengthR(result: Int, curList: List[A]): Int = curList match {
      case Nil => result
      case _ :: tail => lengthR(result + 1, tail)
    }
    lengthR(0, ls)
  }

  // More pure functional solution, with folds.
  def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) {
    (c, _) => c + 1
  }

  // P05 (*) Reverse a list.
  //     Example:
  //     scala> reverse(List(1, 1, 2, 3, 5, 8))
  //     res0: List[Int] = List(8, 5, 3, 2, 1, 1)

  // Simple recursive.  O(n^2)
  def reverse[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => reverse(tail) ::: List(h)
  }

  // Tail recursive.
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

  // Pure functional
  def reverseFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) {
      (r, h) => h :: r
    }

  // P06 (*) Find out whether a list is a palindrome.
  //     Example:
  //     scala> isPalindrome(List(1, 2, 3, 2, 1))
  //     res0: Boolean = true

  // In theory, we could be slightly more efficient than this.  This approach
  // traverses the list twice: once to reverse it, and once to check equality.
  // Technically, we only need to check the first half of the list for equality
  // with the first half of the reversed list.  The code to do that more
  // efficiently than this implementation is much more complicated, so we'll
  // leave things with this clear and concise implementation.
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  // todo: recursive...

  // P07 (**) Flatten a nested list structure.
  //     Example:
  //     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //     res0: List[Any] = List(1, 1, 2, 3, 5, 8)

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  //     If a list contains repeated elements they should be replaced with a
  //     single copy of the element.  The order of the elements should not be
  //     changed.
  //
  //     Example:
  //     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  // Standard recursive.
  def compress[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => h :: compress(tail.dropWhile(_ == h))
  }
  // Tail recursive.
  def compressTailRecursive[A](ls: List[A]): List[A] = {
    def compressR(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
      case Nil => result.reverse
    }
    compressR(Nil, ls)
  }

  // Functional.
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) {
      (h, r) =>
        if (r.isEmpty || r.head != h) h :: r
        else r
    }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  //     If a list contains repeated elements they should be placed in separate
  //     sublists.
  //
  //     Example:
  //     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  // P10 (*) Run-length encoding of a list.
  //     Use the result of problem P09 to implement the so-called run-length
  //     encoding data compression method.  Consecutive duplicates of elements are
  //     encoded as tuples (N, E) where N is the number of duplicates of the
  //     element E.
  //
  //     Example:
  //     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1import no.embriq.scala.solution.Solution1to9.pack Solution1to9.pack

  def encode[A](ls: List[A]): List[(Int, A)] =
    pack(ls) map { e => (e.length, e.head)}

  // P11 (*) Modified run-length encoding.
  //     Modify the result of problem P10 in such a way that if an element has no
  //     duplicates it is simply copied into the result list.  Only elements with
  //     duplicates are transferred as (N, E) terms.
  //
  //     Example:
  //     scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

  def encodeModified[A](ls: List[A]): List[Any] =
    encode(ls) map { t => if (t._1 == 1) t._2 else t}

  // Just for fun, here's a more typesafe version.
  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t)}

  // P12 (**) Decode a run-length encoded list.
  //     Given a run-length code list generated as specified in problem P10,
  //     construct its uncompressed version.
  //
  //     Example:
  //     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  //     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  def decode[A](ls: List[(Int, A)]): List[A] =
    ls flatMap { e => List.fill(e._1)(e._2)}

  // P13 (**) Run-length encoding of a list (direct solution).
  //     Implement the so-called run-length encoding data compression method
  //     directly.  I.e. don't use other methods you've written (like P09's
  //     pack); do all the work directly.
  //
  //     Example:
  //     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  // This is basically a modification of P09.
  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      (packed.length, packed.head) :: encodeDirect(next)
    }

  // P14 (*) Duplicate the elements of a list.
  //     Example:
  //     scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  //     res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e)}

  // P15 (**) Duplicate the elements of a list a given number of times.
  //     Example:
  //     scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  //     res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

  def duplicateN[A](n: Int, ls: List[A]): List[A] =
    ls flatMap {
      List.fill(n)(_)
    }

  // P16 (**) Drop every Nth element from a list.
  //     Example:
  //     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  // Simple recursion.
  def drop[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(c - 1, tail)
    }
    dropR(n, ls)
  }

  // Tail recursive.
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil) => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }

  // Functional.
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0} map {
      _._1
    }

  // P17 (*) Split a list into two parts.
  //     The length of the first part is given.  Use a Tuple for your result.
  //
  //     Example:
  //     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

  // Simple recursion.
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil) => (Nil, Nil)
    case (0, list) => (Nil, list)
    case (n, h :: tail) => {
      val (pre, post) = split(n - 1, tail)
      (h :: pre, post)
    }
  }

  // Tail recursive.
  def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitR(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
      (curN, curL) match {
        case (_, Nil) => (pre.reverse, Nil)
        case (0, list) => (pre.reverse, list)
        case (n, h :: tail) => splitR(n - 1, tail, h :: pre)
      }
    splitR(n, ls, Nil)
  }

  // Functional (barely not "builtin").
  def splitFunctional[A](n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))

  // P18 (**) Extract a slice from a list.
  //     Given two indices, I and K, the slice is the list containing the elements
  //     from and including the Ith element up to but not including the Kth
  //     element of the original list.  Start counting the elements with 0.
  //
  //     Example:
  //     scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //     res0: List[Symbol] = List('d, 'e, 'f, 'g)

  // Simple recursive.
  def slice[A](start: Int, end: Int, ls: List[A]): List[A] =
    (start, end, ls) match {
      case (_, _, Nil) => Nil
      case (_, e, _) if e <= 0 => Nil
      case (s, e, h :: tail) if s <= 0 => h :: slice(0, e - 1, tail)
      case (s, e, h :: tail) => slice(s - 1, e - 1, tail)
    }

  // Tail recursive, using pattern matching.
  def sliceTailRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = {
    def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
      (count, curList) match {
        case (_, Nil) => result.reverse
        case (c, h :: tail) if end <= c => result.reverse
        case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
        case (c, _ :: tail) => sliceR(c + 1, tail, result)
      }
    sliceR(0, ls, Nil)
  }

  // Since several of the patterns are similar, we can condense the tail recursive
  // solution a little.
  def sliceTailRecursive2[A](start: Int, end: Int, ls: List[A]): List[A] = {
    def sliceR(count: Int, curList: List[A], result: List[A]): List[A] = {
      if (curList.isEmpty || count >= end) result.reverse
      else sliceR(count + 1, curList.tail,
        if (count >= start) curList.head :: result
        else result)
    }
    sliceR(0, ls, Nil)
  }

  // Functional.
  def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
    ls drop s take (e - (s max 0))

  // P19 (**) Rotate a list N places to the left.
  //     Examples:
  //     scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //     res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  //
  //     scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //     res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }

  // P20 (*) Remove the Kth element from a list.
  //     Return the list and the removed element in a Tuple.  Elements are
  //     numbered from 0.
  //
  //     Example:
  //     scala> removeAt(1, List('a, 'b, 'c, 'd))
  //     res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

  //object P20 {
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post) => (pre ::: post, e)
    case (pre, Nil) => throw new NoSuchElementException
  }

  // Alternate, with fewer builtins.
  def removeAt2[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }

  //}

  // P21 (*) Insert an element at a given position into a list.
  //     Example:
  //     scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  //     res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = ls.splitAt(n) match {
    case (pre, post) => pre ::: e :: post
  }

  // P22 (*) Create a list containing all integers within a given range.
  //     Example:
  //     scala> range(4, 9)
  //     res0: List[Int] = List(4, 5, 6, 7, 8, 9)

  // Recursive.
  def range(start: Int, end: Int): List[Int] =
    if (end < start) Nil
    else start :: range(start + 1, end)

  // Tail recursive.
  def rangeTailRecursive(start: Int, end: Int): List[Int] = {
    def rangeR(end: Int, result: List[Int]): List[Int] = {
      if (end < start) result
      else rangeR(end - 1, end :: result)
    }
    rangeR(end, Nil)
  }

  // The classic functional approach would be to use `unfoldr`, which Scala
  // doesn't have.  So we'll write one and then use it.
  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
    f(s) match {
      case None => Nil
      case Some((r, n)) => r :: unfoldRight(n)(f)
    }

  def rangeFunctional(start: Int, end: Int): List[Int] =
    unfoldRight(start) { n =>
      if (n > end) None
      else Some((n, n + 1))
    }

  // P23 (**) Extract a given number of randomly selected elements from a list.
  //     Example:
  //     scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  //     res0: List[Symbol] = List('e, 'd, 'a)
  //
  //     Hint: Use the answer to P20.

  //object P23 {
  //  import P20.removeAt
  //
  def randomSelect1[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect1(n - 1, rest)
    }

  // It can be expensive to create a new Random instance every time, so let's
  // only do it once.
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }

  //}

  // P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  //     Example:
  //     scala> lotto(6, 49)
  //     res0: List[Int] = List(23, 1, 17, 33, 21, 37)

  //object P24 {
  //  import P23.randomSelect
  def lotto(count: Int, max: Int): List[Int] =
    randomSelect(count, List.range(1, max + 1))

  //}

  // P25 (*) Generate a random permutation of the elements of a list.
  //     Hint: Use the solution of problem P23.
  //
  //     Example:
  //     scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  //     res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

  //object P25 {
  //  // This algorithm is O(n^2), but it makes up for that in simplicity of
  //  // implementation.
  //  import P23.randomSelect
  def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

  // The canonical way to shuffle imperatively is Fisher-Yates.  It requires a
  // mutable array.  This is O(n).
  //    def randomPermuteCompileError[A](ls: List[A]): List[A] = {
  //
  //      val rand = new util.Random
  //      val a = ls.toArray
  //      for (i <- a.length - 1 to 1 by -1) {
  //        val i1 = rand.nextInt(i + 1)
  //        val t = a(i)
  //        a.update(i, a(i1))
  //        a.update(i1, t)
  //      }
  //      a.toList
  //    }

  //
  //  // Efficient purely functional algorithms for shuffling are a lot harder.  One
  //  // is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
  //  // Haskell. Implementing it in Scala is left as an exercise for the reader.
  //}

  // P26 (**) Generate the combinations of K distinct objects chosen from the N
  //          elements of a list.
  //     In how many ways can a committee of 3 be chosen from a group of 12
  //     people?  We all know that there are C(12,3) = 220 possibilities (C(N,K)
  //     denotes the well-known binomial coefficient).  For pure mathematicians,
  //     this result may be great.  But we want to really generate all the possibilities.
  //
  //     Example:
  //     scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  //     res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {
        sl.head :: _
      }
    }

  // P27 (**) Group the elements of a set into disjoint subsets.
  //     a) In how many ways can a group of 9 people work in 3 disjoint subgroups
  //        of 2, 3 and 4 persons?  Write a function that generates all the
  //        possibilities.
  //
  //        Example:
  //        scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  //        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
  //
  //     b) Generalize the above predicate in a way that we can specify a list
  //        of group sizes and the predicate will return a list of groups.
  //
  //        Example:
  //        scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  //        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
  //
  //     Note that we do not want permutations of the group members;
  //     i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...).
  //     However, we make a difference between ((Aldo, Beat), (Carla, David), ...)
  //     and ((Carla, David), (Aldo, Beat), ...).
  //
  //     You may find more about this combinatorial problem in a good book on
  //     discrete mathematics under the term "multinomial coefficients".

  //    def group3[A](ls: List[A]): List[List[List[A]]] =
  //      for {
  //        a <- combinations(2, ls)
  //        noA = ls -- a
  //        b <- combinations(3, noA)
  //      } yield List(a, b, noA -- b)
  //
  //    def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
  //      case Nil     => List(Nil)
  //      case n :: ns => combinations(n, ls) flatMap { c =>
  //        group(ns, ls -- c) map {c :: _}
  //      }
  //    }

  // P28 (**) Sorting a list of lists according to length of sublists.
  //     a) We suppose that a list contains elements that are lists themselves.
  //        The objective is to sort the elements of the list according to their
  //        length.  E.g. short lists first, longer lists later, or vice versa.
  //
  //     Example:
  //     scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  //     res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  //
  //     b) Again, we suppose that a list contains elements that are lists
  //        themselves.  But this time the objective is to sort the elements
  //        according to their length frequency; i.e. in the default, sorting is
  //        done ascendingly, lists with rare lengths are placed, others with a
  //        more frequent length come later.
  //
  //     Example:
  //     scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  //     res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
  //
  //     Note that in the above example, the first two lists in the result have
  //     length 4 and 1 and both lengths appear just once.  The third and fourth
  //     lists have length 3 and there are two list of this length.  Finally, the
  //     last three lists have length 2.  This is the most frequent length.

  //object P28 {
  //  import P10.encode
  //
  //  def lsort[A](ls: List[List[A]]): List[List[A]] =
  //    ls sort { _.length < _.length }
  //
  //  def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
  //    val freqs = Map(encode(ls map { _.length } sort { _ < _ }) map { _.swap }:_*)
  //    ls sort { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
  //  }
  //}

}
