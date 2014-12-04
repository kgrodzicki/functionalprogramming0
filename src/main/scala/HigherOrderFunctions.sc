
// tre klassiske FP funksjoner

// Map
def map[A, B](list: List[A], f: (A) => B): List[B] = list match {
  case Nil => Nil
  case x :: xs => f(x) :: map(xs, f)
}

// filter
def filter[A](list: List[A], f: (A) => Boolean): List[A] = list match {
  case Nil => Nil
  case x :: xs if f(x) => x :: filter(xs, f)
  case _ :: xs => filter(xs, f)
}

// Reduce
def reduce[A, B](init: B)(list: List[A], f: (B, A) => B): B = list match {
  case Nil => init
  case x :: xs => reduce(f(init, x))(xs, f)
}


// Liste fra 1 til 10
val l1 = (1 to 10).toList

// funksjone som dobler et tall
val double: Int => Int = 2 * _

// tester om et tall er 2
def isTwoFunc(i: Int): Boolean = 2 == i
val isTwoVal = (x: Int) => x == 2

def isMoreThanOne(i: Int): Boolean = 1 < i

map(l1, double)
filter(l1, isTwoFunc)
filter(l1, isMoreThanOne)
filter(l1, (i: Int) => i > 1)

// sum
reduce(0)(l1, (i1: Int, i2: Int) => i1 + i2)
// sum function as partially applied reduce
val sum = reduce(0)(_: List[Int], (i1: Int, i2: Int) => i1 + i2)
sum(l1)

// string concat
reduce("")(l1, (s: String, i: Int) => s + i)


