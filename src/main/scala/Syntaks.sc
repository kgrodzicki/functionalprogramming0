
// values
val s: String = "Duh" // val er imutable!
var smut: String = "Im moldable" // var er mutable

// funksjons def
def pluss(a: Int, b: Int): Int = a + b

// klasser

class classRegular(val v1: Int, v2: Int)

// trenger val her for at v1 skal være synlig
val c1 = new classRegular(1, 2)
c1.v1

//c1.v2 // siden v2 ikke har val så er den ikke synlig.

case class classCase(v1: Int)

// trenger ikke val her i case classes
val c2 = classCase(2) // ingen new
c2.v1

// Lister
val l1 = 1 :: 2 :: 3 :: Nil
val l2 = List(1, 2, 3) // sukra kort form

// Pattern match
val x = l1 match {
  case head :: tail => head // ikke tilfeldig head :: tail syntaksen.
  case Nil => 0
}

// for comprehension
for {x <- 1 to 10} yield x // helt standard for each

for {x <- 1 to 10 if x % 2 == 0} yield x // samme men ed et filter

// flere "generatorer" og en funksjon på resultatet.
for {
  x <- 1 to 3
  y <- 4 to 6
} yield x * y



// Options

val o1 = Some(1)
val o2 = None


for {v1 <- o1; v2 <- o2} yield v1


//Recurson
def sum(l: List[Int]): Int = l match {
  case x :: xs => x + sum(xs)
  case Nil => 0
}
sum(List(1,2,3))

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
gcd(14,6)











