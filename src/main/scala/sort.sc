import scala.util.Random

def quicksort(xs: Array[Int]): Array[Int] = {
  if (xs.length <= 1) xs
  else {
    val pivot = xs(xs.length / 2)
    quicksort(xs filter (pivot >)) ++
      (xs filter (pivot ==)) ++
      quicksort(xs filter (pivot <))
  }
}


def msort[T](less: (T, T) => Boolean)
            (xs: List[T]): List[T] = {
  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (less(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (ys, zs) = xs splitAt n
    merge(msort(less)(ys), msort(less)(zs))
  }
}


def lt = (x: Int, y: Int) => x < y

msort(lt)(List(5, 7, 1, 3))




val aListOfRandomNumbers = List.fill(1000)(Random.nextInt(1000))
val anArrayOfRandomNumbers = aListOfRandomNumbers.toArray
val sortedNumbers = quicksort(anArrayOfRandomNumbers).toList

val sortedMerge = msort(lt)(aListOfRandomNumbers)



