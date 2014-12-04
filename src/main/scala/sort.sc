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

val anArrayOfRandomNumbers = List.fill(1000000)(Random.nextInt(1000)).toArray
val sortedNumbers = quicksort(anArrayOfRandomNumbers.reverse).toList

anArrayOfRandomNumbers.length

anArrayOfRandomNumbers.toList

