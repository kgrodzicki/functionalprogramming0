val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
val abcdef = List('a', 'b', 'c', 'd', 'e', 'f')
list.head // O(1)
list.tail // O(1)
list.last // O(n)
list.init // O(n)
list.reverse
list.drop(5)
list.take(5)
list.splitAt(5)

abcdef zip list
(abcdef zip list).unzip

abcdef zipWithIndex

List(1, 2, 3) map (_ + 1)

val words = List("the", "quick", "brown", "fox")
words map (_.toList)
words flatMap (_.toList)

List(Option(1), None, Option(2)) map (x => x)
List(Option(1), None, Option(2)) flatMap (x => x)
List(Option(1), None, Option(2)) flatMap _


