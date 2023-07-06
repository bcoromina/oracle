package problem2

import org.scalatest.funsuite.AnyFunSuite
import MergeSortedList._
class MergeSortedListSpec extends AnyFunSuite{
  test("Integers") {
    val result = MergeSortedList.mergeSortedLists(List(1, 3, 5, 7), List(2, 4, 6, 8))
    assert(result ==  List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("Random class") {
    case class Person(name: String, age: Int)

    implicit val personOrdering = new Ordering[Person] {
      override def compare(x: Person, y: Person): Int = x.age compare y.age
    }

    val left = List(
      Person("a", 1),
      Person("c", 3),
      Person("e", 5),
      Person("g", 7)
    )

    val right = List(
      Person("b", 2),
      Person("d", 4),
      Person("f", 6),
      Person("h", 8)
    )

    val expected = List(
      Person("a", 1),
      Person("b", 2),
      Person("c", 3),
      Person("d", 4),
      Person("e", 5),
      Person("f", 6),
      Person("g", 7),
      Person("h", 8)
    )

    assert((left, right).merge == expected)
  }
}
