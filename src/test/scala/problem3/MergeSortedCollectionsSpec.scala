package problem3

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer
import SortedCollectionsBuilders._
class MergeSortedCollectionsSpec extends AnyFunSuite{

  test("inmutable list"){
    val result = MergeSortedCollections.mergeSorted( List(1,3,5,7), List(2,4,6,8) )
    assert( result == List(1, 2, 3, 4, 5, 6, 7, 8) )
  }

  test("inmutable vector") {
    val result = MergeSortedCollections.mergeSorted( Vector(1, 3, 5, 7), Vector(2, 4, 6, 8) )
    assert(result == Vector(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("inmutable ListBuffer") {
    val result = MergeSortedCollections.mergeSorted( ListBuffer(1, 3, 5, 7), ListBuffer(2, 4, 6, 8) )
    assert(result == ListBuffer(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("BackwardsList"){
    val result = MergeSortedCollections.mergeSorted( BackwardsList(1,3,5,7), BackwardsList(2,4,6,8) )
    assert(result == BackwardsList(1,2,3,4,5,6,7,8))
  }

  test("Option") {
    val result = MergeSortedCollections.mergeSorted( Option(1), Option(2) )
    assert(result.contains(1))
  }
}


