package problem1

import org.scalatest.funsuite.AnyFunSuite

import MergeSortedIntList._
class MergeSortedIntListsSpec extends AnyFunSuite{


  test("Same length"){
    assert( (List(1,3,5,7), List(2,4,6,8)).merge == List(1, 2, 3, 4, 5, 6, 7, 8) )
  }

  test("Different length"){
    assert( (List(1,3), List(0,2,4,8)).merge == List(0, 1, 2, 3, 4, 8))
  }

  test("Duplicated numbers") {
    assert((List(1, 3), List(0, 2, 3, 4, 8)).merge == List(0, 1, 2, 3, 3, 4, 8))
  }

  test("Empty lists"){
    assert( (List.empty, List.empty).merge == List.empty)
  }


}
