package problem1

import scala.annotation.tailrec

object MergeSortedIntList {
  def mergeSortedIntLists(left: List[Int], right: List[Int]): List[Int] = {
    @tailrec
    def mergeRec(left: List[Int], right: List[Int], acc: List[Int]): List[Int] = {

      (left, right) match {
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) {
            mergeRec(leftTail, right, leftHead :: acc) //Append to beginning is O(1). O(1) is cheap but we are reverting the order
          } else {
            mergeRec(left, rightTail, rightHead :: acc) //Append to beginning is O(1)
          }

        case (Nil, _) =>
          acc.reverse ++ right // O(n) for reverse. O(n1 + n2) for concat. n1: elements of left list, elements on right list
        case (_, Nil) =>
          acc.reverse ++ left //  O(n) for reverse. O(n1 + n2) for concat. n1: elements of left list, elements on right list

      }
    }

    mergeRec(left, right, List.empty)
  }


  implicit class MergeListOps(lists: (List[Int], List[Int])){
    def merge: List[Int] = mergeSortedIntLists(lists._1, lists._2)
  }
}
