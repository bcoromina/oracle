package problem2

import scala.annotation.tailrec

object MergeSortedList {
  def mergeSortedLists[A](left: List[A], right: List[A])(implicit ord: Ordering[A]): List[A] = {
    @tailrec
    def mergeRec(left: List[A], right: List[A], acc: List[A]): List[A] = {
      (left, right) match {
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if( ord.compare(leftHead, rightHead) < 0) {
              mergeRec(leftTail, right, leftHead :: acc) //Append to beginning is O(1). O(1) is cheap but we are reverting the order
          }else {
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

  implicit class MergeListOps[T](lists: (List[T], List[T])) {
    def merge(implicit ord: Ordering[T]): List[T] = mergeSortedLists(lists._1, lists._2)
  }


}
