package problem3


object MergeSortedCollections {


  //https://github.com/scala/docs.scala-lang/blob/main/_overviews/core/custom-collection-operations.md
  def mergeSorted[F[A] <: IterableOnce[A], A](left: F[A], right: F[A])(implicit ord: Ordering[A], cbf: scala.collection.BuildFrom[F[A], A, F[A]]): F[A] = {
    val builder = cbf.newBuilder(left)
    val leftIterator = left.iterator
    val rightIterator = right.iterator

    var leftOption: Option[A] = if (leftIterator.hasNext) Some(leftIterator.next()) else None
    var rightOption: Option[A] = if (rightIterator.hasNext) Some(rightIterator.next()) else None

    while (leftOption.isDefined || rightOption.isDefined) {
      val minValueOption: Option[A] = (leftOption, rightOption) match {
        case (Some(leftValue), Some(rightValue)) =>
          if (ord.lt(leftValue, rightValue)) {
            leftOption = if (leftIterator.hasNext) Some(leftIterator.next()) else None
            Some(leftValue)
          } else {
            rightOption = if (rightIterator.hasNext) Some(rightIterator.next()) else None
            Some(rightValue)
          }
        case (Some(leftValue), None) =>
          leftOption = if (leftIterator.hasNext) Some(leftIterator.next()) else None
          Some(leftValue)
        case (None, Some(rightValue)) =>
          rightOption = if (rightIterator.hasNext) Some(rightIterator.next()) else None
          Some(rightValue)
        case (None, None) => None
      }

      minValueOption.foreach(builder += _)
    }

    builder.result()
  }


}
