package problem3

import scala.annotation.tailrec
import scala.collection.{BuildFrom, mutable}


object MergeSortedCollections {

  def mergeSorted[F[A] <: IterableOnce[A], A](left: F[A], right: F[A])(implicit ord: Ordering[A], cbf: scala.collection.BuildFrom[F[A], A, F[A]]): F[A] = {
    val builder = cbf.newBuilder(left)
    val leftIterator = left.iterator
    val rightIterator = right.iterator

    var leftOption: Option[A] = leftIterator.nextElement
    var rightOption: Option[A] = rightIterator.nextElement

    while (leftOption.isDefined || rightOption.isDefined) {
      val minValueOption: Option[A] = (leftOption, rightOption) match {
        case (Some(leftValue), Some(rightValue)) =>
          if (ord.lt(leftValue, rightValue)) {
            leftOption = leftIterator.nextElement
            Some(leftValue)
          } else {
            rightOption = rightIterator.nextElement
            Some(rightValue)
          }
        case (Some(leftValue), None) =>
          leftOption = leftIterator.nextElement
          Some(leftValue)
        case (None, Some(rightValue)) =>
          rightOption = rightIterator.nextElement
          Some(rightValue)
        case (None, None) => None
      }

      minValueOption.foreach(builder += _)
    }

    builder.result()
  }

  implicit class IteratorOps[A](i: Iterator[A]) {
    def nextElement: Option[A] = if (i.hasNext) Some(i.next()) else None
  }
}


object SortedCollectionsBuilders{
  implicit def bwlBuildFrom[A] = new BuildFrom[BackwardsList[A], A, BackwardsList[A]] {
    override def fromSpecific(from: BackwardsList[A])(it: IterableOnce[A]): BackwardsList[A] = {

      @tailrec
      def resRec(ite: Iterator[A], acc: BackwardsList[A]): BackwardsList[A] = {
        if (ite.hasNext) {
          val next = ite.next()
          resRec(ite, BWLCons(next, acc))
        } else
          acc
      }

      resRec(it.iterator, BWLNil)
    }

    override def newBuilder(from: BackwardsList[A]): mutable.Builder[A, BackwardsList[A]] =
      new mutable.Builder[A, BackwardsList[A]] {
        private val elements = collection.mutable.ArrayBuffer[A]()

        override def clear(): Unit = elements.clear()

        override def result(): BackwardsList[A] = {

          @tailrec
          def resRec(curr: collection.mutable.ArrayBuffer[A], acc: BackwardsList[A]): BackwardsList[A] = {
            if (curr.length > 0)
              resRec(curr.tail, BWLCons(curr.head, acc))
            else
              acc
          }

          resRec(elements.reverse, BWLNil)

        }

        override def addOne(elem: A): this.type = {
          elements.addOne(elem)
          this
        }
      }
  }

  implicit def optionBuildFrom[A] = new BuildFrom[Option[_], A, Option[A]] {
    override def fromSpecific(from: Option[_])(it: IterableOnce[A]): Option[A] = {
      val iterator = it.iterator
      if (Option(iterator.hasNext).isDefined) {
        Option(iterator.next())
      } else None
    }

    override def newBuilder(from: Option[_]): mutable.Builder[A, Option[A]] =
      new mutable.Builder[A, Option[A]] {
        self =>
        private val elements = collection.mutable.ArrayBuffer[A]()

        def addOne(x: A): this.type = {
          elements.addOne(x)
          this
        }

        def clear(): Unit = elements.clear()

        override def addAll(xs: IterableOnce[A]): this.type = {
          val iterator = xs.iterator
          if (Option(iterator.hasNext).isDefined) {
            elements.addOne(iterator.next())
          }
          this
        }

        override def result(): Option[A] = elements.headOption
      }
  }

}