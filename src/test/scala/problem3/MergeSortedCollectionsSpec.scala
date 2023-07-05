package problem3

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.collection.{BuildFrom, mutable}

class MergeSortedCollectionsSpec extends AnyFunSuite{

  test("inmutable list"){

    val result = MergeSortedCollections.mergeSorted(
      List(1,3,5,7), List(2,4,6,8)
    )
    assert( result == List(1, 2, 3, 4, 5, 6, 7, 8) )
  }

  test("inmutable vector") {

    val result = MergeSortedCollections.mergeSorted(
      Vector(1, 3, 5, 7), Vector(2, 4, 6, 8)
    )
    assert(result == Vector(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("inmutable ListBuffer") {
    import scala.collection.mutable.ListBuffer
    val result = MergeSortedCollections.mergeSorted(
      ListBuffer(1, 3, 5, 7), ListBuffer(2, 4, 6, 8)
    )
    assert(result == ListBuffer(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("BackwardsList"){

    class BackwardsIterator[A](list: BackwardsList[A]) extends Iterator[A] {
      private var current: BackwardsList[A] = list

      def hasNext: Boolean = current != BWLNil

      def next(): A = current match {
        case BWLNil => throw new NoSuchElementException("No more elements")
        case BWLCons(head, tail) =>
          current = tail
          head
      }
    }
    sealed trait BackwardsList[+A] extends IterableOnce[A]
    case object BWLNil extends BackwardsList[Nothing] {
      override def iterator: Iterator[Nothing] = new Iterator[Nothing] {
        override def hasNext: Boolean = false

        override def next(): Nothing = ???
      }
    }
    case class BWLCons[A](last: A, init: BackwardsList[A])extends BackwardsList[A] {
      override def iterator: Iterator[A] = new BackwardsIterator[A](this)
    }


    class BWLBuildFrom[A] extends BuildFrom[BackwardsList[A], A, BackwardsList[A]] {
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
        new Builder[A, BackwardsList[A]] {
          private val elements = collection.mutable.ArrayBuffer[A]()
          override def clear(): Unit = elements.clear()

          override def result(): BackwardsList[A] = {

            @tailrec
            def resRec(curr: collection.mutable.ArrayBuffer[A], acc: BackwardsList[A]): BackwardsList[A] = {
              if(curr.length > 0)
                resRec( curr.tail, BWLCons(curr.head, acc))
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


    implicit val p = new BWLBuildFrom[Int]

    val left: BackwardsList[Int] = BWLCons(1, BWLCons(3, BWLCons(5, BWLCons(7, BWLNil))))
    val right: BackwardsList[Int] = BWLCons(2, BWLCons(4, BWLCons(6, BWLCons(8, BWLNil))))
    val result = MergeSortedCollections.mergeSorted(
      left,
      right,
    )

    val expected = BWLCons(1, BWLCons(2, BWLCons(3, BWLCons(4, BWLCons(5, BWLCons(6, BWLCons(7, BWLCons(8, BWLNil))))))))
    assert(result == expected)

  }

  test("Option") {


    class OptionBuildFrom[A] extends BuildFrom[Option[_], A, Option[A]]{
      override def fromSpecific(from: Option[_])(it: IterableOnce[A]): Option[A] = {
        val iterator = it.iterator
        if (Option(iterator.hasNext).isDefined) {
          Option(iterator.next())
        }else None
      }

      override def newBuilder(from: Option[_]): mutable.Builder[A, Option[A]] =
        new Builder[A, Option[A]] { self =>
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

    implicit val p = new OptionBuildFrom[Int]
    val result: Option[Int] = MergeSortedCollections.mergeSorted(
      Option(1), Option(2)
    )
    assert(result == Some(1))
  }
}


