package problem3

import scala.annotation.tailrec
import scala.collection.{BuildFrom, mutable}

sealed trait BackwardsList[+A] extends IterableOnce[A]

case object BWLNil extends BackwardsList[Nothing] {
  override def iterator: Iterator[Nothing] = new Iterator[Nothing] {
    override def hasNext: Boolean = false
    override def next(): Nothing = throw new NoSuchElementException("No more elements")
  }
}

case class BWLCons[A](last: A, init: BackwardsList[A]) extends BackwardsList[A] {
  override def iterator: Iterator[A] = new BackwardsIterator[A](this)
}

object BackwardsList{
  def apply[T](elem: T*):BackwardsList[T] = {
    @tailrec
    def toBWLRec(rem: List[T], acc: BackwardsList[T]): BackwardsList[T] = rem match {
      case head :: next => toBWLRec(next, BWLCons(head, acc))
      case Nil => acc
    }

    toBWLRec( elem.reverse.toList, BWLNil)
  }
}

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



