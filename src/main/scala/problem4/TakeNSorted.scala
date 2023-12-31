package problem4

import cats.effect.IO
import fs2.Stream

object TakeNSorted {




  def takeNSorted[A](
                   initial: A, //Starting generation on `initial` value
                   producer1: A => IO[Option[A]],
                   takeN: Int
                 ): IO[List[A]] = {
    def takeNrec(current: A, acc: List[A], remaining: Int): IO[List[A]] = {
      if(remaining <= 0){
        IO.pure(acc)
      }else{
        producer1(current).flatMap {
            case Some(value) => takeNrec(value, value :: acc, remaining - 1)
            case None => takeNrec( current, acc, 0)
          }
      }
    }
    takeNrec(initial, List.empty, takeN)
  }





  def takeNSorted[A: Ordering](
                      initial: A, //Starting generation on `initial` value
                      producer1: A => IO[Option[A]],
                      producer2: A => IO[Option[A]],
                      takeN: Int
                    ): IO[List[A]] = {
    //https://gist.github.com/gatorcse/1f92aa7e52a04d9c91511ca79f73e911
    val resultStream: Stream[IO, A] = StreamSortMerge.sortMerge(
      List(
        buildStream(initial, producer1),
        buildStream(initial, producer2)
      )
    )

    resultStream.take(takeN).compile.toList
  }

  def buildStream[A](
                      initial: A,
                      producer1: A => IO[Option[A]]
                    ): Stream[IO, A] = {

    def generateStream(current: A): Stream[IO, A] = {
      val streamElement = Stream.eval(producer1(current))
      streamElement.flatMap {
        case Some(nextValue) =>
          Stream.emit(nextValue) ++ generateStream(nextValue)
        case None =>
          Stream.empty
      }
    }

    Stream.emit(initial) ++ generateStream(initial)
  }


}

object StreamSortMerge {

  import fs2.{ Stream, Pull}
  import cats.collections.Heap

  import cats.implicits._

  //https://gist.github.com/johnynek/689199b4ac49364e7c94abef996ae59f
  def sortMerge[F[_], A: Ordering](streams: List[Stream[F, A]]): Stream[F, A] = {
    implicit val ord: cats.Order[Stream.StepLeg[F, A]] =
      new cats.Order[Stream.StepLeg[F, A]] {
        val ordA = implicitly[Ordering[A]]

        def compare(left: Stream.StepLeg[F, A], right: Stream.StepLeg[F, A]): Int = {
          if (left.head.isEmpty) {
            // prefer to step so we don't skip items
            if (right.head.isEmpty) 0 else -1
          }
          else if (right.head.isEmpty) {
            // we need to step so we don't misorder items
            1
          }
          else {
            // neither are empty just compare the head
            ordA.compare(left.head(0), right.head(0))
          }
        }
      }

    def go(heap: Heap[Stream.StepLeg[F, A]]): Pull[F, A, Unit] =
      heap.pop match {
        case Some((sl, rest)) =>
          if (sl.head.nonEmpty) {
            Pull.output1(sl.head(0)) >> {
              val nextSl = sl.setHead(sl.head.drop(1))
              val nextHeap = rest.add(nextSl)
              go(nextHeap)
            }
          }
          else {
            // this chunk is done
            sl.stepLeg
              .flatMap {
                case Some(nextSl) =>
                  val nextHeap = rest.add(nextSl)
                  go(nextHeap)
                case None =>
                  // this leg is exhausted
                  go(rest)
              }
          }

        case None => Pull.done
      }

    def heapOf(ls: List[Stream.StepLeg[F, A]]): Heap[Stream.StepLeg[F, A]] =
      Heap.fromIterable(ls)

    val heap: Pull[F, fs2.INothing, Heap[Stream.StepLeg[F, A]]] =
      streams
        .traverse(_.pull.stepLeg)
        .map { ls => heapOf(ls.flatten) }

    heap.flatMap(go).stream
  }
}
