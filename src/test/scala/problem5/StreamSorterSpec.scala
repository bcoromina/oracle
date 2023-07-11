package problem5

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite


class StreamSorterSpec extends AnyFunSuite{

    test("sort from two queues"){
      val result = for{
        queue1 <- Queue.unbounded[IO, Int]
        queue2 <- Queue.unbounded[IO, Int]
        _ <- enqueueElements(queue1, List(1,3,5))
        _ <- enqueueElements(queue2, List(2,4,6))
        size1 <- queue1.size
        size2 <- queue2.size
        result <- StreamSorter.streamSorter(queue1, queue2, size1 + size2)
      }yield result

      assert(result.unsafeRunSync() == List(1,2,3,4,5,6))
    }

    def enqueueElements[A](queue: Queue[IO, A], values: List[A]): IO[Unit] =
      values.traverse(v => queue.offer(v)).void
}
