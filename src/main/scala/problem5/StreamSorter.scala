package problem5

import cats.effect._
import cats.effect.std.Queue
import fs2.Stream
import problem4.StreamSortMerge

import scala.concurrent.duration._

object StreamSorter {
  def streamSorter[A: Ordering](
                       queue1: Queue[IO, A],
                       queue2: Queue[IO, A],
                       takeN: Int
                     ): IO[List[A]] = {
    val t1 = takeN / 2
    val t2 = takeN - t1

    StreamSortMerge.sortMerge(
      List(
        Stream.fromQueueUnterminated(queue1).take(t1),
        Stream.fromQueueUnterminated(queue2).take(t2)
      )
    )
    .compile.toList
  }
}
