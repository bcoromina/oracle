package problem5

import cats.effect._
import cats.effect.std.Queue
import fs2.Stream
import problem4.SortMerge

import scala.concurrent.duration._

object StreamSorter {
  def streamSorter[A: Ordering](
                       queue1: Queue[IO, A],
                       queue2: Queue[IO, A],
                       takeN: Int
                     ): IO[List[A]] = {
    SortMerge.sortMerge(
      List(
        Stream.fromQueueUnterminated(queue1),
        Stream.fromQueueUnterminated(queue2)
      )
    ).take(takeN).compile.toList
  }
}
