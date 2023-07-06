package problem4

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.util.Random
class TakeNSortedSpec extends AnyFunSuite{
    test("test"){

      def generateNextLong(previous: Long): IO[Option[Long]] =
        IO(Random.nextInt().abs % 100).map(v => Option(v.toLong + previous))

      val take = 10000
      val result = TakeNSorted.takeNSorted(1.toLong, generateNextLong,generateNextLong, take ).unsafeRunSync()
      assert(result.length == take)
      val res = firstDisorderPos(result)
      assert(res.isEmpty)
      assert(result == result.sorted)
    }



    private def firstDisorderPos[A](a: List[A])(implicit ord: Ordering[A]): Option[Int] = {

      @tailrec
      def checkRec(r: List[A]): Option[Int]=
      r match {
        case i::j::next=>
          if(ord.compare(i,j) > 0){
            Some(a.indexOf(i))
          }else{
            checkRec(j::next)
          }

        case _ => None
      }

      checkRec(a)
    }


}
