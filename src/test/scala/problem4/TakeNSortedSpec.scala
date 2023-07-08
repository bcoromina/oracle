package problem4

import cats.effect.kernel.Resource.Pure
import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.util.Random
class TakeNSortedSpec extends AnyFunSuite{
    test("takeNSorted function"){

      val num = Ref[IO].of(0).unsafeRunSync()

      def generateNextLong(previous: Long): IO[Option[Long]] = {
        for{
          n <- num.get
          res <- IO(Random.nextInt().abs % 100).map(v => Option(v.toLong + previous))
          _ <- num.set(n + 1)
        }yield res
      }

      val take = 10
      val result = TakeNSorted.takeNSorted(1.toLong, generateNextLong,generateNextLong, take ).unsafeRunSync()
      assert(result.length == take)

      //The initial value is provided by us, not computed by the function
      assert(num.get.unsafeRunSync() == take - 1 )
      val res = firstDisorderPos(result)
      assert(res.isEmpty)
      assert(result == result.sorted)
    }


  test("takeNSorted function. One generatorn returns None") {

    def generateNextLong(previous: Long): IO[Option[Long]] = IO.pure(None)

    def generateNextLong2(previous: Long): IO[Option[Long]] = IO.pure(Option(1 + previous))

    val take = 10
    val expectedResult = List(1,1,2,3,4,5,6,7,8,9)
    val result = TakeNSorted.takeNSorted(1.toLong, generateNextLong, generateNextLong2, take).unsafeRunSync()
    assert(result.length == take)
    assert(result == expectedResult)
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
