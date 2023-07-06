package problem3

import org.scalatest.funsuite.AnyFunSuite

class BackwardsListSpec extends AnyFunSuite{

  test("Smart constructor. Non empty List"){
    assert(BackwardsList(1,2,3) == BWLCons(1, BWLCons(2, BWLCons(3, BWLNil))))
  }

  test("Smart constructor. Empty List") {
    assert(BackwardsList() == BWLNil)
  }

  //TODO: test bwlBuildFrom
}
