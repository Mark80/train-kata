package kata

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

//  Given a string representation of a train, print an ASCII-art representation of this train.
//
//  H: locomotive <HHHH
//  P: passenger car |OOOO|
//  Cars are attached together by ::
//
//  HPP —> <HHHH::|OOOO|::|OOOO|

//  R: restaurant car |hThT|
//  HPRP —> <HHHH::|OOOO|::|hThT|::|OOOO|

//  H: locomotive attached at the end HHHH>
//  HPRPH —> <HHHH::|OOOO|::|hThT|::|OOOO|::HHHH>
//
//A car can be deatached from the head or the end of the train
//HPRPH —> <HHHH::|OOOO|::|hThT|::|OOOO|::HHHH>
//—> detachEnd —> <HHHH::|OOOO|::|hThT|::|OOOO|
//—> detachHead —> |OOOO|::|hThT|::|OOOO|
//
//  C: cargo car |____| (when empty) |^^^^| (when full)
//  HCCC —> <HHHH::|____|::|____|::|____|
//  —> fill —> <HHHH::|^^^^|::|____|::|____|
//  —> fill —> <HHHH::|^^^^|::|^^^^|::|____|
//  —> fill —> <HHHH::|^^^^|::|^^^^|::|^^^^|
//  —> fill —> error: cannot fill a full train


class Tests extends AnyWordSpec with Matchers {

  "TrainBuilder" should {

    "build a train with only a locomotive" in {
      val result = Train.build("H")
      result shouldBe new Train("<HHHH")
    }

    "build a train with only a locomotive and one passenger car" in {
      val result = Train.build("HP")
      result shouldBe new Train("<HHHH::|OOOO|")
    }

    "build a train with a locomotive and 3 passenger car" in {
      val result = Train.build("HPPP")
      result shouldBe new Train("<HHHH::|OOOO|::|OOOO|::|OOOO|")
    }

    "build a train with a locomotive and a restaurant car" in {
      val result = Train.build("HR")
      result shouldBe new Train("<HHHH::|hThT|")
    }

    "build a train with: locomotive passenger car restaurant and passenger car" in {
      val result = Train.build("HPRP")
      result shouldBe new Train("<HHHH::|OOOO|::|hThT|::|OOOO|")
    }

    "build a train with a locomotive at the end of the train" in {
      val result = Train.build("HH")
      result shouldBe new Train("<HHHH::HHHH>")
    }

    "build a train with a locomotive passengers cars and a reverse locomotive at the end" in {
      val result = Train.build("HPRPH")
      result shouldBe new Train("<HHHH::|OOOO|::|hThT|::|OOOO|::HHHH>")
    }

    "detachEnd" in {
      new Train("<HHHH::|OOOO|").detachEnd shouldBe new Train("<HHHH")
    }

    "detachHead" in {
      new Train("<HHHH::|OOOO|").detachHead shouldBe new Train("|OOOO|")
      new Train("<HHHH::|OOOO|::|hThT|::|OOOO|::HHHH>")
        .detachHead
        .detachEnd
        .detachEnd shouldBe new Train("|OOOO|::|hThT|")
    }

    "build a train with a locomotive and 3 cargo car" in {
      val result = Train.build("HCCC")
      result shouldBe new Train("<HHHH::|____|::|____|::|____|")
    }

    "fill" in {

      new Train("<HHHH::|____|::|____|::|____|")
        .fill shouldBe new Train("<HHHH::|^^^^|::|____|::|____|")

      new Train("<HHHH::|____|::|____|::|____|")
        .fill.fill shouldBe new Train("<HHHH::|^^^^|::|^^^^|::|____|")

      new Train("<HHHH::|____|::|____|::|____|")
        .fill.fill.fill shouldBe new Train("<HHHH::|^^^^|::|^^^^|::|^^^^|")
    }

  }

}


