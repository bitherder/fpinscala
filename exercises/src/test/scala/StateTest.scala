/**
  * Created by larry on 2017-04-27.
  */

import fpinscala.state._
import org.scalacheck.Arbitrary._
import org.scalatest._
import org.scalatest.prop._


class StateTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  def assertIntValue(i: Int): Unit = {
    assert(i >= Int.MinValue)
    assert(i <= Int.MaxValue)
  }

  def assertIntValue(i: Long): Unit = {
    assert(i >= Long.MinValue)
    assert(i <= Long.MaxValue)
  }

  describe("RNG.Simple()") {
    it("has a seed"){
      forAll { l: Long => {
        assertIntValue(RNG.Simple(l).seed)
      }}
    }
  }

  describe("A RNG.Simple") {
    it("is a rand int with another RNG") {
      forAll { l: Long => { assertIntValue(RNG.Simple(l).nextInt._1) }}
    }
  }

  describe("RNG.ints") {
    it("has a list of integers") {
      val ints = RNG.ints(3)(RNG.Simple(44))
      assert(ints._1.length == 3)
    }
  }

  describe("nonNegativeIntLessThan") {
    it("is less than limit") {
      forAll { (l: Long, i: Int) => whenever(i > 1) { assert(RNG.nonNegativeIntLessThan(i)(RNG.Simple(l))._1 < i) }}
    }

    it("is non negative") {
      forAll { (l: Long, i: Int) => whenever(i > 1) { assert(RNG.nonNegativeIntLessThan(i)(RNG.Simple(l))._1 >= 0) }}
    }
  }

  describe("nonNegativeInt") {
    it("is less than max integer value") {
      forAll { l: Long => { assert(RNG.nonNegativeInt(RNG.Simple(l))._1 < Int.MaxValue) }}
    }

    it("is non negative") {
      forAll { l: Long => { assert(RNG.nonNegativeInt(RNG.Simple(l))._1 >= 0) }}
    }
  }

}


