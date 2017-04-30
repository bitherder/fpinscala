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

  describe("RNG") {
    describe(".Simple()") {
      it("has a seed"){
        forAll { l: Long => {
          assertIntValue(RNG.Simple(l).seed)
        }}
      }
    }

    describe(".Simple") {
      it("is a rand int with another RNG") {
        forAll { l: Long => { assertIntValue(RNG.Simple(l).nextInt._1) }}
      }
    }

    describe(".ints") {
      it("has a list of integers") {
        val ints = RNG.ints(3)(RNG.Simple(44))
        assert(ints._1.length == 3)
      }
    }

    describe(".nonNegativeIntLessThan") {
      it("is less than limit") {
        forAll { (l: Long, i: Int) => whenever(i > 1) { assert(RNG.nonNegativeIntLessThan(i)(RNG.Simple(l))._1 < i) }}
      }

      it("is non negative") {
        forAll { (l: Long, i: Int) => whenever(i > 1) { assert(RNG.nonNegativeIntLessThan(i)(RNG.Simple(l))._1 >= 0) }}
      }
    }

    describe(".nonNegativeInt") {
      it("is less than max integer value") {
        forAll { l: Long => { assert(RNG.nonNegativeInt(RNG.Simple(l))._1 < Int.MaxValue) }}
      }

      it("is non negative") {
        forAll { l: Long => { assert(RNG.nonNegativeInt(RNG.Simple(l))._1 >= 0) }}
      }
    }
  }

  describe("State") {
    describe(".sequence") {
      val element = State[RNG, Int](RNG.nonNegativeInt)
      val seq = State.sequence[RNG, Int](List.fill(10)(element)).run(RNG.Simple(44))

      it("has n elements") {
        assert(seq._1.length == 10)
      }

      it("has values less than or equal to Int.maxValue") {
        seq._1.foreach(i => assert(i < Int.MaxValue))
      }

      it("has positive values") {
        seq._1.foreach( i => assert(i > 0) )
      }
    }
  }
}


