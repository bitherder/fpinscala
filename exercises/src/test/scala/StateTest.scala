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

  describe("CandyMachine") {
    describe("simulateMachine") {
      describe("with candy") {
        describe("and locked") {
          describe("when a coin is entered") {
            val newState = CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = true, candies = 1, coins = 0))
            val newMachine = newState._2

            it("is unlocked by a coin") {
              assert(!newMachine.locked)
            }
            it("has one more coin") {
              assert(newMachine.coins == 1)
            }
            it("has the same number of candies") {
              assert(newMachine.candies == 1)
            }
          }

          describe("when turned") {
            val newState = CandyMachine.simulateMachine(List(Turn)).run(Machine(locked = true, candies = 1, coins = 0))
            val newMachine = newState._2

            it("is locked") {
              assert(newMachine.locked)
            }
            it("has the same number of coins") {
              assert(newMachine.coins == 0)
            }
            it("has the same number of candies") {
              assert(newMachine.candies == 1)
            }
          }
        }

        describe("and unlocked") {
          describe("when a coin is entered") {
            val newState = CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = false, candies = 1, coins = 0))
            val newMachine = newState._2

            it("is unlocked by a coin") {
              assert(!newMachine.locked)
            }
            it("has the same number of coins") {
              assert(newMachine.coins == 0)
            }
            it("has the same number of candies") {
              assert(newMachine.candies == 1)
            }
          }

          describe("when turned") {
            val newState = CandyMachine.simulateMachine(List(Turn)).run(Machine(locked = false, candies = 1, coins = 0))
            val newMachine = newState._2

            it("is locked") {
              assert(newMachine.locked)
            }
            it("has the same number of coins") {
              assert(newMachine.coins == 0)
            }
            it("has one less candy") {
              assert(newMachine.candies == 0)
            }
          }
        }
      }

      describe("without candy") {
        describe("and locked") {
          describe("when a coin is entered") {
            val newState = CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = true, candies = 0, coins = 0))
            val newMachine = newState._2

            it("remains locked") {
              assert(newMachine.locked)
            }
            it("has the same number of coins") {
              assert(newMachine.coins == 0)
            }
            it("has the same number of candies") {
              assert(newMachine.candies == 0)
            }
          }

          describe("when turned") {
            val newState = CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = true, candies = 0, coins = 0))
            val newMachine = newState._2

            it("remains locked") {
              assert(newMachine.locked)
            }
            it("has the same number of coins") {
              assert(newMachine.coins == 0)
            }
            it("has the same number of candies") {
              assert(newMachine.candies == 0)
            }
          }
        }

        describe("and unlocked") {
          describe("when a coin is entered") {
            val newState = CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = false, candies = 0, coins = 0))
            val newMachine = newState._2

            it("remains unlocked") {
              assert(!newMachine.locked)
            }
            it("has same number of coins") {
              assert(newMachine.coins == 0)
            }
            it("has same number of candies") {
              assert(newMachine.candies == 0)
            }
          }

          describe("when turned") {
            val newState = CandyMachine.simulateMachine(List(Turn)).run(Machine(locked = false, candies = 0, coins = 0))
            val newMachine = newState._2
            it("remains unlocked") {
              assert(!newMachine.locked)
            }
            it("has no more coins") {
              assert(newMachine.coins == 0)
            }
            it("has same number of candies") {
              assert(newMachine.candies == 0)
            }
          }
        }
      }
    }
  }
}


