package com.github.tscholak.fpscala

import org.scalatest.{Matchers, FlatSpec}
import Chapter6Machine._

class MachineSpec extends FlatSpec with Matchers {

  "When a coin is inserted into a locked candy machine with one piece of candy in it, the machine" should "become unlocked" in {
    CandyMachine.candy(Coin).run(CandyMachine(locked = true, 1, 0)) should be (((0, 0), CandyMachine(locked = false, 1, 1)))
  }

  "When a coin is inserted into a locked candy machine with no candy in it, the machine" should "return the coin and remain locked" in {
    CandyMachine.candy(Coin).run(CandyMachine(locked = true, 0, 0)) should be (((0, 1), CandyMachine(locked = true, 0, 0)))
  }

  "When a coin is inserted into an unlocked candy machine, the machine" should "return the coin and remain unlocked" in {
    CandyMachine.candy(Coin).run(CandyMachine(locked = false, 1, 1)) should be (((0, 1), CandyMachine(locked = false, 1, 1)))
  }

  "When the knob is turned on an unlocked candy machine, the machine" should "dispense a piece of candy and become locked" in {
    CandyMachine.candy(Turn).run(CandyMachine(locked = false, 1, 1)) should be (((1, 0), CandyMachine(locked = true, 0, 1)))
  }

  "When the knob is turned on a locked candy machine, the machine" should "remain locked and do nothing" in {
    CandyMachine.candy(Turn).run(CandyMachine(locked = true, 1, 0)) should be (((0, 0), CandyMachine(locked = true, 1, 0)))
  }

  "When a coin is inserted an then the knob is turned on a locked candy machine with one piece of candy in it, the machine" should "accept the coin, dispense the piece of candy, and be locked at the end" in {
    CandyMachine.candies(List(Coin,Turn)).run(CandyMachine(locked = true, 1, 0)) should be (((1, 0), CandyMachine(locked = true, 0, 1)))
  }

  "When the knob is turned and then a coin is inserted into a locked candy machine with one piece of candy in it, the machine" should "accept the coin, and be unlocked at the end" in {
    CandyMachine.candies(List(Turn,Coin)).run(CandyMachine(locked = true, 1, 0)) should be (((0, 0), CandyMachine(locked = false, 1, 1)))
  }

  "The Sequence of Coin, Turn, Turn, Coin, Coin, Turn, Coin, Turn on a locked candy machine with two pieces of candy in it" should "cause the machine to accept two coins, return two coins, dispense both pieces of candy, and be locked at the end" in {
    CandyMachine.candies(List(Coin, Turn, Turn, Coin, Coin, Turn, Coin, Turn)).run(CandyMachine(locked = true, 2, 0)) should be (((2, 2), CandyMachine(locked = true, 0, 2)))
  }

}