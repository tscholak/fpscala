package com.github.tscholak.fpscala

import Chapter6State._

object Chapter6Machine {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  // introduce vending machine type constructor with covariant subtyping
  sealed trait VendingMachine[+A] {
    def dispense(i: Input): (A, VendingMachine[A])
  }

  // `Comm` for commodity
  type Comm[C, D] = State[VendingMachine[C], D]

  def comm[C](i: Input): Comm[C, C] = State(_.dispense(i))

  def commSeq[C](inputs: List[Input]): Comm[C, List[C]] =
    State.sequence(inputs.map(comm[C]))

  case class CandyMachine(locked: Boolean, candies: Int, coins: Int) extends VendingMachine[(Int, Int)] {

    def dispense(i: Input): ((Int, Int), VendingMachine[(Int, Int)]) = (i, this) match {
      case (Coin, CandyMachine(true, candy, coin)) if candy > 0 =>
        ((0, 0), CandyMachine(locked = false, candy, coin + 1))
      case (Turn, CandyMachine(false, candy, coin)) =>
        ((1, 0), CandyMachine(locked = true, candy - 1, coin))
      case (Coin, CandyMachine(_, _, _)) => ((0, 1), this)
      case (Turn, CandyMachine(_, _, _)) => ((0, 0), this)
    }

  }

  object CandyMachine {

    def candy(i: Input) = comm[(Int, Int)](i)

    def candySec(inputs: List[Input]) = commSeq[(Int, Int)](inputs)

    def candies(inputs: List[Input]) =
      candySec(inputs).map(_.foldRight((0, 0))((x, y) => (x._1 + y._1, x._2 + y._2)))

  }

}