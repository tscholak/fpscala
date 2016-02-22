package com.github.tscholak.fpscala

import Chapter6State._

object Chapter6Machine {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Machine {

    def stepMachine(i: Input, m: Machine): Machine = (i, m) match {
      case (Coin, Machine(true, candies, coins)) if candies > 0 => Machine(false, candies, coins+1)
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies-1, coins)
      case (_, Machine(_, _, _)) => m
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      // produce list of state updates of type `List[State[Machine, Unit]]`
      val ls = inputs.map(i => State.modify((m: Machine) => stepMachine(i, m)))
      for {
        // combine all the transitions in the list `ls` into a single transition, and discard the output (`List[Unit]`)
        _ <- State.sequence(ls)
        // retrieve current state and assign to `s`
        s <- State.get
      } yield (s.coins, s.candies)
    }

  }

}