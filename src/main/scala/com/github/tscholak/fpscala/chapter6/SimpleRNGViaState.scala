package com.github.tscholak.fpscala

import Chapter6State._

object Chapter6SimpleRNGViaState {

  sealed trait RNG {
    def nextInt: (Int, RNG)
  }

  // most important two lines:
  type Rand[A] = State[RNG, A]
  val int: Rand[Int] = State(_.nextInt)

  val nonNegativeInt: Rand[Int] = int.map(n => if (n < 0) -(n+1) else n)

  val nonNegativeEven: Rand[Int] = nonNegativeInt.map(n => n - n % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(m => {
      val mod = m % n
      if (m + (n-1) - mod >= 0)
        State.unit(mod)
      else
        nonNegativeLessThan(n)
    })

  val flipCoin: Rand[Boolean] = nonNegativeLessThan(2).map(_ == 0)

  val rollD4: Rand[Int] = nonNegativeLessThan(4).map(_ + 1)

  val rollD6: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)

  val rollD8: Rand[Int] = nonNegativeLessThan(8).map(_ + 1)

  val rollD20: Rand[Int] = nonNegativeLessThan(20).map(_ + 1)

  val double: Rand[Double] = nonNegativeInt.map(_.toDouble / (1.0 + Int.MaxValue.toDouble))

  def drawList[A](s: Rand[A], len: Int): Rand[List[A]] =
    State.sequence(List.fill(len)(s))

  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

      // the next state is an `RNG` instance created from `newSeed`
      val nextRNG = SimpleRNG(newSeed)

      // `>>>` is right binary shift with zero fill
      val n = (newSeed >>> 16).toInt

      // as specified in the trait, the return value is a tuple of a pseudo-random integer and an `RNG` instance
      (n, nextRNG)
    }

  }

}