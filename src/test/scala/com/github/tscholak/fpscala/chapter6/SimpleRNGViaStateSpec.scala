package com.github.tscholak.fpscala

import org.scalatest.{Matchers, FlatSpec}
import Chapter6SimpleRNGViaState._

class SimpleRNGViaStateSpec extends FlatSpec with Matchers {

  "The next pseudo-random integer following a seed of 42" should "be 16159453" in {
    int.run(SimpleRNG(42))._1 should be (16159453)
  }

  "The next pseudo-random non-negative integer following a seed of 42" should "be 16159453" in {
    nonNegativeInt.run(SimpleRNG(42))._1 should be (16159453)
  }

  "The next pseudo-random non-negative even integer following a seed of 42" should "be 16159452" in {
    nonNegativeEven.run(SimpleRNG(42))._1 should be (16159452)
  }

  "The next pseudo-random non-negative integer less than 100 following a seed of 42" should "be 53" in {
    nonNegativeLessThan(100).run(SimpleRNG(42))._1 should be (53)
  }

  "The next pseudo-random coin flip following a seed of 42" should "be false" in {
    flipCoin.run(SimpleRNG(42))._1 should be (false)
  }

  "The next pseudo-random D4 roll following a seed of 42" should "be 2" in {
    rollD4.run(SimpleRNG(42))._1 should be (2)
  }

  "The next pseudo-random D6 roll following a seed of 42" should "be 2" in {
    rollD6.run(SimpleRNG(42))._1 should be (2)
  }

  "The next pseudo-random D8 roll following a seed of 42" should "be 6" in {
    rollD8.run(SimpleRNG(42))._1 should be (6)
  }

  "The next pseudo-random D20 roll following a seed of 42" should "be 14" in {
    rollD20.run(SimpleRNG(42))._1 should be (14)
  }

  "The next 10 pseudo-random D6 rolls following a seed of 42" should "be List(2,3,2,4,5,5,3,1,5,3)" in {
    drawList(rollD6, 10).run(SimpleRNG(42))._1 should be (List(2, 3, 2, 4, 5, 5, 3, 1, 5, 3))
  }

}