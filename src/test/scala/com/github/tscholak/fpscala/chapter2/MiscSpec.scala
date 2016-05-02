package com.github.tscholak.fpscala.chapter2

import org.scalatest.{Matchers, FlatSpec}
import Misc._

class MiscSpec extends FlatSpec with Matchers {

  // exercise 2.1
  "The fifth Fibonacci number" should "be 3" in {
    fib(4) should be (3)
  }

  // exercise 2.2
  "An Array of 1,2,3,4,5" should "be sorted" in {
    isSorted(Array(1,2,3,4,5), (a: Int, b: Int) => a < b) should be (true)
  }
  "An Array of 1,2,3,5,4" should "not be sorted" in {
    isSorted(Array(1,2,3,5,4), (a: Int, b: Int) => a < b) should be (false)
  }

  // exercise 2.3
  "Partial function evaluation" should "work" in {
    partial(10, (a: Int, b: Int) => a + b)(20) should be(30)
  }

  "Currying" should "work" in {
    curry((a: Int, b: Int) => a + b)(10)(20) should be(30)
  }

  "Uncurrying" should "work" in {
    uncurry(curry((a: Int, b: Int) => a + b))(10, 20) should be (30)
  }

  // exercise 2.5
  "Function composition" should "work" in {
    compose((a: Int) => 2 * a, (b: Int) => b + 3)(1) should be (8)
  }
}