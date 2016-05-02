package com.github.tscholak.fpscala.chapter5

import org.scalatest.{Matchers, FlatSpec}

class StreamSpec extends FlatSpec with Matchers {

  // exercise 5.1
  "A Stream(1,2,3)" should "be a List(1,2,3)" in {
    Stream(1, 2, 3).toList should be (List(1, 2, 3))
  }

  // exercise 5.2
  "A Stream(1,2,3) when 0 elements are taken" should "be an empty Stream" in {
    Stream(1, 2, 3).take2(0) should be (Empty)
  }
  "A Stream(1,2,3) when 2 elements are taken" should "be a Stream(1,2)" in {
    Stream(1, 2, 3).take2(2).toList should be (List(1, 2))
  }
  "A Stream(1,2,3) when 100 elements are taken" should "be a Stream(1,2,3)" in {
    Stream(1, 2, 3).take2(100).toList should be (List(1, 2, 3))
  }

  "A Stream of 1,2,3,4,5 when reversed" should "be Stream(5,4,3,2,1)" in {
    Stream(1, 2, 3, 4, 5).reverse.toList should be (List(5, 4, 3, 2, 1))
  }
  "A Stream of 1 when reversed" should "be Stream(1)" in {
    Stream(1).reverse.toList should be (List(1))
  }
  "An empty Stream when reversed" should "be an empty Stream" in {
    Empty.reverse should be (Empty)
  }

  "A Stream of 1,2,3,4,5,6 when 2 elements are dropped" should "be Stream(3,4,5,6)" in {
    Stream(1, 2, 3, 4, 5, 6).drop(2).toList should be (List(3, 4, 5, 6))
  }
  "A Stream of 1,2,3,4,5,6 when 32 elements are dropped" should "be Empty" in {
    Stream(1, 2, 3, 4, 5, 6).drop(32) should be (Empty)
  }
  "A Stream of 1,2,3,4,5,6 when 0 elements are dropped" should "be Stream(1,2,3,4,5,6)" in {
    Stream(1, 2, 3, 4, 5, 6).drop(0).toList should be (List(1, 2, 3, 4, 5, 6))
  }
  "A Stream of 1,2,3,4,5,6 when -1 elements are dropped" should "be Stream(1,2,3,4,5,6)" in {
    Stream(1, 2, 3, 4, 5, 6).drop(-1).toList should be (List(1, 2, 3, 4, 5, 6))
  }
  "A Stream of Empty when 1 element is dropped" should "be Empty" in {
    Empty.drop(1) should be (Empty)
  }

  // exercise 5.3
  "A Stream of 1,2,3,4,5" should "be Stream(1) if we take elements mod 2 != 0" in {
    Stream(1, 2, 3, 4, 5).takeWhile((x: Int) => x % 2 != 0).toList should be (List(1))
  }
  "A Stream of 1,2,3,4,5" should "be Stream(1,2) if we take elements < 3" in {
    Stream(1, 2, 3, 4, 5).takeWhile((x: Int) => x < 3).toList should be (List(1, 2))
  }
  "A Stream of 1,2,3,4,5" should "be Empty if we take elements > 3" in {
    Stream(1, 2, 3, 4, 5).takeWhile((x: Int) => x > 3) should be (Empty)
  }
  "A Stream of 1,2,3,4,5" should "be Stream(1,2,3) if we drop elements < 4" in {
    Stream(1, 2, 3, 4, 5).takeWhile((x: Int) => x < 4).toList should be (List(1, 2, 3))
  }

  "A Stream of 1,2,3,4,5" should "be Stream(2,3,4,5) if we drop elements mod 2 != 0" in {
    Stream(1, 2, 3, 4, 5).dropWhile((x: Int) => x % 2 != 0).toList should be (List(2, 3, 4, 5))
  }
  "A Stream of 1,2,3,4,5" should "be Stream(3,4,5) if we drop elements < 3" in {
    Stream(1, 2, 3, 4, 5).dropWhile((x: Int) => x < 3).toList should be (List(3, 4, 5))
  }
  "A Stream of 1,2,3,4,5" should "be Stream(1,2,3) if we drop elements > 3" in {
    Stream(1, 2, 3, 4, 5).dropWhile((x: Int) => x > 3).toList should be (List(1, 2, 3, 4, 5))
  }
  "A Stream of 1,2,3,4,5" should "be Empty if we drop elements < 6" in {
    Stream(1, 2, 3, 4, 5).dropWhile((x: Int) => x < 6) should be (Empty)
  }

  // exercise 5.4
  "A Stream of 1,3,5,-1,7,11" should "contain only odd numbers" in {
    Stream(1, 3, 5, -1, 7, 11).forAll((x: Int) => x % 2 != 0) should be (true)
  }
  "A Stream of 1,3,5,-1,7,0" should "not contain only odd numbers" in {
    Stream(1, 3, 5, -1, 7, 0).forAll((x: Int) => x % 2 != 0) should be (false)
  }
  "An empty Stream" should "contain only odd numbers" in {
    (Empty: Stream[Int]).forAll((x: Int) => x % 2 != 0) should be (true)
  }

  // exercise 5.5
  "A Stream of 4,2,0,-1,2" should "be Stream(4,2,0) if we take elements while mod 2 == 0" in {
    Stream(4, 2, 0, -1, 2).takeWhile2((x: Int) => x % 2 == 0).toList should be (List(4, 2, 0))
  }
  "A Stream of 1,2,3,4" should "be Stream(1,2) if we take elements while < 3" in {
    Stream(1, 2, 3, 4).takeWhile2((x: Int) => x < 3).toList should be (List(1, 2))
  }
  "A Stream of 1,5" should "be Empty if we take elements while > 3" in {
    Stream(1, 5).takeWhile2((x: Int) => x > 3) should be (Empty)
  }
  "A Stream of 1,2,3,4,5" should "be Stream(1,2,3) if we take elements while < 4" in {
    Stream(1, 2, 3, 4, 5).takeWhile2((x: Int) => x < 4).toList should be (List(1, 2, 3))
  }

  // exercise 5.6
  "A Stream of 1,2,3" should "transform into Some(1) when the first element is selected optionally" in {
    Stream(1, 2, 3).headOption2 should be (Some(1))
  }
  "An empty stream" should "transform into None when the first element is selected optionally" in {
    Empty.headOption2 should be (None)
  }

  // exercise 5.7
  "A Stream of 1,2,3,4,5" should "be Stream(2,4,6,8,10) when each element is multiplied by two" in {
    Stream(1, 2, 3, 4, 5).map((x: Int) => 2 * x).toList should be (List(2, 4, 6, 8, 10))
  }

  "A Stream of 1,2,3,4,5" should "be Stream(2,4) if we only keep all elements mod 2 == 0" in {
    Stream(1, 2, 3, 4, 5).filter((x: Int) => x % 2 == 0).toList should be (List(2, 4))
  }
  "A Stream of 1,2,3,4,5,2,2,1" should "be Stream(3,4,5) if we only keep elements < 3" in {
    Stream(1, 2, 3, 4, 5, 2, 2, 1).filter((x: Int) => x < 3).toList should be (List(1, 2, 2, 2, 1))
  }
  "A Stream of 1,2,3,4,5" should "be Stream(4,5) if we only keep elements > 3" in {
    Stream(1, 2, 3, 4, 5, 1).filter((x: Int) => x > 3).toList should be (List(4, 5))
  }
  "A Stream of 1,2,3,4,5" should "be Empty if we only keep elements > 6" in {
    Stream(1, 2, 3, 4, 5).filter((x: Int) => x > 6) should be (Empty)
  }

  "A Stream of 1,2,3 when appended a Stream(3,4,5)" should "be Stream(1,2,3,3,4,5)" in {
    Stream(1, 2, 3).append(Stream(3, 4, 5)).toList should be (List(1, 2, 3, 3, 4, 5))
  }
  "An empty Stream when appended an empty Stream" should "be an empty Stream" in {
    Empty.append(Empty) should be (Empty)
  }
  "An empty Stream when appended a Stream(1)" should "be a Stream(1)" in {
    Empty.append(Stream(1)).toList should be (List(1))
  }
  "A Stream of 1 when appended an empty Stream" should "be a Stream(1)" in {
    Stream(1).append(Empty).toList should be (List(1))
  }

  "Stream(Stream(1,2), Stream(1), Empty, Stream(1,2,3)) when flattened" should "be Stream(1,2,1,1,2,3)" in {
    Stream(Stream(1, 2), Stream(1), Empty, Stream(1, 2, 3)).flatten.toList should be (List(1, 2, 1, 1, 2, 3))
  }

  "A Stream of 1,2,3" should "be a Stream(1,1,2,2,3,3) when every element is repeated" in {
    Stream(1, 2, 3).flatMap((x: Int) => Stream(x, x)).toList should be (List(1, 1, 2, 2, 3, 3))
  }

  "A Stream of 1,2,3,4" should "be a List(12,14) when transformed as in Listing 5.3" in {
    Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList should be (List(12, 14))
  }

  // exercise 5.8
  "An infinite Stream of ones" should "be a Stream(1,1,1,1,1) when truncated after 5 elements" in {
    Stream.constant(1).take(5).toList should be (List(1,1,1,1,1))
  }
  "An infinite Stream of ones" should "return false if tested for containing only values != 1" in {
    Stream.constant(1).forAll(_ != 1) should be (false)
  }

  // exercise 5.9
  "An infinite Stream of 10,11,12,..." should "be a Stream(10,11,12,13,14) when truncated after 5 elements" in {
    Stream.from(10).take(5).toList should be (List(10,11,12,13,14))
  }

  // exercise 5.10
  "An infinite Stream of Fibonacci numbers" should "begin as Stream(0,1,1,2,3,5,8,13,21,34,55,89,144)" in {
    Stream.fibs.take(13).toList should be (List(0,1,1,2,3,5,8,13,21,34,55,89,144))
  }

  // exercise 5.11
  "An infinite Stream of Fibonacci numbers" should "begin as Stream(0,1,1,2,3,5,8,13,21,34,55,89)" in {
    Stream.fibs2.take(12).toList should be (List(0,1,1,2,3,5,8,13,21,34,55,89))
  }

  "An infinite Stream of 9,10,11,..." should "be a Stream(9,10,11,12,13) when truncated after 5 elements" in {
    Stream.from2(9).take(5).toList should be (List(9,10,11,12,13))
  }

  "An infinite Stream of twos" should "be a Stream(2,2,2,2,2) when truncated after 5 elements" in {
    Stream.constant2(2).take(5).toList should be (List(2,2,2,2,2))
  }

  // exercise 5.13
  "A Stream of 1,2,3,4,5" should "be Stream(1,4,9,16,25) when each element is squared" in {
    Stream(1, 2, 3, 4, 5).map2((x: Int) => x * x).toList should be (List(1, 4, 9, 16, 25))
  }

  "An infinite Stream of Fibonacci numbers" should "begin as Stream(0)" in {
    Stream.fibs.take3(1).toList should be (List(0))
  }

  "An infinite Stream of Fibonacci numbers" should "be Stream(0,1,1,2,3,5,8) if only elements < 10 are taken" in {
    Stream.fibs.takeWhile3((x: Int) => x < 10).toList should be (List(0,1,1,2,3,5,8))
  }

  "An infinite Stream of Fibonacci numbers and an infinite Stream of ones when zipped" should "start as Stream((0,1),(1,1),(1,1),(2,1),(3,1))" in {
    Stream.fibs.zip(Stream.ones).take(5).toList should be (List((0,1),(1,1),(1,1),(2,1),(3,1)))
  }

  // exercise 5.14
  "The statement that the infinite Stream of Fibonacci numbers starts with Stream(0,1,1,2,3,5,8)" should "be true" in {
    Stream.fibs.startsWith(Stream(0,1,1,2,3,5,8)) should be (true)
  }

  // exercise 5.15
  "Given the Stream of 1,2,3, the return value of tails" should "be Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())" in {
    Stream(1,2,3).tails.map(s => s.toList).toList should be (List(List(1,2,3), List(2,3), List(3), List()))
  }

  // exercise 5.16
  "A Stream of 1,2,3 when scanned from the right and added up" should "be Stream(1+2+3+0, 2+3+0, 3+0, 0)" in {
    Stream(1,2,3).scanRight2(0)(_ + _).toList should be (List(6,5,3,0))
  }
}