package com.github.tscholak.fpscala

import scala.{Nil => _, List => _, _}
import org.scalatest.{Matchers, FlatSpec}
import Chapter3List._

class ListSpec extends FlatSpec with Matchers {

  // exercise 3.2
  "A List of 1,2,3" should "have a tail of 2,3" in {
    List.tail(List(1,2,3)) should be (List(2,3))
  }

  // exercise 3.3
  "A List of 1,2,3" should "transform to 0,2,3 when setHead is 0" in {
    List.setHead(0, List(1,2,3)) should be (List(0,2,3))
  }
  "A List of Nil" should "transform to 6 when setHead is 6" in {
    List.setHead(6, Nil) should be (List(6))
  }
  "A List of (4)" should "transform to 6 when setHead is 6" in {
    List.setHead(6, List(4)) should be (List(6))
  }

  // exercise 3.4
  "A List of 1,2,3,4,5,6 when 2 elements are dropped" should "be List(3,4,5,6)" in {
    List.drop(List(1,2,3,4,5,6), 2) should be (List(3,4,5,6))
  }
  "A List of 1,2,3,4,5,6 when 32 elements are dropped" should "be Nil" in {
    List.drop(List(1,2,3,4,5,6), 32) should be (Nil)
  }
  "A List of 1,2,3,4,5,6 when 0 elements are dropped" should "be List(1,2,3,4,5,6)" in {
    List.drop(List(1,2,3,4,5,6), 0) should be (List(1,2,3,4,5,6))
  }
  "A List of 1,2,3,4,5,6 when -1 elements are dropped" should "be List(1,2,3,4,5,6)" in {
    List.drop(List(1,2,3,4,5,6), -1) should be (List(1,2,3,4,5,6))
  }
  "A List of Nil when 1 element is dropped" should "be Nil" in {
    List.drop(Nil, 1) should be (Nil)
  }

  // exercise 3.5
  "A List of 1,2,3" should "be (2,3) if we drop elements mod 2 != 0" in {
    List.dropWhile(List(1,2,3), (x: Int) => x % 2 != 0) should be (List(2,3))
  }
  "A List of 1,2,3" should "be (3) if we drop elements < 3" in {
    List.dropWhile(List(1,2,3), (x: Int) => x < 3) should be (List(3))
  }
  "A List of 1,2,3" should "be (1,2,3) if we drop elements > 3" in {
    List.dropWhile(List(1,2,3), (x: Int) => x > 3) should be (List(1,2,3))
  }
  "A List of 1,2,3" should "be Nil if we drop elements < 4" in {
    List.dropWhile(List(1,2,3), (x: Int) => x < 4) should be (Nil)
  }

  // exercise 3.6
  "A List of 1,2,3,4" should "be 1,2,3 if call init" in {
    List.init(List(1,2,3,4)) should be (List(1,2,3))
  }
  "A List of 1" should "be Nil if call init" in {
    List.init(List(1)) should be (Nil)
  }
  "A List of Nil" should "be Nil if call init" in {
    List.init(Nil) should be (Nil)
  }

  // exercise 3.9
  "A List of 1,2,3,4" should "have length 4" in {
    List.length(List(1,2,3,4)) should be (4)
  }
  "A List of 1" should "have length 1" in {
    List.length(List(1)) should be (1)
  }
  "A List of Nil" should "have length 0" in {
    List.length(Nil) should be (0)
  }

  // exercise 3.10
  "A List of 1,2,3,4" should "have total sum 10" in {
    List.foldLeft(List(1,2,3,4), 0)(_ + _) should be (10)
  }

  // exercise 3.11
  "A List of 1,2,3,4" should "have sum 10" in {
    List.sum3(List(1,2,3,4)) should be (10)
  }
  "A List of 1.0,2.0,3.0,4.0" should "have product 24" in {
    List.product3(List(1.0,2.0,3.0,4.0)) should be (24)
  }
  "A List of 1,2,3,4,5" should "have length 5" in {
    List.length3(List(1,2,3,4,5)) should be (5)
  }

  // exercise 3.12
  "A List of 1,2,3,4,5 when reversed" should "be List(5,4,3,2,1)" in {
    List.reverse(List(1,2,3,4,5)) should be (List(5,4,3,2,1))
  }
  "A List of 1 when reversed" should "be List(1)" in {
    List.reverse(List(1)) should be (List(1))
  }
  "A List of Nil when reversed" should "be Nil" in {
    List.reverse(Nil) should be (Nil)
  }

  // exercise 3.13
  "A List of 1,2,3,4,5,6,7" should "have total sum 28" in {
    List.foldRight2(List(1,2,3,4,5,6,7), 0)(_ + _) should be (28)
  }

  // exercise 3.14
  "A List of 1,2,3 when appended a List of 4,5,6" should "be List(1,2,3,4,5,6)" in {
    List.append2(List(1,2,3), List(4,5,6)) should be (List(1,2,3,4,5,6))
  }
  "A List of 3,2,1 when appended a List of 6,5,4" should "be List(3,2,1,6,5,4)" in {
    List.append3(List(3,2,1), List(6,5,4)) should be (List(3,2,1,6,5,4))
  }

  // exercise 3.15
  "A List of List(1,2,3), Nil, List(4,5,6), and List(7) when flattened" should "be List(1,2,3,4,5,6,7)" in {
    List.flatten(List(List(1,2,3), List(4,5,6), List(7), Nil: List[Int])) should be (List(1,2,3,4,5,6,7))
  }

  // exercise 3.16
  "A List of 1,2,3" should "transform to List(2,3,4) when every element is increased by one" in {
    List.addOneEverywhere2(List(1,2,3)) should be (List(2,3,4))
  }

  // exercise 3.17
  "A List of 1.0,2e10,3.0" should "transform to List(1.0.toString,2e10.toString,3.0.toString) when every" +
    "element is converted to a String" in {
    List.everyValueToString(List(1.0,2e10,3.0)) should be (List(1.0.toString,2e10.toString,3.0.toString))
  }

  // exercise 3.19
  "A List of 1,2,3,4,5,6" should "transform to List(2,4,6) if all odd elements are removed" in {
    List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) should be (List(2,4,6))
  }
  "A List of 1,3,5" should "transform to Nil if all odd elements are removed" in {
    List.filter(List(1,3,5))(_ % 2 == 0) should be (Nil: List[Int])
  }
  "An empty List" should "transform to Nil if all odd elements are removed" in {
    List.filter(Nil: List[Int])(_ % 2 == 0) should be (Nil: List[Int])
  }

  // exercise 3.20
  "flatMap(List(1,2,3))(i => List(i,i))" should "result in List(1,1,2,2,3,3)" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  // exercise 3.21
  "A List of 1,2,3,4,5,6,7,7,7,7" should "transform to List(2,4,6) if all odd elements are removed" in {
    List.filter2(List(1,2,3,4,5,6,7,7,7,7))(_ % 2 == 0) should be (List(2,4,6))
  }

  // exercise 3.22
  "Two List of 1,3 and a List of 2,5,8" should "add up to List(3,8,8)" in {
    List.addLists(List(1,3), List(2,5,8)) should be (List(3,8,8))
  }

  // exercise 3.24
  "The statement that a List of 1,2,3 begins with the List of 1,2" should "evaluate to true" in {
    List.beginsWith(List(1,2,3), List(1,2)) should be (true)
  }
  "The statement that a List of 1 begins with the List of 1,2" should "evaluate to false" in {
    List.beginsWith(List(1), List(1,2)) should be (false)
  }
  "The statement that a List of 1,3,2 begins with the List of 1,2" should "evaluate to false" in {
    List.beginsWith(List(1,3,2), List(1,2)) should be (false)
  }
  "The statement that a List of 1,2 begins with Nil" should "evaluate to true" in {
    List.beginsWith(List(1,2), Nil) should be (true)
  }
  "The statement that a Nil begins with Nil" should "evaluate to true" in {
    List.beginsWith(Nil, Nil) should be (true)
  }
  "The statement that a List of 1,2,3 contains Nil" should "evaluate to true" in {
    List.hasSubsequence(List(1,2,3), Nil) should be (true)
  }
  "The statement that a List of 1,2,3 contains the List of 3" should "evaluate to true" in {
    List.hasSubsequence(List(1,2,3), List(2)) should be (true)
  }
  "The statement that a List of 1,2,3 contains the List of 1,3" should "evaluate to false" in {
    List.hasSubsequence(List(1,2,3), List(1,3)) should be (false)
  }
  "The statement that a List of 1,2,1,3 contains the List of 1,3" should "evaluate to true" in {
    List.hasSubsequence(List(1,2,1,3), List(1,3)) should be (true)
  }
  "The statement that Nil contains Nil" should "evaluate to true" in {
    List.hasSubsequence(Nil, Nil) should be (true)
  }
  "The statement that Nil contains the List of 1,2" should "evaluate to false" in {
    List.hasSubsequence(Nil, List(1,2)) should be (false)
  }
  "The statement that a List of 1,2,3,4 contains the List of 1,2" should "evaluate to true" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2)) should be (true)
  }
  "The statement that a List of 1,2,3,4 contains the List of 2,3" should "evaluate to true" in {
    List.hasSubsequence(List(1,2,3,4), List(2,3)) should be (true)
  }
  "The statement that a List of 1,2,3,4 contains the List of 4" should "evaluate to true" in {
    List.hasSubsequence(List(1,2,3,4), List(4)) should be (true)
  }

}