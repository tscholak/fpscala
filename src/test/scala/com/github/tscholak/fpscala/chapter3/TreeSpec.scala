package com.github.tscholak.fpscala.chapter3

import org.scalatest.{Matchers, FlatSpec}

class TreeSpec extends FlatSpec with Matchers {

  // exercise 3.25
  "A Tree of Leaf(1)" should "have a size of 1" in {
    Tree.size(Leaf(1)) should be(1)
  }
  "A Tree of Branch(Leaf(1), Leaf(1))" should "have a size of 3" in {
    Tree.size(Branch(Leaf(1), Leaf(1))) should be(3)
  }
  "A Tree of Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))" should "have a size of 5" in {
    Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) should be(5)
  }
  "A Tree of Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))" should "have a size of 7" in {
    Tree.size(Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))) should be(7)
  }

  // exercise 3.26
  "A Tree of Leaf(5)" should "have a maximum of 5" in {
    Tree.maximum(Leaf(5)) should be(5)
  }
  "A Tree of Branch(Leaf(1), Leaf(5))" should "have a maximum of 5" in {
    Tree.maximum(Branch(Leaf(1), Leaf(5))) should be(5)
  }
  "A Tree of Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))" should "have a maximum of 3" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(3)
  }
  "A Tree of Branch(Leaf(-1), Branch(Branch(Leaf(0), Leaf(100)), Leaf(2)))" should "have a maximum of 100" in {
    Tree.maximum(Branch(Leaf(-1), Branch(Branch(Leaf(0), Leaf(100)), Leaf(2)))) should be(100)
  }

  // exercise 3.26
  "A Tree of Leaf(1)" should "have a depth of 0" in {
    Tree.depth(Leaf(1)) should be(0)
  }
  "A Tree of Branch(Leaf(1), Leaf(1))" should "have a depth of 1" in {
    Tree.depth(Branch(Leaf(1), Leaf(1))) should be(1)
  }
  "A Tree of Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))" should "have a depth of 2" in {
    Tree.depth(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) should be(2)
  }
  "A Tree of Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))" should "have a depth of 3" in {
    Tree.depth(Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))) should be(3)
  }


}