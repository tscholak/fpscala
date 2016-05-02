package com.github.tscholak.fpscala.chapter2

import scala.annotation.tailrec

object Misc {

  // exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n <= 0) prev
      else if (n == 1) cur
      else go(n-1, cur, prev+cur)

    go(n, 0, 1)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(as: Array[A]): Boolean = {
      if (as.length > 1) {
        if (ordered(as(0), as(1))) go(as.drop(1))
        else false
      }
      else true
    }

    go(as)
  }

  // exercise 2.3
  def partial[A,B,C](a: A, f: (A, B) => C): (B => C) = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): ((A, B) => C) = {
    (a: A, b: B) => f(a)(b)
  }

  // exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): (A => C) = {
    (a: A) => f(g(a))
  }

}