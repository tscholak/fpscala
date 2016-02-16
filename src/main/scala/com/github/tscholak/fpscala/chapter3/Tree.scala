package com.github.tscholak.fpscala

object Chapter3Tree {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    // exercise 3.25
    def size[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }

    // exercise 3.26
    def maximum(t: Tree[Int]): Int = t match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(v) => v
    }

    // exercise 3.27
    def depth[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + (depth(l) max depth(r))
      case Leaf(_) => 0
    }

    // exercise 3.28
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }

    // exercise 3.29
    def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B = t match {
      case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
      case Leaf(v) => z(v)
    }

    def size2[A](t: Tree[A]): Int = fold(t)((a: A) => 1)(1 + _ + _)

    def maximum2(t: Tree[Int]): Int = fold(t)((a: Int) => a)((a: Int, b: Int) => a max b)

    def depth2[A](t: Tree[A]): Int = fold(t)((a: A) => 0)((a: Int, b: Int) => 1 + (a max b))

    // note the type annotation on the expression `Leaf(f(a)): Tree[B]`
    def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)((a: A) => Leaf(f(a)): Tree[B])((l: Tree[B], r: Tree[B]) => Branch(l, r))

  }

}