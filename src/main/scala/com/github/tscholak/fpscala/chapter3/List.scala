package com.github.tscholak.fpscala

import scala.annotation.tailrec

object Chapter3List {

  // `List` data type, parameterized on the type `A`
  sealed trait List[+A]

  // representation of an empty `List`
  case object Nil extends List[Nothing]

  // constructor for the `List` type
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // `List` companion object
  object List {

    // variadic apply function
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // exercise 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, t) => t
      case Nil => Nil
    }

    // exercise 3.3
    def setHead[A](nh: A, l: List[A]): List[A] = l match {
      case Cons(_, t) => Cons(nh, t)
      case Nil => List(nh)
    }

    // exercise 3.4
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n > 0)
        l match {
          case Cons(_, t) => drop(t, n - 1)
          case _ => Nil
        }
      else l
    }

    // exercise 3.5
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      case _ => Nil
    }

    // section 3.3.1
    //@tailrec
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    // exercise 3.6
    //@tailrec
    def init[A](l: List[A]): List[A] = l match {
      case Cons(h1, Cons(h2, t)) => Cons(h1, init(Cons(h2, t)))
      case _ => Nil
    }

    // section 3.3.2
    @tailrec
    def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

    // listing 3.2
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    //foldRight(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), z)(f)
    //f(1, foldRight(Cons(2, Cons(3, Cons(4, Nil))), z)(f))
    //f(1, f(2, foldRight(Cons(3, Cons(4, Nil)), z)(f)))
    //f(1, f(2, f(3, foldRight(Cons(4, Nil), z)(f))))
    //f(1, f(2, f(3, f(4, foldRight(Nil, z)(f)))))
    //f(1, f(2, f(3, f(4, z))))

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    // exercise 3.9
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, n: Int) => 1 + n)

    // exercise 3.10
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    //foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), z)(f)
    //foldLeft(Cons(2, Cons(3, Cons(4, Nil))), f(z, 1))(f)
    //foldLeft(Cons(3, Cons(4, Nil)), f(f(z, 1), 2))(f)
    //foldLeft(Cons(4, Nil), f(f(f(z, 1), 2), 3))(f)
    //foldLeft(Nil, f(f(f(f(z, 1), 2), 3), 4))(f)
    //f(f(f(f(z, 1), 2), 3), 4)

    // exercise 3.11
    def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

    def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

    def length3[A](as: List[A]): Int = foldLeft(as, 0)((n: Int, _) => n + 1)

    // exercise 3.12
    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))

    // exercise 3.13
    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((a, b) => f(b, a))

    //foldRight2(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), z)((a, b) => f(b, a))
    //foldLeft(Cons(4, Cons(3, Cons(2, Cons(1, Nil)))), z)((a, b) => f(b, a))
    //foldLeft(Cons(3, Cons(2, Cons(1, Nil))), f(4, z))((a, b) => f(b, a))
    //foldLeft(Cons(2, Cons(1, Nil)), f(3, f(4, z)))((a, b) => f(b, a))
    //foldLeft(Cons(1, Nil), f(2, f(3, f(4, z))))((a, b) => f(b, a))
    //foldLeft(Nil, f(1, f(2, f(3, f(4, z)))))((a, b) => f(b, a))
    //f(1, f(2, f(3, f(4, z))))
    def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    //foldLeft2(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
    //foldRight(Cons(1, Cons(2, Cons(3, Nil))), (b: Int) => b)((a, g) => b => g(b + a))(0)
    //((b: Int) => foldRight(Cons(2, Cons(3, Nil)), (b: Int) => b)((a, g) => b => g(b + a))(b + 1))(0)
    //((b: Int) => ((c: Int) => foldRight(Cons(3, Nil), (b: Int) => b)((a, g) => b => g(b + a))(c + 2))(b + 1))(0)
    //((b: Int) => ((c: Int) => ((d: Int) => foldRight(Nil, (b: Int) => b)((a, g) => b => g(b + a))(d + 3))(c + 2))(b + 1))(0)
    //((b: Int) => ((c: Int) => ((d: Int) => ((e: Int) => e)(d + 3))(c + 2))(b + 1))(0)

    // exercise 3.14
    def append2[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)(Cons(_, _))

    def append3[A](a1: List[A], a2: List[A]): List[A] =
      foldLeft(reverse(a1), a2)((a, b) => Cons(b, a))

    // exercise 3.15
    def flatten[A](lol: List[List[A]]): List[A] =
      foldLeft(reverse(lol), Nil: List[A])((t, h) => foldLeft(reverse(h), t)((a, b) => Cons(b, a)))

    def flatten2[A](lol: List[List[A]]): List[A] =
      foldRight(lol, Nil: List[A])((h, t) => foldRight(h, t)(Cons(_, _)))

    // exercise 3.16
    def addOneEverywhere(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    def addOneEverywhere2(l: List[Int]): List[Int] =
      foldLeft(reverse(l), Nil: List[Int])((t, h) => Cons(h + 1, t))

    // exercise 3.17
    def everyValueToString(l: List[Double]): List[String] =
      foldLeft(reverse(l), Nil: List[String])((t, h) => Cons(h.toString, t))

    // exercise 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldLeft(reverse(as), Nil: List[B])((t, h) => Cons(f(h), t))

    // exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldLeft(reverse(as), Nil: List[A])((t, h) => if (f(h)) Cons(h, t) else t)

    // exercise 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      flatten(map(as)(f))

    // exercise 3.21
    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)((a: A) => if (f(a)) Cons(a, Nil: List[A]) else Nil: List[A])

    def zip[A](a1: List[A], a2: List[A]): List[List[A]] = (a1, a2) match {
      case (Nil, Nil) => Nil: List[List[A]]
      case (Cons(h1, t1), Nil) => Cons(Cons(h1, Nil), zip(t1, Nil))
      case (Nil, Cons(h2, t2)) => Cons(Cons(h2, Nil), zip(t2, Nil))
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(Cons(h1, Cons(h2, Nil)), zip(t1, t2))
    }

    // exercise 3.22
    def addLists(a1: List[Int], a2: List[Int]): List[Int] = {
      map(zip(a1, a2))((as: List[Int]) => foldLeft(as, 0)(_ + _))
    }

    // exercise 3.23
    def zipWith[A, B](a1: List[A], a2: List[A], z: B)(f: (A, B) => B): List[B] = {
      map(zip(a1, a2))((as: List[A]) => foldLeft(reverse(as), z)((b, a) => f(a, b)))
    }

    // exercise 3.24
    @tailrec
    def beginsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Cons(suph, supt), Cons(subh, subt)) if suph == subh => beginsWith(supt, subt)
      case (_, Nil) => true
      case (_, _) => false
    }

    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
      if (beginsWith(sup, sub))
        true
      else if (tail(sup) == Nil)
        false
      else
        hasSubsequence(tail(sup), sub)

  }

}