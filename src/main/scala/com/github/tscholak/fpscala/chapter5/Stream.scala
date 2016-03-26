package com.github.tscholak.fpscala

import scala.annotation.tailrec

object Chapter5Stream {

  // lazy list data type, parameterized on the type `A`
  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // exercise 5.1
    // does not work for infinite Streams, but that's ok
    def toList: List[A] = {
      @tailrec
      def go(s: Stream[A], z: List[A]): List[A] = s match {
        case Empty => z
        case Cons(h, t) => go(t(), h() :: z)
      }

      go(this, List()).reverse
    }

    // exercise 5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n-1))
      case _ => Empty
    }

     def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
       case Cons(h, t) => f(h(), t().foldRight(z)(f))
       case _ => z
     }

    @tailrec
    final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z
    }

    def reverse: Stream[A] =
      this.foldLeft(Empty: Stream[A])((t, h) => Stream.cons(h, t))

    // although tail-recursive, this causes problems for infinite Streams, since they cannot be reversed
    // def foldRight[B](z: => B)(f: (A, => B) => B): B =
    //   this.reverse.foldLeft(z)((t, h) => f(h, t))

    // although it does use the reverse method, this implementation works for infinite streams, because n is finite
    def take2(n: Int): Stream[A] = {
      @tailrec
      def go(s: Stream[A], m: Int, z: Stream[A]): Stream[A] = s match {
        case Cons(h, t) if m > 0 => go(t(), m-1, Stream.cons(h(), z))
        case _ => z
      }

      go(this, n, Empty).reverse
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

    // exercise 5.3
    // although it does use the reverse method, this implementation works for infinite streams, because n is finite
    def takeWhile(p: A => Boolean): Stream[A] = {
      @tailrec
      def go(s: Stream[A], z: Stream[A]): Stream[A] = s match {
        case Cons(h, t) if p(h()) => go(t(), Stream.cons(h(), z))
        case _ => z
      }

      go(this, Empty).reverse
    }

    @tailrec
    final def dropWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => t().dropWhile(p)
      case _ => this
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    // exercise 5.4
    def forAll(p: A => Boolean): Boolean =
      this.foldRight(true)((h, t) => p(h) && t)

    // exercise 5.5
    def takeWhile2(p: A => Boolean): Stream[A] =
      this.foldRight(Empty: Stream[A])((h, t) => if (p(h)) Stream.cons(h, t) else Empty)

    // exercise 5.6
    def headOption2: Option[A] =
      this.foldRight(None: Option[A])((h, _) => Some(h))

    // exercise 5.7
    def map[B](f: A => B): Stream[B] =
      this.foldRight(Empty: Stream[B])((h, t) => Stream.cons(f(h), t))

    // returns all the elements of this Stream that satisfy the predicate p in a new Stream
    def filter(p: A => Boolean): Stream[A] =
      this.foldRight(Empty: Stream[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

    // returns all the elements of this Stream which do not satisfy the predicate p
    def filterNot(p: A => Boolean): Stream[A] = this.filter(h => !p(h))

    def append[B >: A](s: => Stream[B]): Stream[B] =
      this.foldRight(s)((h, t) => Stream.cons(h, t))

    // converts a Stream of traversable collections into a Stream formed by the elements of these collections
    def flatten[B](implicit asStream: (A) => Stream[B]): Stream[B] =
      this.foldRight(Empty: Stream[B])((h, t) => asStream(h).foldRight(t)(Stream.cons(_, _)))

    def flatMap[B](f: A => Stream[B]): Stream[B] = this.map(f).flatten

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    // exercise 5.13
    def map2[B](f: A => B): Stream[B] =
      Stream.unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }

    def take3(n: Int): Stream[A] =
      Stream.unfold((this, n)) {
        case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m-1)))
        case _ => None
      }

    def takeWhile3(p: A => Boolean): Stream[A] =
      Stream.unfold(this) {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, s2))({
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      })

    def zip[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s2))({
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
        case _ => None
      })

    // exercise 5.14
    @tailrec
    final def startsWith[B >: A](s: Stream[B]): Boolean = (this, s) match {
      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
      case (_, Empty) => true
      case (_, _) => false
    }

    def startsWith2[B >: A](s: Stream[B]): Boolean =
      this.zipAll(s).takeWhile(_._2.isDefined).forAll {
        case (h1, h2) => h1 == h2
      }

    // exercise 5.15
    def tails: Stream[Stream[A]] =
      Stream.unfold(this)({
        // case Cons(h, t) => Some((Stream.cons(h(), t()), t()))
        // case _ => None
        case Empty => None
        case s => Some((s, s.drop(1)))
      }).append(Stream(Stream.empty))

    def hasSubsequence[B >: A](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

//    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
//      case Cons(h, t) => f(h(), t().foldRight(z)(f))
//      case _ => z
//    }

//    final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
//      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
//      case _ => z
//    }

    // exercise 5.16
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      this.foldRight((z, Stream(z)))(
        (h, prev) => {
          lazy val lazyPrev = prev
          val cur = f(h, lazyPrev._1)
          (cur, Stream.cons(cur, lazyPrev._2))
        }
      )._2

    // (z, Stream(z))
    // (f(h4, z), Stream.cons(f(h4, z), Stream(z)))
    // (f(h3, f(h4, z)), Stream.cons(f(h3, f(h4, z)), Stream.cons(f(h4, z), Stream(z))))
    // (f(h2, f(h3, f(h4, z))), Stream.cons(f(h2, f(h3, f(h4, z))), Stream.cons(f(h3, f(h4, z)), Stream.cons(f(h4, z), Stream(z)))))
    // (f(h1, f(h2, f(h3, f(h4, z)))), Stream.cons(f(h1, f(h2, f(h3, f(h4, z)))), Stream.cons(f(h2, f(h3, f(h4, z))), Stream.cons(f(h3, f(h4, z)), Stream.cons(f(h4, z), Stream(z))))))

    def scanRight2[B](z: B)(f: (=> B, A) => B): Stream[B] =
      this.reverse.foldLeft((z, Stream(z)))(
        (prev, h) => {
          lazy val lazyPrev = prev
          val cur = f(lazyPrev._1, h)
          (cur, Stream.cons(cur, lazyPrev._2))
        }
      )._2

    // foldLeft(Cons(h1, Cons(h2, Cons(h3, Cons(h4, Empty)))), (z, Stream(z)))(f)
    // foldLeft(Cons(h2, Cons(h3, Cons(h4, Empty))), (f(z, h1), Stream.cons(f(z, h1), z)))(f)
    // foldLeft(Cons(h3, Cons(h4, Empty)), (f(f(z, h1), h2), Stream.cons(f(f(z, h1), h2), Stream.cons(f(z, h1), z))))(f)
    // foldLeft(Cons(h4, Empty), (f(f(f(z, h1), h2), h3), Stream.cons(f(f(f(z, h1), h2), h3), Stream.cons(f(f(z, h1), h2), Stream.cons(f(z, h1), z)))))(f)
    // foldLeft(Empty, (f(f(f(f(z, h1), h2), h3), h4), Stream.cons(f(f(f(f(z, h1), h2), h3), h4), Stream.cons(f(f(f(z, h1), h2), h3), Stream.cons(f(f(z, h1), h2), Stream.cons(f(z, h1), z))))))(f)
    // (f(f(f(f(z, h1), h2), h3), h4), Stream.cons(f(f(f(f(z, h1), h2), h3), h4), Stream.cons(f(f(f(z, h1), h2), h3), Stream.cons(f(f(z, h1), h2), Stream.cons(f(z, h1), z)))))

  }

  // representation of an empty stream
  case object Empty extends Stream[Nothing]

  // constructor for the `Stream` type, takes explicit thunks instead of regular strict values
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  // `Stream` companion object
  object Stream {

    // a smart constructor for creating a nonempty stream
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    // a smart constructor for creating an empty stream of a particular type
    def empty[A]: Stream[A] = Empty

    // a convenient variable-argument method for constructing a stream from multiple elements
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty)
        empty
      else
        cons(as.head, apply(as.tail: _*))

    // exercise 5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // exercise 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    // exercise 5.10
    def fibs: Stream[Int] = {
      def go(prev: Int, cur: Int): Stream[Int] =
        cons(prev, go(cur, prev+cur))
      go(0, 1)
    }

    // exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((a, zp)) => cons(a, unfold(zp)(f))
        case _ => Empty
      }

    // exercise 5.12
    def fibs2: Stream[Int] =
      unfold((0, 1))(z => Some((z._1, (z._2, z._1+z._2))))

    def from2(n: Int): Stream[Int] = unfold(n)(z => Some((z, z+1)))

    def constant2[A](a: A): Stream[A] = unfold(())(_ => Some((a, ())))

    def ones: Stream[Int] = constant2(1)

  }

}