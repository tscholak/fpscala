package com.github.tscholak.fpscala.chapter4

import scala.{Nil => _, List => _, Some => _, None => _, Option => _, _}
import scala.annotation.tailrec
import com.github.tscholak.fpscala.chapter3._

sealed trait Option[+A] {

  // exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  // defined at the beginning of section 3.1
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(ga), Some(gb)) => Some(f(ga, gb))
    case (_, _) => None
  }

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(ga => b.map(gb => f(ga, gb)))

  def map2_3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(as: List[Option[A]], z: List[A]): Option[List[A]] = as match {
      case Cons(Some(h), t) => go(t, Cons(h, z))
      case Cons(None, _) => None
      case _ => Some(List.reverse(z))
    }

    go(a, Nil: List[A])
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => h.flatMap(hh => sequence2(t).map(Cons(hh, _)))
  }

  // exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(as: List[A], z: List[B]): Option[List[B]] = as match {
      case Cons(h, t) => {
        f(h) match {
          case Some(fh) => go(t, Cons(fh, z))
          case None => None
        }
      }
      case _ => Some(List.reverse(z))
    }

    go(a, Nil: List[B])
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => map2(f(h), traverse2(t)(f))(Cons(_, _))
  }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}