package com.github.tscholak.fpscala.chapter8

case class SGen[+A](g: Int ⇒ Gen[A]) {
  // exercise 8.11
  def apply(n: Int): Gen[A] = g(n)

  // exercise 8.11
  def map[B](f: A ⇒ B): SGen[B] =
    SGen(g andThen (_ map f))

  // exercise 8.11
  def flatMap[B](f: A ⇒ Gen[B]): SGen[B] =
    SGen(g andThen (_ flatMap f))

  // exercise 8.11
  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n ⇒ apply(n) ** s2(n))
}