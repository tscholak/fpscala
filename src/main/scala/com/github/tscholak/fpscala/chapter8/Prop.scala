package com.github.tscholak.fpscala.chapter8

import java.util.concurrent.{ExecutorService, Executors}

import com.github.tscholak.fpscala.chapter5.Stream
import com.github.tscholak.fpscala.chapter6.RandomState
import com.github.tscholak.fpscala.chapter7.Par
import Prop._

case class Prop(run: (MaxSize, TestCases, RandomState.RNG) ⇒ Result) {
  // exercise 8.9
  def &&(p: Prop) = Prop {
    (max, n, rng) ⇒ run(max, n, rng) match {
      case Passed | Proved ⇒ p.run(max, n, rng)
      case x ⇒ x
    }
  }

  // exercise 8.9
  def ||(p: Prop) = Prop {
    (max, n, rng) ⇒ run(max, n, rng) match {
      case Falsified(msg, _) ⇒ p.tag(msg).run(max, n, rng)
      case x ⇒ x
    }
  }

  // exercise 8.9
  def tag(msg: String) = Prop {
    (max, n, rng) ⇒ run(max,n,rng) match {
      case Falsified(e, c) ⇒ Falsified(msg + "\n" + e, c)
      case x ⇒ x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RandomState.RNG): Stream[A] =
    Stream.unfold(rng)(rng ⇒ Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A ⇒ Boolean): Prop = Prop {
    (n,rng) ⇒ randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) ⇒ try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception ⇒ Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RandomState.RNG) ⇒ Result): Prop =
    Prop { (_,n,rng) ⇒ f(n,rng) }

  def forAll[A](g: SGen[A])(f: A ⇒ Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int ⇒ Gen[A])(f: A ⇒ Boolean): Prop = Prop {
    (max,n,rng) ⇒
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i ⇒ forAll(g(i))(f))
      val prop: Prop =
        props.map(p ⇒ Prop { (max, n, rng) ⇒
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RandomState.RNG = RandomState.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) ⇒
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed ⇒
        println(s"+ OK, passed $testCases tests.")
      case Proved ⇒
        println(s"+ OK, proved property.")
    }


  val es: ExecutorService = Executors.newCachedThreadPool

  val p1 = Prop.forAll(Gen.unit(Par.unit(1))) {
    i ⇒ Par.map(i)(_ + 1)(es).get == Par.unit(2)(es).get
  }

  def check(p: ⇒ Boolean): Prop = Prop {
    (_, _, _) ⇒ if (p) Passed else Falsified("()", 0)
  }

  val p2 = check {
    val p1 = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p1(es).get == p2(es).get
  }

  def equal[A](p1: Par.Par[A], p2: Par.Par[A]): Par.Par[Boolean] =
    Par.map2(p1, p2)(_ == _)

  val p3 = check {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    ) (es) get
  }

  val S = Gen.weighted(
    Gen.choose(1,4).map(Executors.newFixedThreadPool) → .75,
    Gen.unit(Executors.newCachedThreadPool) → .25
  )

  def forAllPar[A](g: Gen[A])(f: A ⇒ Par.Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s, a) ⇒ f(a)(s).get }

  def checkPar(p: Par.Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ ⇒ p)

  def forAllPar2[A](g: Gen[A])(f: A ⇒ Par.Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) ⇒ f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A ⇒ Par.Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a ⇒ f(a)(s).get }

  val pint = Gen.choose(0,10) map Par.unit
  val p4 = forAllPar(pint)(n ⇒ equal(Par.map(n)(identity), n))

  val forkProp = Prop.forAllPar(Gen.pint2)(i ⇒ equal(Par.fork(i), i)) tag "fork"
}