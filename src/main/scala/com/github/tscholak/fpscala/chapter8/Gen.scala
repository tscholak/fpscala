package com.github.tscholak.fpscala.chapter8

import com.github.tscholak.fpscala.chapter6.{State, RandomState}
import com.github.tscholak.fpscala.chapter7.Par

import scala.language.implicitConversions

case class Gen[+A](sample: RandomState.Rand[A]) {
  def map[B](f: A ⇒ B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) ⇒ C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  // exercise 8.6
  def flatMap[B](f: A ⇒ Gen[B]): Gen[B] =
    Gen(sample.flatMap(a ⇒ f(a).sample))

  // exercise 8.6
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  // exercise 8.6
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n ⇒ this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)
  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  // exercise 8.10
  def unsized = SGen(_ ⇒ this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_,_))
}

object Gen {
  // exercise 8.5
  def unit[A](a: ⇒ A): Gen[A] =
    Gen(State.unit(a))

  // exercise 8.5
  val boolean: Gen[Boolean] =
    Gen(RandomState.flipCoin)

  // exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RandomState.nonNegativeInt.map(n ⇒ start + n % (stopExclusive-start)))

  // exercise 8.5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  val uniform: Gen[Double] = Gen(RandomState.double)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(RandomState.double.map(d ⇒ i + d*(j-i)))


  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
      map (n ⇒ if (n%2 != 0) n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
      map (n ⇒ if (n%2 == 0) n+1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i ← choose(from, to)
    j ← if (i%2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a,b) ⇒ a.map2(b)(_ :: _))

  // exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b ⇒ if (b) g1 else g2)

  // exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(RandomState.double flatMap {
      d ⇒ if (d * (g1._2.abs + g2._2.abs) < g1._2.abs) g1._1.sample else g2._1.sample
    })
  }

  // exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n ⇒ g.listOfN(n))

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ ⇒ g)

  val smallInt: Gen[Int] = choose(-10, 10)
  val maxProp = Prop.forAll(listOf(smallInt)) { l ⇒
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  // exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n ⇒ g.listOfN(n max 1))

  // exercise 8.13
  val maxProp1 = Prop.forAll(listOf1(smallInt)) { l ⇒
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  // exercise 8.14
  val sortedProp = Prop.forAll(listOf(smallInt)) { l ⇒
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a,b) ⇒ a > b }
  }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  // exercise 8.16
  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
   * computation for each element of the input list summed to produce the final
   * result. This is not the most compelling example, but it provides at least some
   * variation in structure to use for testing.
   *
   * Note that this has to be a `lazy val` because of the way Scala initializes objects.
   * It depends on the `Prop` companion object being created, which references `pint2`.
   */
  lazy val pint2: Gen[Par.Par[Int]] =
    choose(-100,100).
      listOfN(choose(0,20)).
      map {
        l ⇒ l.foldLeft(Par.unit(0)) {
          (p, i) ⇒ Par.fork {
            Par.map2(p, Par.unit(i))(_ + _)
          }
        }
      }

  def genStringIntFn(g: Gen[Int]): Gen[String ⇒ Int] =
    g map (i ⇒ s ⇒ i)
}