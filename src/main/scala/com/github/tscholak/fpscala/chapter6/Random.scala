package com.github.tscholak.fpscala.chapter6

object Random {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((Nil: List[A], rng))((rh, rt) => {
        val (h, nextRNG) = rh(rt._2)
        (h :: rt._1, nextRNG)
      })
    }

  // exercise 6.8 part 1
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  // exercise 6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

      // the next state is an `RNG` instance created from `newSeed`
      val nextRNG = SimpleRNG(newSeed)

      // `>>>` is right binary shift with zero fill
      val n = (newSeed >>> 16).toInt

      // as specified in the trait, the return value is a tuple of a pseudo-random integer and an `RNG` instance
      (n, nextRNG)
    }

  }

  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt

    val nn = if (n < 0) -(n+1) else n

    (nn, nextRNG)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // exercise 6.8 part 2
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

  // exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nn, nextRNG) = nonNegativeInt(rng)

    val d = nn.toDouble / (1.0 + Int.MaxValue.toDouble)

    (d, nextRNG)
  }

  // exercise 6.5
  def double2: Rand[Double] = map(nonNegativeInt)(_.toDouble / (1.0 + Int.MaxValue.toDouble))

  // exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, tmpRNG) = rng.nextInt
    val (d, nextRNG) = double(tmpRNG)

    ((n, d), nextRNG)
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, tmpRNG) = double(rng)
    val (n, nextRNG) = tmpRNG.nextInt

    ((d, n), nextRNG)
  }

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, tmpRNG1) = double(rng)
    val (d2, tmpRNG2) = double(tmpRNG1)
    val (d3, nextRNG) = double(tmpRNG2)

    ((d1, d2, d3), nextRNG)
  }

  // exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (h, nextRNG) = rng.nextInt
      val (t, retRNG) = ints(count - 1)(nextRNG)
      (h :: t, retRNG)
    } else
      (Nil, rng)
  }

  // exercise 6.7
  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // implementation of rollDie using `map` and `nonNegativeLessThan`
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}
