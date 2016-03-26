package com.github.tscholak.fpscala

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

import scalaz.concurrent._

// see also https://en.wikipedia.org/wiki/Actor_model,
//          http://berb.github.io/diploma-thesis/original/054_actors.html,
//          http://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency,
//          http://doc.akka.io/docs/misc/Comparison_between_4_actor_frameworks.pdf,
//          http://stackoverflow.com/questions/5997018/whats-the-difference-between-the-different-actors-implementation-in-scala
//          http://docs.scala-lang.org/overviews/core/actors.html,
//          http://docs.scala-lang.org/overviews/core/actors-migration-guide.html
//          http://doc.akka.io/docs/akka/2.4.2/general/actors.html,
//          https://www.manning.com/books/akka-in-action,
//          https://github.com/plokhotnyuk/actors,
//          http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue,
//          http://stew.vireo.org/posts/I-hate-akka/
//          https://github.com/viktorklang/blog/blob/master/Futures-in-Scala-2.12-part-1.md

object Chapter7Par {

  // where is the callback/continuation `cb` eventually coming from?
  sealed trait Future[A] {
    // this method is declared private so that its side effects are unobservable to outside code using this API
    private[fpscala] def apply(cb: A => Unit): Unit
  }


  // `p(es)(result => ...)` for some `ExecutorService`, `es`, and some `Par`, `p`, is the idiom for running `p`, and
  // registering a callback to be invoked when its result is available. The result will be bound to `result` in the
  // function passed to `p(es)`
  type Par[A] = ExecutorService => Future[A]

  object Par {

    // has to block the calling thread while waiting for a latch
    def run[A](es: ExecutorService)(pa: Par[A]): A = {

      // a mutable, thread-safe reference to use for storing the result
      val ref = new AtomicReference[A]

      // a latch allows threads to wait until its `countDown` method is called a certain number of times (here: once)
      val latch = new CountDownLatch(1)

      // when the value is received, this sets the result and releases the hatch by calling `countDown` once
      pa(es) { (a: A) => ref.set(a); latch.countDown }

      // waits until the result becomes available and the latch is released
      latch.await

      // at this point we know that `ref` has been set, and we can safely return its value
      ref.get

    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        // passes the value to the callback/continuation `cb` without making use of `ExecutorService`
        def apply(cb: A => Unit): Unit = cb(a)
      }

    // a non-strict version of `unit`
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    // fork off evaluation of `r` and return immediately
    def eval(es: ExecutorService)(r: => Unit): Unit = {
      es.submit(new Callable[Unit] {
        def call: Unit = r
      })
    }

    // construct `Par` values out of calls to non-blocking continuation-passing-style APIs
    def async[A](f: (A => Unit) => Unit): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit) = f(cb)
      }

    // when the `Future` returned by `fork` receives its callback/continuation `cb`, it will fork off a task to evaluate
    // `a`
    def fork[A](pa: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = {
          // once `pa` has been evaluated and called with `es` to produce `Future[A]`, we register the callback/
          // continuation `cb` to be invoked when that `Future` has its resulting `A`
          eval(es)(pa(es)(cb))
        }
      }

    // exercise 7.1
    // superseded by listing 7.7
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          // use two *mutable* vars to store the two results
          var ar: Option[A] = None
          var br: Option[B] = None

          // define an actor that awaits both results, combine them with `f`, and pass the result to the callback/
          // continuation `cb`
          // note that scalaz's `Actor` invokes an `ExecutorService`, here `es`, implicitly
          val combiner = Actor[Either[A, B]] {
            case Left(a) => br match {
              // if the `A` result comes in first, store it in `ar` and wait for the `B` result to come in
              case None => ar = Some(a)
              // otherwise, call `f` with both results and pass the resulting `C` to the callback/continuation `cb`
              case Some(b) => eval(es)(cb(f(a, b)))
            }

            case Right(b) => ar match {
              // if the `B` result comes in first, store it in `br` and wait for the `A` result to come in
              case None => br = Some(b)
              // otherwise, call `f` with both results and pass the resulting `C` to the callback/continuation `cb`
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          // pass the `combiner` actor as a callback/continuation to both sides
          // on the `A` side, we wrap the result in `Left`
          pa(es) { a => combiner ! Left(a) }
          // on the `B` side, we wrap the result in `Right`
          pb(es) { b => combiner ! Right(b) }
        }
      }

    // map in terms of map2
    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists,
    // these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    def fold[A, B](xs: IndexedSeq[Par[A]], z: B)(project: A => B)(merge: (B, B) => B): Par[B] = {
      import scala.util.control.TailCalls._

      def go(xs: IndexedSeq[Par[A]]): TailRec[Par[B]] = {
        if (xs.length <= 1)
          done(xs.headOption.fold(unit(z))(map(_)(project)))
        else {
          // divide the sequence in half using the `splitAt` function
          val (l, r) = xs.splitAt(xs.length / 2)

          for {
            lFold <- tailcall(go(l))
            rFold <- tailcall(go(r))
          } yield fork(map2(lFold, rFold)(merge))
        }
      }

      fork(go(xs).result)
    }

    // exercise 7.5
    private def sequence[A](lpa: List[Par[A]]): Par[List[A]] =
      map(fold(lpa.toIndexedSeq, IndexedSeq[A]())(IndexedSeq(_))(_ ++ _))(_.toList)

    def parMap[A, B](la: List[A])(f: A => B): Par[List[B]] = fork {
      val lpb: List[Par[B]] = la.map(asyncF(f))
      sequence(lpb)
    }

    def parFold[A, B](xs: IndexedSeq[A], z: B)(project: A => B)(merge: (B, B) => B): Par[B] =
      fold(xs.map(unit), z)(project)(merge)

    // exercise 7.6
    def parFilter[A](la: List[A])(f: A => Boolean): Par[List[A]] = {
      val lpla: List[Par[List[A]]] = la.map(
        // trick to deal with elements being dropped: use nested lists
        asyncF(a =>
          if (f(a))
            List(a)
          else
            Nil
        )
      )
      val plla = sequence(lpla)
      // convert the nested list into a flattened list
      map(plla)(_.flatten)
    }

    // how to order stuff in a type-agnostic way? with `implicit`!
    def sortPar[A, B >: A](implicit ord: math.Ordering[B], parList: Par[List[A]]) =
      map(parList)(_.sorted(ord))

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b)
              eval(es) { t(es)(cb) }
            else
              eval(es) { f(es)(cb) }
          }
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b => eval(es) { ps(b)(es)(cb) } }
      }

    // exercise 7.11
    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    // exercise 7.12
    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit): Unit =
          p(es) { k =>
            eval(es) {
              // TODO: what happens when `k` isn't in `ps`?
              ps get k foreach { _(es)(cb) }
            }
          }
      }

    // exercise 7.13
    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          // TODO: do I need the `eval(es)` here?
          p(es) { a => eval(es) { f(a)(es)(cb) } }
      }

    // a parallel computation that, when run, will run an initial computation whose result is used to determine a second
    // computation
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      flatMap(p)(f)

    def choiceViaChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(p)(b => if (b) t else f)

    def choiceNViaChooser[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      chooser(p)(i => ps(i))

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          // TODO: again, do I need the `eval(es)` here, e.g. why not `p(es) { _(es)(cb) }`?
          p(es) { pa => eval(es) { pa(es)(cb) } }
      }

    def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
      flatMap(ppa)(pa => pa)

    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pa)(f))

    // can I implement a function with the same signature as `map2`, but using `flatMap` and `unit`?
    // TODO: how is this different than `map2`?
    // I guess it's sequential -- first a, then b.
    def notMap2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }

    // TODO: is this the same as `notMap2`?
    // Almost, I think scala uses `map` instead of `unit` here.
    // TODO: check this
    // Note that the above implicit conversion is necessary for the for-comprehension to work
    def alsoNotMap2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = for {
      a <- pa
      b <- pb
    } yield f(a, b)

  }

  object Examples {
    import Par._

    def parSum(ints: IndexedSeq[Int]): Par[Int] =
      parFold(ints, 0)(identity)(_ + _)

    def parMultiply(ints: IndexedSeq[Int]): Par[Int] =
      parFold(ints, 1)(identity)(_ * _)

    def parFactorial(n: Int): Par[BigInt] =
      parFold(1 to n, BigInt(1))(i => i)(_ * _)

    // find the maximum value of an IndexedSeq in parallel
    def parMax(ints: IndexedSeq[Int]): Par[Int] =
      parFold(ints, Int.MinValue)(identity)(_ max _)

    // find the minimum value of an IndexedSeq in parallel
    def parMin(ints: IndexedSeq[Int]): Par[Int] =
      parFold(ints, Int.MaxValue)(identity)(_ min _)

    // a function that takes a list of paragraphs (a `List[String]`) and returns the total number of words across all
    // paragraphs, in parallel
    def parWordCount(strings: List[String]): Par[Int] =
      parFold(strings.toIndexedSeq, 0)(_.split("\\W+").length)(_ + _)

    // map3 implemented in terms of map2
    def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(map2(pa, pb)((_, _)), pc)((t: (A, B), c: C) => f(t._1, t._2, c))

    // map4 implemented in terms of map2
    def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] =
      //map3(map2(pa, pb)((_, _)), pc, pd)((t: (A, B), c: C, d: D) => f(t._1, t._2, c, d))
      //map2(map2(pa, pb)((_, _)), map2(pc, pd)((_, _)))((t1: (A, B), t2: (C, D)) => f(t1._1, t1._2, t2._1, t2._2))
      map2(map2(map2(pa, pb)((_, _)), pc)((_, _)), pd)((t, d) => f(t._1._1, t._1._2, t._2, d))

    // map5 implemented in terms of map2
    def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])
                              (f: (A, B, C, D, E) => F): Par[F] =
      map2(map2(map2(map2(pa, pb)((_, _)), pc)((_, _)), pd)((_, _)), pe)((t, e) =>
        f(t._1._1._1, t._1._1._2, t._1._2, t._2, e))

  }

}