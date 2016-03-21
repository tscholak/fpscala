package com.github.tscholak.fpscala

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.annotation.tailrec

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

object Chapter6Par {

  sealed trait Future[A] {
    // this method is declared private so that its side effects are unobservable to outside code using this API
    private[Chapter6Par] def apply(k: A => Unit): Unit
  }

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

    // fork off evaluation of `r` and return immediately
    def eval(es: ExecutorService)(r: => Unit): Unit = {
      es.submit(new Callable[Unit] {
        def call: Unit = r
      })
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

    // where is the callback/continuation `cb` coming from?

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

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
          val combiner = Actor[Either[A, B]](es) {
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
          pa(es)(a => combiner ! Left(a))
          // on the `B` side, we wrap the result in `Right`
          pb(es)(b => combiner ! Right(b))
        }
      }

    // map in terms of map2
    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    // exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    // exercise 7.5
    def sequence[A](lpa: List[Par[A]]): Par[List[A]] = {
      // just repurpose the approach for the `State` case class
      lpa.reverse.foldLeft(unit[List[A]](Nil)) {
        (pa, pb) => map2(pa, pb)((t, h) => h :: t)
      }
    }

    def parMap[A, B](la: List[A])(f: A => B): Par[List[B]] = fork {
      val lpb: List[Par[B]] = la.map(asyncF(f))
      sequence(lpb)
    }

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

  }


  /**
    * Implementation is taken from `scalaz` library, with only minor changes. See:
    *
    * https://github.com/scalaz/scalaz/blob/6146103aed28b4f603ec02cd9df8bcade31e15f7/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
    *
    * This code is copyright Andriy Plokhotnyuk, Runar Bjarnason, and other contributors,
    * and is licensed using 3-clause BSD, see LICENSE file at:
    *
    * https://github.com/scalaz/scalaz/blob/scalaz-seven/etc/LICENCE
    */

  /**
    * Processes messages of type `A`, one at a time. Messages are submitted to
    * the actor with the method `!`. Processing is typically performed asynchronously,
    * this is controlled by the provided `strategy`.
    *
    * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
    * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
    * the `strategy` runs the invocations of `handler` on separate threads. This is achieved because
    * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
    * location before suspending.
    *
    * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
    * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
    *
    * @see scalaz.concurrent.Promise for a use case.
    *
    * @param handler  The message handler
    * @param onError  Exception handler, called if the message handler throws any `Throwable`.
    * @param strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
    * @tparam A       The type of messages accepted by this actor.
    */
  final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw _) {
    self =>

    private val tail = new AtomicReference(new Node[A]())
    private val suspended = new AtomicInteger(1)
    private val head = new AtomicReference(tail.get)

    /** Alias for `apply` */
    def !(a: A) {
      val n = new Node(a)
      head.getAndSet(n).lazySet(n)
      trySchedule()
    }

    /** Pass the message `a` to the mailbox of this actor */
    def apply(a: A) {
      this ! a
    }

    def contramap[B](f: B => A): Actor[B] =
      new Actor[B](strategy)((b: B) => this ! f(b), onError)

    private def trySchedule() {
      if (suspended.compareAndSet(1, 0)) schedule()
    }

    private def schedule() {
      strategy(act())
    }

    private def act() {
      val t = tail.get
      val n = batchHandle(t, 1024)
      if (n ne t) {
        n.a = null.asInstanceOf[A]
        tail.lazySet(n)
        schedule()
      } else {
        suspended.set(1)
        if (n.get ne null) trySchedule()
      }
    }

    @tailrec
    private def batchHandle(t: Node[A], i: Int): Node[A] = {
      val n = t.get
      if (n ne null) {
        try {
          handler(n.a)
        } catch {
          case ex: Throwable => onError(ex)
        }
        if (i > 0) batchHandle(n, i - 1) else n
      } else t
    }
  }

  private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

  object Actor {

    /** Create an `Actor` backed by the given `ExecutorService`. */
    def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw _): Actor[A] =
      new Actor(Strategy.fromExecutorService(es))(handler, onError)

  }

  /**
    * Provides a function for evaluating expressions, possibly asynchronously.
    * The `apply` function should typically begin evaluating its argument
    * immediately. The returned thunk can be used to block until the resulting `A`
    * is available.
    */
  trait Strategy {
    def apply[A](a: => A): () => A
  }

  object Strategy {

    /**
      * We can create a `Strategy` from any `ExecutorService`. It's a little more
      * convenient than submitting `Callable` objects directly.
      */
    def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
      def apply[A](a: => A): () => A = {
        val f = es.submit { new Callable[A] { def call = a } }
        () => f.get
      }
    }

    /**
      * A `Strategy` which begins executing its argument immediately in the calling thread.
      */
    def sequential: Strategy = new Strategy {
      def apply[A](a: => A): () => A = {
        val r = a
        () => r
      }
    }

  }

}