package com.github.tscholak.fpscala

object Chapter6State {

  case class State[S, +A](run: S => (A, S)) {

    // exercise 6.10
    // make use of `case class` syntactic sugar
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, sa) = this.run(s)
        f(a).run(sa)
      })

    // exercise 6.10
    def map[B](f: A => B): State[S, B] =
      this.flatMap(a => State.unit(f(a)))

    // exercise 6.10
    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- this
        b <- sb
      } yield f(a, b)
      // this.flatMap(a => sb.map(b => f(a, b)))

  }

  object State {

    // exercise 6.10
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    // exercise 6.10
    // def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    //   State(s => {
    //     ls.foldRight((Nil: List[A], s))((sh, st) => {
    //       val (h, nextState) = sh.run(st._2)
    //       (h :: st._1, nextState)
    //     })
    //   })

    // exercise 6.10
    def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
      ls.reverse.foldLeft(unit[S, List[A]](Nil))(
        (st, sh) => sh.map2(st)(_ :: _)
        // (st, sh) => for {
        //   h <- sh
        //   t <- st
        // } yield h :: t
        // (st, sh) => sh.flatMap(h => st.map(t => h :: t))
      )

    // foldLeft(Cons(s4, Cons(s3, Cons(s2, Cons(s1, Nil)))), unit(Nil))(f)
    // foldLeft(Cons(s3, Cons(s2, Cons(s1, Nil))), f(unit(Nil), s4))(f)
    // foldLeft(Cons(s2, Cons(s1, Nil)), f(f(unit(Nil), s4), s3))(f)
    // foldLeft(Cons(s1, Nil), f(f(f(unit(Nil), s4), s3), s2))(f)
    // foldLeft(Nil, f(f(f(f(unit(Nil), s4), s3), s2), s1))(f)
    // f(f(f(f(unit(Nil), s4), s3), s2), s1)

    // f(unit(Nil), s4)
    // s4.map2(unit(Nil))(_ :: _)
    // s4.flatMap(h4 => unit(Nil).map(t4 => h4 :: t4))
    // for {
    //   h4 <- s4
    //   t4 <- unit(Nil)
    // } yield h4 :: t4

    // nested for-comprehensions ...

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))
    
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }
}