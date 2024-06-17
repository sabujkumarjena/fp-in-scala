package com.allevite.iomonad

object IO2:
  enum IO[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
  
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
    def map[B](f: A => B): IO[B] =
      flatMap(a => Return(f(a)))
  
    // There is only one sensible way to implement this as a
    // tail-recursive function, the one tricky case is left-nested
    // flatMaps, as in `a.flatMap(f).flatMap(g)`, which we
    // reassociate to the right as `a.flatMap(ar => f(a).flatMap(g))`
    @annotation.tailrec final def unsafeRun(): A = this match
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).unsafeRun()
        case Suspend(r) => f(r()).unsafeRun()
        case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun()
  
  object IO: // Notice that none of these operations DO anything
    def apply[A](a: => A): IO[A] =
      suspend(Return(a))
  
    def suspend[A](ioa: => IO[A]): IO[A] =
      Suspend(() => ioa).flatMap(identity)
  
    given monad: Monad[IO] with
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        def flatMap[B](f: A => IO[B]): IO[B] = fa.flatMap(f)