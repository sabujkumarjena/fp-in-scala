package com.allevite.iomonad

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] = flatMap(a => unit(f(a)))

    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a,b)))

    def **[B](fb: F[B]): F[(A, B)] = map2(fb)((_, _))
    def *>[B](fb: F[B]): F[B] = map2(fb)((_, b) => b)

    def as[B](b: B): F[B] = map(_ => b)

    def void: F[Unit] = as(())

object Monad:

  given function0Monad: Monad[Function0] with
    def unit[A](a: => A) = () => a
    extension [A](fa: Function0[A])
      def flatMap[B](f: A => Function0[B]) =
        () => f(fa())()

  given tailrecMonad: Monad[TailRec] with
    def unit[A](a: => A) = TailCalls.done(a)
    extension [A](fa: TailRec[A])
      def flatMap[B](f: A => TailRec[B]) =
        fa.flatMap(f)