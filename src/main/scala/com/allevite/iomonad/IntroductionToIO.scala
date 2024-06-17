package com.allevite.iomonad

sealed trait IO[A]:
  self =>
  def unsafeRun: A
  def map[B](f: A => B): IO[B] = new:
    def unsafeRun = f(self.unsafeRun)
  def flatMap[B](f: A => IO[B]): IO[B] = new:
    def unsafeRun = f(self.unsafeRun).unsafeRun
object IO:
  def apply[A](a: => A): IO[A] = new: // syntax for IO { .. }
    def unsafeRun = a

  given monad: Monad[IO] with
    def unit[A](a: => A): IO[A] = IO(a)
    extension [A](fa: IO[A])
      def flatMap[B](f: A => IO[B]) = fa.flatMap(f)