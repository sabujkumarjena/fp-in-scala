package com.allevite.stream.core

trait Monad[F[_]]:
  def unit[A](a: A):F[A]
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]
  
