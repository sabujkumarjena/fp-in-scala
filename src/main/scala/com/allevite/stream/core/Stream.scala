package com.allevite.stream.core

type Stream[+O] = Pull[O, Unit]

object Stream:
  def apply[O](os:O*): Stream[O] =
    Pull.fromList(os.toList).toStream
  extension [O](self: Stream[O])
    def toPull: Pull[O, Unit]  = self
    def fold[A](init: A)(f: (A,O) => A) : A =
      self.fold(init)(f)(1)

