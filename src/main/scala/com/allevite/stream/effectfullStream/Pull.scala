package com.allevite.stream.effectfullStream

enum Pull[+F[_], +O, +R]:
  case Result[+R](result: R) extends Pull[Nothing, Nothing, R]
  case Output[+O](value: O) extends Pull[Nothing, O, Unit]
  case Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
  case FlatMap[+F[_], X, +O, +R](
                                source: Pull[F, O, X],
                                f: X => Pull[F, O, R]
                                ) extends Pull[F, O, R]


