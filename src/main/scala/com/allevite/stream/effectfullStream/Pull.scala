package com.allevite.stream.effectfullStream

import com.allevite.stream.core.Monad

enum Pull[+F[_], +O, +R]:
  case Result[+R](result: R) extends Pull[Nothing, Nothing, R]
  case Output[+O](value: O) extends Pull[Nothing, O, Unit]
  case Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
  case FlatMap[+F[_], X, +O, +R](
                                source: Pull[F, O, X],
                                f: X => Pull[F, O, R]
                                ) extends Pull[F, O, R]
  def flatMap[F2[X] >: F[X], O2 >: O, R2 >: R](f: R => Pull[F2,O2, R2 ] ): Pull[F2, O2, R2] = FlatMap(this, f)
  def step[F2[X] >: F[X], O2 >: O, R2 >: R](
                                            using F: Monad[F2] // Monad instance for effect type
                                            ): F2[Either[R2, (O2, Pull[F2, O2, R2])]] =
    this match
      case Result(r) => F.unit(Left(r)) //Lift the pure result into the effect type via unit
      case Output(o) => F.unit(Right(o, Result(()))) // Pull.done = Result(())
      case Eval(action) => action.map(Left(_)) // wrap the result of the action in a Left
//      case FlatMap(source, f) =>
//        source match
//          case FlatMap(s2, g) => ???
//          case other => other.step.flatMap:
//            case Left(r) => ???
//            case Right((hd, tl)) => F.unit(Right(hd, tl.flatMap(f)))