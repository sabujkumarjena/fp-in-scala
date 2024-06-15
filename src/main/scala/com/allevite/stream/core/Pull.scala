package com.allevite.stream.core

import scala.annotation.targetName

/** A Pull[O, R] outputs any number of values of type [[O]] and then terminates
 *  with a single value of type [[R]]. A pull can be incrementally evaluated
 *  via [[step]], either outputting a final value of [[R]] or outputting a value
 *  [[O]] and a new pull representing the remainder of the input stream*/
enum Pull[+O, +R]:
  case Result[+R](result: R) extends Pull[Nothing, R]
  case Output[+O](value: O) extends Pull[O, Unit]
  case FlatMap[X, +O, +R](
                         source: Pull[O, X],
                         f: X => Pull[O, R]
                         ) extends Pull[O, R]
  def step: Either[R, (O, Pull[O, R])] = this match
    case  Result(r) => Left(r)
    case Output(o) => Right((o, Pull.done)) //Right((o, Result(())))
    case FlatMap(source, f) =>
      source match
        case FlatMap(s2, g) =>
          s2.flatMap(x => g(x).flatMap(y => f(y))).step
        case other => other.step match
          case Left(r) => f(r).step
          case Right((hd, tl)) => Right((hd, tl.flatMap(f)))

  def flatMap[O2 >: O, R2](
                          f: R => Pull[O2, R2]
                          ): Pull[O2, R2] = FlatMap(this,f)

  /** steps the pull until a final result is produced, accumulating an output value
    *
    * @param init
    * @param f
    * @tparam A
    * @return
    */
  def fold[A](init: A)(f: (A,O) => A): (R, A) =
    step match
      case Left(r) => (r, init)
      case Right((hd, tl)) => tl.fold(f(init, hd))(f)
  /** folds the pull, collecting all output elements into a single list, and discards the R value */
  def toList: List[O] =
    fold(List.empty[O])((l,o) => l.appended(o))._2

  @targetName("next_pull")
  def >>[O2 >: O, R2](next: => Pull[O2, R2]): Pull[O2, R2] =
    flatMap(_ => next)

  def map[R2](f: R => R2): Pull[O, R2] =
    flatMap( r => Result(f(r)))

  def repeat: Pull[O, R] =
    this >> repeat


object Pull:
  val done: Pull[Nothing, Unit] = Result(())
  //Creating Pulls
  def fromList[O](os: List[O]): Pull[O,Unit] =
    os match
      case Nil => done
      case hd :: tl => Output(hd) >> fromList(tl)

  def fromLazyList[O](os: LazyList[O]): Pull[O, Unit] =
    os match
      case LazyList() => done
      case hd #:: tl => Output(hd) >> fromLazyList(tl)

  def continually[A](a: A): Pull[A, Nothing]=
    Output(a) >> continually(a)
    
  extension [O](self: Pull[O, Unit])
    def toStream: Stream[O] = self
    





@main def MAIN(): Unit =
  println("sabuj")
  val p = Pull.Output(1) >> Pull.Output("skj")
  println(p.step)
  println(p.toList)