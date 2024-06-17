package com.allevite.stream.givens

import com.allevite.stream.core.{Monad, Pull}


//given [O]: Monad[[x] =>> Pull[O,x]] with 
//  def unit[A](a: A): Pull[O, A] = Pull.Result(a)
//  extension [A](pa: Pull[O, A])
//    def flatMap[B](f: A => Pull[O, B]): Pull[O, B] = pa.flatMap(f)
