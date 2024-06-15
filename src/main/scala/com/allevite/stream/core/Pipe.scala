package com.allevite.stream.core

type Pipe[-I, +O] = Stream[I] => Stream[O]

