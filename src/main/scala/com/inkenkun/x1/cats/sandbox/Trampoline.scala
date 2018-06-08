package com.inkenkun.x1.cats.sandbox

import scala.language.higherKinds

case class Done[+A](a: A) extends Trampoline[A]
case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class FlatMap[A, B](sub: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]


sealed trait Trampoline[+A] {
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k)  => k().runT
  }

  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(a)                   => Right(a)
    case More(k)                   => Left(k)
    case FlatMap(Done(a)      , f) => f(a).resume // 末尾再帰！
    case FlatMap(More(k)      , f) => Left(() => FlatMap(k(), f))
    case FlatMap(FlatMap(a, f), g) => FlatMap(a, (x: Any) => FlatMap(f(x), g)).resume  // 末尾再帰！
  }

  def flatMap[B](g: A => Trampoline[B]): Trampoline[B] = this match {
    case FlatMap(a: Trampoline[A], f: (A => Trampoline[B])) => FlatMap[A, B](a, a => f(a).flatMap(g))
    case a: Trampoline[A] => FlatMap(a, g)
  }

  def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))
}


object MainTrampoline extends App
  with Example
//  with Fact5ExecutionPlan
//  with ExampleFor

trait Example {
  def fact(n: Int): Trampoline[BigInt] = {
    if (n <= 1) Done(n)
    else        More(() => FlatMap(fact(n - 1), (x: BigInt) => Done(x * n)))
  }

  println(fact(10000).runT)

  def foldRight[A, B](list: List[A])(z: B)(f: (A, B) => B): Trampoline[B] = {
    list match {
      case Nil     => Done(z)
      case a :: as => More(() => FlatMap(foldRight(as)(z)(f), (b: B) => Done(f(a, b))))
    }
  }

  println(foldRight((1 to 100000).toList)(0L)(_ + _).runT)

  def fib(n: Int): Trampoline[Long] =
    if (n <= 1) Done(n)
    else        More(() =>
      FlatMap(fib(n - 2), (x: Long) =>
        FlatMap(fib(n - 1), (y: Long) =>
          Done(x + y))))

  println(fib(30).runT)
}

trait Fact5ExecutionPlan {
  println( // fact(5).runT
    FlatMap(Done(1), (a: Int) =>
      FlatMap(Done(a * 2), (b: Int) =>
        FlatMap(Done(b * 3), (c: Int) =>
          FlatMap(Done(c * 4), (d: Int) =>
            Done(d * 5))))).resume
  ) // Right(120)
}

trait ExampleFor {
  def fact(n: Int): Trampoline[BigInt] = {
    if (n <= 1) Done(n)
    else        More(() => fact(n - 1).map(_ * n))
  }

  println(fact(10000).runT)

  def foldRight[A, B](list: List[A])(z: B)(f: (A, B) => B): Trampoline[B] = {
    list match {
      case Nil     => Done(z)
      case a :: as => More(() => foldRight(as)(z)(f).map(b => f(a, b)))
    }
  }

  println(foldRight((1 to 100000).toList)(0L)(_ + _).runT)

  def fib(n: Int): Trampoline[Long] =
    if (n <= 1) Done(n) else for {
      x <- More(() => fib(n -1))
      y <- More(() => fib(n -2))
    } yield x + y

  println(fib(30).runT)
}
