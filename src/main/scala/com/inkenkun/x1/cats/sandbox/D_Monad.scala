package com.inkenkun.x1.cats.sandbox

object MonadDeifinition {

  /**
    * 4.1.2 Exercise: Getting Func-y
    */
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }
}

object MonadTypeClass {
  /**
    * 4.2.1 The Monad Type Class
    */
  import cats.Monad
  import cats.instances.option._ // for Monad
  import cats.instances.list._   // for Monad

  val opt1 = Monad[Option].pure(3)
  // opt1: Option[Int] = Some(3)

  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  // opt2: Option[Int] = Some(5)

  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  // opt3: Option[Int] = Some(500)

  val list1 = Monad[List].pure(3)
  // list1: List[Int] = List(3)

  val list2 = Monad[List].
    flatMap(List(1, 2, 3))(a => List(a, a*10))
  // list2: List[Int] = List(1, 10, 2, 20, 3, 30)

  val list3 = Monad[List].map(list2)(a => a + 123)
  // list3: List[Int] = List(124, 133, 125, 143, 126, 153)

}

object MonadInstances {
  /**
    * 4.2.2 Default Instances
    */
  import cats.Monad
  import cats.instances.option._ // for Monad

  Monad[Option].flatMap(Option(1))(a => Option(a*2))
  // res0: Option[Int] = Some(2)

  import cats.instances.list._ // for Monad

  Monad[List].flatMap(List(1, 2, 3))(a => List(a, a*10))
  // res1: List[Int] = List(1, 10, 2, 20, 3, 30)

  import cats.instances.vector._ // for Monad

  Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a*10))


  import cats.instances.future._ // for Monad
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val fm = Monad[Future]

}

object IdMonad {
  /**
    * 4.3.1 Exercise: Monadic Secret Identities
    */
  type Id[A] = A
//  trait Id[A] {
//    def pure(a: A): Id[A] = a
//
//  }
  object Id {
    def apply[A](a: A): Id[A] = a
    def pure[A](a: A): Id[A] = a
    def map[A,B](a: Id[A])(f: A => B): Id[B] = f(a)
    def flatMap[A,B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }
}