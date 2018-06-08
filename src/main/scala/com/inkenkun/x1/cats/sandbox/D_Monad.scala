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
//  import scala.concurrent.duration._
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

object ErrorHandling {
  /**
    * 4.4 Either
    */
  import cats.syntax.either._

  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String)
    extends LoginError

  final case class PasswordIncorrect(username: String)
    extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError =>
        println(s"Unexpected error")
  }

  import cats.MonadError
  import cats.instances.either._ // for MonadError

  type ErrorOr[A] = Either[String, A]


  def main(args: Array[String]): Unit = {
    /**
      * 4.4.4 Error Handling
      */
    val result1: LoginResult = User("dave", "passw0rd").asRight
    // result1: LoginResult = Right(User(dave,passw0rd))

    val result2: LoginResult = UserNotFound("dave").asLeft
    // result2: LoginResult = Left(UserNotFound(dave))

    result1.fold(handleError, println)
    // User(dave,passw0rd)”
    println(result2)

    /**
      * 4.5.2 Raising and Handling Errors
      */
    val monadError = MonadError[ErrorOr, String]
    val success = monadError.pure(42)
    // success: ErrorOr[Int] = Right(42)

    val failure = monadError.raiseError("Badness")
    // failure: ErrorOr[Nothing] = Left(Badness)”

    monadError.handleError(failure) {
      case "Badness" =>
        monadError.pure("It's ok")

      case other =>
        monadError.raiseError("It's not ok")
    }
    // res2: ErrorOr[ErrorOr[String]] = Right(Right(It's ok))

    monadError.ensure(success)("Number too low!")(_ > 1000)

    en()

    def en(): Unit = {
      import cats.syntax.applicative._      // for pure
      import cats.syntax.applicativeError._ // for raiseError etc
      import cats.syntax.monadError._       // for ensure

      val success = 42.pure[ErrorOr]
      val failure = "Badness".raiseError[ErrorOr, Int]
      println(failure)

      success.ensure("Number to low!")(_ > 1000)
      // res4: Either[String,Int] = Left(Number to low!)
    }

    /**
      * 4.6.5
      */
    import cats.Eval

    def foldRightOrg[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
      case head :: tail =>
        fn(head, foldRightOrg(tail, acc)(fn))
      case Nil =>
        acc
    }
    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
      def loop[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
        as match {
          case head :: tail =>
            Eval.defer(fn(head, foldRight(tail, acc)(fn)))
          case Nil =>
            acc
        }
      loop(as, Eval.now(acc)) { (a, b) => b.map(fn(a, _)) }.value
    }
  }
}

/**
  * 4.7 The Writer Monad
  *  Writer Monadはログと計算を一緒に運びます.
  *
  */
object WriterMonad {
  def main(args: Array[String]): Unit = {
    import cats.data.Writer
    import cats.instances.vector._
    import cats.syntax.applicative._
    import cats.syntax.writer._

    Writer(Vector(
      "It was the best of times",
      "It was the worst of times"
    ), 1859)

    type Logged[A] = Writer[Vector[String], A]
    123.pure[Logged]

    Vector("msg1", "msg2", "msg3").tell
    val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
    val b = 123.writer(Vector("msg1", "msg2", "msg3"))

    val aResult: Int = a.value
    val aLog: Vector[String] = a.written
    val (log, result) = b.run

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b
    writer1.run

    val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
    writer2.run

    val writer3 = writer1.bimap(
      log => log.map(_.toUpperCase),
      res => res * 100
    )
    writer3.run

    val writer4 = writer1.mapBoth { (log, res) =>
      val log2 = log.map(_ + "!")
      val res2 = res * 100
      (log2, res2)
    }
    writer4.run

    val writer5 = writer1.reset
    writer5.run

    val writer6 = writer1.swap
    writer6.run

    /**
      * 4.7.3 Exercise: Show Your Working
      */
    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }
//    factorial(5)

    def newFactorial(n: Int): Logged[Int] = {
      val ans: Logged[Int] = slowly {
        if (n == 0) 1.pure[Logged]
        else {
          newFactorial(n - 1).map(_ * n)
        }
      }
      ans.mapBoth { (log: Vector[String], res) =>
        (log :+ s"fact $n $res", res)
      }
    }

    def rsfactorial(n: Int): Logged[Int] =
      for {
        ans <- if(n == 0) {
            1.pure[Logged]
          } else {
            slowly(rsfactorial(n - 1).map(_ * n))
          }
        o <- Vector(s"fact $n $ans").tell
      } yield {
        ans
      }


    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val results = Await.result(Future.sequence(Vector(
      Future(rsfactorial(5).run),
      Future(rsfactorial(3).run)
    )), 5.seconds)

    results.foreach { r =>
      println(r._1.mkString("\n"))
      println("----------------------")
    }


  }
}

/**
  * 4.8 The Reader Monad
  *
  * Reader Monad は 依存注入(Dependency Injection)に良く使われる.
  */
object ReaderMonad {
  def main(args: Array[String]): Unit = {
    import cats.data.Reader

    case class Cat(name: String, favoriteFood: String)
    // defined class Cat

    val catName: Reader[Cat, String] =
      Reader(cat => cat.name)
    catName.run(Cat("Garfield", "lasagne"))

    val greetKitty: Reader[Cat, String] =
      catName.map(name => s"Hello ${name}")

    greetKitty.run(Cat("Heathcliff", "junk food"))

    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed  <- feedKitty
      } yield s"$greet. $feed."

    greetAndFeed(Cat("Garfield", "lasagne"))
    // res3: cats.Id[String] = Hello Garfield. Have a nice bowl of lasagne.

    greetAndFeed(Cat("Heathcliff", "junk food"))
    // res4: cats.Id[String] = Hello Heathcliff. Have a nice bowl of junk food.

    /**
      * “4.8.3 Exercise: Hacking on Readers
      */
    case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
    )

    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(
      username: String,
      password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(
      userId: Int,
      password: String): DbReader[Boolean] =
      ???
    //    val validList = valid.run(Db(
//      Map(1 -> "apple", 2 -> "banana", 3 -> "grape", 4 -> "kiwi"),
//      Map("apple" -> "sider", "banana" -> "milk", "kiwi" -> "juce", "peach" -> "nector")
//    ))
//    validList.foreach(println(_))
  }
}