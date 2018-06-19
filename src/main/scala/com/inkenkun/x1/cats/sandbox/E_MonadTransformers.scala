package com.inkenkun.x1.cats.sandbox

object MonadTransformersExercise {

  /**
   * 5.1 Exercise: Composing Monads
   */
  import cats.Monad
  import cats.syntax.applicative._ // for pure
  import cats.syntax.flatMap._     // for flatMap
  import scala.language.higherKinds

  // Hypothetical example. This won't actually compile:
  def compose[M1[_]: Monad, M2[_]: Monad] = {
    type Composed[A] = M1[M2[A]]

    new Monad[Composed] {
      def pure[A](a: A): Composed[A] =
        a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])
        (f: A => Composed[B]): Composed[B] =
      // Problem! How do we write flatMap?
        ???
      /* for Option
      def flatMap[A, B](fa: Composed[A])
        (f: A => Composed[B]): Composed[B] =
        fa.flatMap(_.fold(None.pure[M])(f))
      */
      override def tailRecM[A, B](a: A)(f: (A) => Composed[Either[A, B]]): Composed[B] = ???
    }
  }

  /**
   * 5.2 A Transformative Example
   */
  import cats.data.OptionT

  type ListOption[A] = OptionT[List, A]

  import cats.Monad
  import cats.instances.list._     // for Monad
  import cats.syntax.applicative._ // for pure

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  // result1: ListOption[Int] = OptionT(List(Some(10)))

  val result2: ListOption[Int] = 32.pure[ListOption]
  // result2: ListOption[Int] = OptionT(List(Some(32)))

  result1.flatMap { (x: Int) =>
    result2.map { (y: Int) =>
      x + y
    }
  }

  /**
   * 5.3 Monad Transformers in Cats
   */

  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A] = Either[String, A]

  // Build our final monad stack using OptionT:
  type ErrorOrOption[A] = OptionT[ErrorOr, A]


  /**
   * 5.3.2 Building Monad Stacks
   */
  import cats.instances.either._ // for Monad

  val a = 10.pure[ErrorOrOption]
  // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))

  val b = 32.pure[ErrorOrOption]
  // b: ErrorOrOption[Int] = OptionT(Right(Some(32)))

  val c = a.flatMap(x => b.map(y => x + y))
  // c: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(42)))

  import scala.concurrent.{Await, Future}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import cats.data.EitherT
  import cats.instances.future._

  type FutureEither[A] = EitherT[Future, String, A]

  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b


  /**
   * 5.3.3 Constructing and Unpacking Instances
   */
  // Create using apply:
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  // errorStack1: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(10)))
  println(errorStack1.value)

  // Create using pure:
  val errorStack2 = 32.pure[ErrorOrOption]
  // errorStack2: ErrorOrOption[Int] = OptionT(Right(Some(32)))
  println(errorStack2.value.map(_.getOrElse(-1)))



  trait FutureEitherOr {
    futureEitherOr
    // res14: FutureEitherOption[Int] = OptionT(EitherT(Future(Success(Right(Some(42))))))

    val intermediate = futureEitherOr.value
    // intermediate: FutureEither[Option[Int]] = EitherT(Future(Success(Right(Some(42)))))

    val stack = intermediate.value
    // stack: scala.concurrent.Future[Either[String,Option[Int]]] = Future(Success(Right(Some(42))))

    Await.result(stack, 1.second)
    // res15: Either[String,Option[Int]] = Right(Some(42))
  }

  /**
   * 5.3.4 Default Instances
   */
  import cats.Id
  import cats.data.{ReaderT, StateT, WriterT}

  type Reader[E, A] = ReaderT[Id, E, A] // = Kleisli[Id, E, A]
  type Writer[W, A] = WriterT[Id, W, A]
  type State[S, A]  = StateT[Id, S, A]

  /**
   * 5.3.5 Usage Patterns
   */
  sealed abstract class HttpError
  final case class NotFound(item: String) extends HttpError
  final case class BadRequest(msg: String) extends HttpError
  // etc...

//  type FutureEither[A] = EitherT[Future, HttpError, A]


  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  // This approach doesn't force OptionT on other users' code:
  val result11 = addAll("1", "2", "3")
  // result11: Logged[Option[Int]] = WriterT((List(Read 1, Read 2, Read 3),Some(6)))

  val result12 = addAll("1", "a", "3")
  // result12: Logged[Option[Int]] = WriterT((List(Read 1, Failed on a),None))

  /**
   * 5.4 Exercise: Monads: Transform and Roll Out
   */
  //  type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )
  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(x) => EitherT.rightT(x)
      case None => EitherT.leftT(s"Comms error: $autobot unreachable.")
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      a1 <- getPowerLevel(ally1)
      a2 <- getPowerLevel(ally2)
    } yield a1 + a2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 10.second) match {
      case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Left(s)      => s
    }
  }

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz", "Bumblebee"))
    // res28: String = Jazz and Bumblebee need a recharge.

    println(tacticalReport("Bumblebee", "Hot Rod"))
    // res29: String = Bumblebee and Hot Rod are ready to roll out!

    println(tacticalReport("Jazz", "Ironhide"))
    // res30: String = Comms error: Ironhide unreachable
  }
}
