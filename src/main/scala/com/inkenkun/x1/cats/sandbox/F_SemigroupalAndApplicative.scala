package com.inkenkun.x1.cats.sandbox

import java.io

/**
 * 6.1 Semigroupal
 */
object SemigroupalSection {
  def main(args: Array[String]): Unit = {
    import cats.Semigroupal
    import cats.instances.option._ // for Semigroupal

    println(Semigroupal[Option].product(Some(123), Some("abc")))
    // res0: Option[(Int, String)] = Some((123,abc))

    println(Semigroupal[Option].product(None, Some("abc")))
    // res1: Option[(Nothing, String)] = None

    println(Semigroupal[Option].product(Some(123), None))
    // res2: Option[(Int, Nothing)] = None

    println(Semigroupal.tuple3(Option(1), Option(2), Option(3)))
    // res3: Option[(Int, Int, Int)] = Some((1,2,3))

    println(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]))
    // res4: Option[(Int, Int, Int)] = None

    println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))
    // res5: Option[Int] = Some(6)

    println(Semigroupal.map2(Option(1), Option.empty[Int])(_ + _))
    // res6: Option[Int] = None

  }
}

/**
 * 6.2 Apply Syntax
 */
object SemigroupalApplySyntaxSection {
  import cats.instances.option._ // for Semigroupal
  import cats.syntax.apply._     // for tupled and mapN

  def main(args: Array[String]): Unit = {

    println((Option(123), Option("abc")).tupled)
    println((Option(123), Option("abc"), Option(true)).tupled)

    case class Cat1(name: String, born: Int, color: String)
    println((
      Option("Garfield"),
      Option(1978),
      Option("Orange & black")
    ).mapN(Cat1.apply))
    // res9: Option[Cat1] = Some(Cat1(Garfield,1978,Orange & black))

    import cats.instances.boolean._ // for Monoid
    import cats.instances.int._     // for Monoid
    import cats.instances.list._    // for Monoid
    import cats.instances.string._  // for Monoid
    import cats.syntax.all._
//    import cats.syntax.apply._      // for imapN
    import cats.{Monoid, Semigroupal}
    import cats.instances.invariant._

    case class Cat(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
    )

    val tupleToCat: (String, Int, List[String]) => Cat =
      Cat.apply _

    val catToTuple: Cat => (String, Int, List[String]) =
      cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

//    import cats.syntax.semigroup._ // for |+|

    val garfield   = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

    garfield |+| heathcliff
    // res17: Cat = Cat(GarfieldHeathcliff,3966,List(Lasagne, Junk Food))

    import cats.instances.future._ // for Semigroupal
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.language.higherKinds

    val futurePair = Semigroupal[Future].
      product(Future("Hello"), Future(123))

    println(Await.result(futurePair, 1.second))
    // res1: (String, Int) = (Hello,123)

    val futureCat = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
    ).mapN(Cat.apply)

    println(Await.result(futureCat, 1.second))
    // res4: Cat = Cat(Garfield,1978,List(Lasagne))

    println(Semigroupal[List].product(List(1, 2), List(3, 4)))

    import cats.instances.either._ // for Semigroupal

    type ErrorOr[A] = Either[Vector[String], A]

    println(Semigroupal[ErrorOr].product(
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    ))

    /**
     * 6.3.1.1 Exercise: The Product of Monads
     */
    import cats.Monad

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      for {
        ix <- x
        iy <- y
      } yield (ix, iy)

    println(product(List(1,2), List("A", "B")))
    println(product(Left(Vector("Error 1")), Right(55)))
    println(product(Future("Future 1"), Future(5.55)))

    import cats.data.Validated

    type AllErrorsOr[A] = Validated[List[String], A]

    println(Semigroupal[AllErrorsOr].product(
      Validated.invalid(List("Error 1")),
      Validated.invalid(List("Error 2"))
    ))

    val v = Validated.Valid(123)
    // v: cats.data.Validated.Valid[Int] = Valid(123)

    val i = Validated.Invalid(List("Badness"))
    // i: cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))

    val v2 = Validated.valid[List[String], Int](123)
    // v: cats.data.Validated[List[String],Int] = Valid(123)

    val i2 = Validated.invalid[List[String], Int](List("Badness"))
    // i: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

//    import cats.syntax.validated._ // for valid and invalid

    123.valid[List[String]]
    // res2: cats.data.Validated[List[String],Int] = Valid(123)

    List("Badness").invalid[Int]
    // res3: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

//    import cats.syntax.applicative._      // for pure
//    import cats.syntax.applicativeError._ // for raiseError

    type ErrorsOr[A] = Validated[List[String], A]

    123.pure[ErrorsOr]
    // res5: ErrorsOr[Int] = Valid(123)

    List("Badness").raiseError[ErrorsOr, Int]
    // res6: ErrorsOr[Int] = Invalid(List(Badness))

    Validated.catchOnly[NumberFormatException]("foo".toInt)
    // res7: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

    Validated.catchNonFatal(sys.error("Badness"))
    // res8: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeException: Badness)

    Validated.fromTry(scala.util.Try("foo".toInt))
    // res9: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

    Validated.fromEither[String, Int](Left("Badness"))
    // res10: cats.data.Validated[String,Int] = Invalid(Badness)

    Validated.fromOption[String, Int](None, "Badness")
    // res11: cats.data.Validated[String,Int] = Invalid(Badness)

    type AllErrorsOr2[A] = Validated[String, A]

    import cats.instances.string._ // for Semigroup
    import cats.instances.vector._ // for Semigroupal

    Semigroupal[AllErrorsOr2]

    (
      "Error 1".invalid[Int],
      "Error 2".invalid[Int]
    ).tupled

    (
      Vector(404).invalid[Int],
      Vector(500).invalid[Int]
    ).tupled
    // res15: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, Int)] = Invalid(Vector(404, 500))

    import cats.data.NonEmptyVector

    (
      NonEmptyVector.of("Error 1").invalid[Int],
      NonEmptyVector.of("Error 2").invalid[Int]
    ).tupled

    123.valid.map(_ * 100)
    // res17: cats.data.Validated[Nothing,Int] = Valid(12300)

    "?".invalid.leftMap(_.toString)
    // res18: cats.data.Validated[String,Nothing] = Invalid(?)

    123.valid[String].bimap(_ + "!", _ * 100)
    // res19: cats.data.Validated[String,Int] = Valid(12300)

    "?".invalid[Int].bimap(_ + "!", _ * 100)
    // res20: cats.data.Validated[String,Int] = Invalid(?!)

//    import cats.syntax.either._ // for toValidated
    // import cats.syntax.either._

    "Badness".invalid[Int]
    // res21: cats.data.Validated[String,Int] = Invalid(Badness)

    "Badness".invalid[Int].toEither
    // res22: Either[String,Int] = Left(Badness)

    "Badness".invalid[Int].toEither.toValidated
    // res23: cats.data.Validated[String,Int] = Invalid(Badness)

    41.valid[String].withEither(_.flatMap(n => Right(n + 1)))
    // res24: cats.data.Validated[String,Int] = Valid(42)

    123.valid[String].ensure("Negative!")(_ > 0)

    "fail".invalid[Int].getOrElse(0)
    // res26: Int = 0

    "fail".invalid[Int].fold(_ + "!!!", _.toString)
    // res27: String = fail!!!
  }
}

/**
 * “6.4.4 Exercise: Form Validation
 */
object SemigroupalExercise {
  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._
  import cats.syntax.either._
  import cats.syntax.validated._

  type AllErrorsOr[A] = Validated[List[String], A]
  type ErrorOr[A] = Either[List[String], A]

  case class User(name: String, age: Int)
  def getValue(params: Map[String, String], key: String): ErrorOr[String] =
    params
      .get(key)
      .toRight(List(s"$key was not found."))

  def parseInt(value: String): ErrorOr[Int] =
    Either.catchOnly[NumberFormatException](value.toInt)
      .leftMap(e => List(s"Cannot parse $value to Int.: ${e.getMessage}"))

  def nonBlank(value: String): ErrorOr[String] =
    value
      .valid[List[String]]
      .ensure(List("name is blank."))(_.nonEmpty)
      .toEither

  def nonNegative(value: Int): ErrorOr[Int] =
    value
      .valid[List[String]]
      .ensure(List(s"$value is negative value."))(_ >= 0)
      .toEither

  def readName(params: Map[String, String]): ErrorOr[String] =
    getValue(params, "name")
      .flatMap(nonBlank)

  def readAge(params: Map[String, String]): ErrorOr[Int] =
    getValue(params, "age")
      .flatMap(nonBlank)
      .flatMap(parseInt)
      .flatMap(nonNegative)

  def productUser(params: Map[String, String]): Validated[List[String], User] =
    Semigroupal[AllErrorsOr].product(
      readName(params).toValidated,
      readAge(params).toValidated
    ).map { case (name, age) => User(name, age) }

  def main(args: Array[String]): Unit = {
    val params1 = Map(
      "name" -> "Garfield",
      "age"  -> "-10"
    )
    println(productUser(params1))

    val params2 = Map(
      "age"  -> "10"
    )
    println(productUser(params2))

    val params3 = Map(
      "name" -> "Lasagne",
      "age"  -> "seven"
    )
    println(productUser(params3))

    val params4 = Map(
      "name" -> "",
      "age"  -> "-10"
    )
    println(productUser(params4))
  }
}