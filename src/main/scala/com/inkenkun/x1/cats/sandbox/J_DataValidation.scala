package com.inkenkun.x1.cats.sandbox


object J_DataValidation {

//  type Check[E, A] = A => Either[E, A]

  /**
   * 10.3 Basic Combinators
   */
  import cats.Semigroup
  import cats.data.Validated
  import cats.instances.list._
  import cats.syntax.semigroup._
  import cats.syntax.validated._

  trait Check[E, A] {
    def apply(value: A): Validated[E, A]

    def and(that: Check[E, A])(implicit semigroup: Semigroup[E]): Check[E, A] = (a: A) => (this(a), that(a)) match {
      case (Validated.Invalid(e1), Validated.Invalid(e2)) => Validated.Invalid(e1 |+| e2)
      case (Validated.Invalid(e1), _) => Validated.Invalid(e1)
      case (_, Validated.Invalid(e2)) => Validated.Invalid(e2)
      case (Validated.Valid(_), Validated.Valid(_)) => Validated.Valid(a)
    }

    def or(that: Check[E, A])(implicit semigroup: Semigroup[E]): Check[E, A] = (a: A) => (this(a), that(a)) match {
      case (Validated.Invalid(e1), Validated.Invalid(e2)) => Validated.Invalid(e1 |+| e2)
      case (Validated.Valid(r), _) => Validated.Valid(r)
      case (_, Validated.Valid(r)) => Validated.Valid(r)
    }
  }

  def main(args: Array[String]): Unit = {

    val checkNumeric = new Object with Check[List[String], String] {
      override def apply(value: String): Validated[List[String], String] = {
        Validated.fromTry(scala.util.Try(value.toInt).map(_.toString))
          .leftMap(e => List(e.getMessage))
      }
    }
    val checkLength = new Object with Check[List[String], String] {
      override def apply(value: String): Validated[List[String], String] = {
        value.valid[List[String]].ensure(List(s"The length of charachters are not enough: $value is ${value.size}"))(_.size > 3)
      }
    }
    println(checkNumeric.and(checkLength).apply("12345"))
    println(checkNumeric.and(checkLength).apply("123"))
    println(checkNumeric.and(checkLength).apply("abc"))
    println(checkNumeric.or(checkLength).apply("12345"))
    println(checkNumeric.or(checkLength).apply("12"))
    println(checkNumeric.or(checkLength).apply("abcdef"))
    println(checkNumeric.or(checkLength).apply("ab"))
  }
}

object MakeSenseOfPredicates {
  /**
   * 10.4.1 Predicates
   */
  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._   // for Valid and Invalid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._     // for mapN
  import cats.syntax.validated._

  sealed trait Predicate[E, A] {
    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case PredPure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(a1)   => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a2)   => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  final case class And[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]) extends Predicate[E, A]

  final case class PredPure[E, A](
    func: A => Validated[E, A]) extends Predicate[E, A]

  /**
   * 10.4.2 Checks
   */
  sealed trait Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
      FlatMap[E, A, B, C](this, f)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, that)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      Pure(pred)
  }

  final case class Pure[E, A](
    pred: Predicate[E, A]
  ) extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(in)
  }

  final case class Map[E, A, B, C](
    check: Check[E, A, B],
    func: B => C
  ) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }
  final case class FlatMap[E, A, B, C](
    check: Check[E, A, B],
    func: B => Check[E, A, C]
  ) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).withEither(_.flatMap(x => func(x)(in).toEither))
  }
  final case class AndThen[E, A, B, C](
    check: Check[E, A, B],
    that: Check[E, B, C]
  ) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).andThen(b => that(b))
  }
}

object MakeSenseOfRecap {
  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._   // for Valid and Invalid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._     // for mapN
  import cats.syntax.validated._

  sealed trait Predicate[E, A] {
    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(a1)   => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a2)   => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Predicate {
    final case class And[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](
      func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
      Pure(a => if(fn(a)) a.valid else err.invalid)
  }

  sealed trait Check[E, A, B] {
    import Check._

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]) =
      FlatMap[E, A, B, C](this, f)

    def andThen[C](next: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, next)
  }

  object Check {
    final case class Map[E, A, B, C](
      check: Check[E, A, B],
      func: B => C) extends Check[E, A, C] {

      def apply(a: A)
        (implicit s: Semigroup[E]): Validated[E, C] =
        check(a) map func
    }

    final case class FlatMap[E, A, B, C](
      check: Check[E, A, B],
      func: B => Check[E, A, C]) extends Check[E, A, C] {

      def apply(a: A)
        (implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }

    final case class AndThen[E, A, B, C](
      check: Check[E, A, B],
      next: Check[E, B, C]) extends Check[E, A, C] {

      def apply(a: A)
        (implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => next(b).toEither))
    }

    final case class Pure[E, A, B](
      func: A => Validated[E, B]) extends Check[E, A, B] {

      def apply(a: A)
        (implicit s: Semigroup[E]): Validated[E, B] =
        func(a)
    }

    final case class PurePredicate[E, A](
      pred: Predicate[E, A]) extends Check[E, A, A] {

      def apply(a: A)
        (implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      PurePredicate(pred)

    def apply[E, A, B]
    (func: A => Validated[E, B]): Check[E, A, B] =
      Pure(func)
  }

  import cats.data.NonEmptyList

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  def splitting: Check[Errors, String, (String, String)] = {
    val split: (String) => Validated[Errors, (String, String)] = { s =>
      s.split('@').toList match {
        case h :: tail if !tail.isEmpty => Valid((h, tail.head))
        case _ => Invalid(error("Must contain the character @"))
      }
    }
    Check(s => split(s))
  }
  val checkLeft: Check[Errors, String, String] =
    Check(longerThan(0))

  val checkRight: Check[Errors, String, String] =
    Check(longerThan(3) and contains('.'))

  val joinEmail: Check[Errors, (String, String), String] =
    Check { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkUsername: Check[Errors, String, String] =
    Check(longerThan(4).and(alphanumeric))

  val checkEmail: Check[Errors, String, String] =
    splitting andThen joinEmail

  final case class User(username: String, email: String)
  def createUser(
    username: String,
    email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  def main(args: Array[String]): Unit = {
    createUser("Noel", "noel@underscore.io")
    createUser("", "dave@underscore@io")
  }
}

/**
 * 10.5 Kleislis
 */
object Kleislis {
  import cats.data.Kleisli
  import cats.instances.list._ // for Monad


  def main(args: Array[String]): Unit = {
    val step1: Kleisli[List, Int, Int] =
      Kleisli(x => List(x + 1, x - 1))

    val step2: Kleisli[List, Int, Int] =
      Kleisli(x => List(x, -x))

    val step3: Kleisli[List, Int, Int] =
      Kleisli(x => List(x * 2, x / 2))

    val pipeline = step1 andThen step2 andThen step3

    println(pipeline.run(20))
  }
}