package com.inkenkun.x1.cats.sandbox

object G_FoldableAndTraverse {
  /**
   * 7.1 Foldable
   */
  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  def main(args: Array[String]): Unit = {
    println(show(Nil))
    println(show(List(1, 2, 3)))

    println(List(1, 2, 3).foldLeft(0)(_ + _))
    // res2: Int = 6

    println(List(1, 2, 3).foldRight(0)(_ + _))
    // res3: Int = 6

    println(List(1, 2, 3).foldLeft(0)(_ - _))
    // res4: Int = -6

    println(List(1, 2, 3).foldRight(0)(_ - _))

    /**
     * 7.1.2 Exercise: Reflecting on Folds
     */
    println(List(1, 2, 3).foldLeft(List.empty[Int])((acc, n) => n :: acc))
    println(List(1, 2, 3).foldRight(List.empty[Int])((n, acc) => n :: acc))

    /**
     * 7.1.3 Exercise: Scaf-fold-ing Other Methods
     */
    println(map(List(1, 2, 3))(_.toString))
    println(flatMap(List(1, 2, 3))(a => List(a.toString)))
    println(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))
    println(sum(List(1, 2, 3, 4, 5)))
  }
  /**
   * 7.1.3 Exercise: Scaf-fold-ing Other Methods
   */
  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List.empty[B])((a, acc) => f(a) :: acc)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])((a, acc) => f(a) ++ acc)

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    xs.foldRight(List.empty[A]){(a, acc) =>
      if (f(a)) a :: acc
      else acc
    }
  def sum[A](xs: List[A])(implicit num: Numeric[A]): A =
    xs.foldRight(num.zero)((a, acc) => num.plus(a, acc))
}

/**
 * 7.1.4 Foldable in Cats
 */
object FoldableInCats {
  import cats.{Eval, Foldable}
  import cats.instances.int._
  import cats.instances.list._ // for Foldable
  import cats.instances.option._ // for Foldable
  import cats.instances.stream._ // for Foldable
  import cats.instances.string._
  import cats.instances.vector._

  def bigData = (1 to 100000).toStream

  def main(args: Array[String]): Unit = {
    val ints = List(1, 2, 3)

    Foldable[List].foldLeft(ints, 0)(_ + _)
    // res1: Int = 6
    val maybeInt = Option(123)

    Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

    val eval: Eval[Long] = Foldable[Stream].foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
    println(eval.value)

    println(Foldable[List].combineAll(List(1, 2, 3)))
    println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))


    val ints2 = List(Vector(1, 2, 3), Vector(4, 5, 6))

    println((Foldable[List] compose Foldable[Vector]).combineAll(ints2))
  }
}

/**
 * 7.2 Traverse
 */
object TraverseInCats {
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  def main(args: Array[String]): Unit = {
    val allUptimes: Future[List[Int]] =
      Future.traverse(hostnames)(getUptime)

    val result = Await.result(allUptimes, 1.second)
    println(result)

    /**
     * 7.2.2 Traversing with Applicatives
     */
    import cats.Applicative
    import cats.instances.future._   // for Applicative
    import cats.syntax.applicative._ // for pure
    import cats.syntax.apply._

    List.empty[Int].pure[Future]
    def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
      (accum, getUptime(host)).mapN(_ :+ _)

    import scala.language.higherKinds
    def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
        (accum, func(item)).mapN(_ :+ _)
      }

    def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
      listTraverse(list)(identity)

    val totalUptime = listTraverse(hostnames)(getUptime)

    Await.result(totalUptime, 1.second)

    /**
     * 7.2.2.1 Exercise: Traversing with Vectors
     */
    import cats.instances.vector._ // for Applicative
    println(listSequence(List(Vector(1, 2), Vector(3, 4))))
    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

    /**
     * 7.2.2.2 Exercise: Traversing with Options
     */
    import cats.instances.option._ // for Applicative

    def process(inputs: List[Int]) =
      listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

    println(process(List(2, 4, 6)))
    println(process(List(1, 2, 3)))

    /**
     * 7.2.2.3 Exercise: Traversing with Validated
     */
    import cats.data.Validated
    import cats.instances.list._ // for Monoid

    type ErrorsOr[A] = Validated[List[String], A]

    def process_validated(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if(n % 2 == 0) {
          Validated.valid(n)
        } else {
          Validated.invalid(List(s"$n is not even"))
        }
      }
    println(process_validated(List(2, 4, 6)))
    println(process_validated(List(1, 2, 3)))

    /**
     * 7.2.3 Traverse in Cats
     */
    import cats.Traverse
    import cats.instances.future._
    import cats.instances.list._

    val totalUptime2 = Traverse[List].traverse(hostnames)(getUptime)
    val res3 = Await.result(totalUptime2, 1.second)
    println(res3)

    val numbers = List(Future(1), Future(2), Future(3))
    val numbers2 : Future[List[Int]] = Traverse[List].sequence(numbers)
    val res4 = Await.result(numbers2, 1.second)
    println(res4)

  }
//  object TraverseInCats {
//
//    def main(args: Array[String]): Unit = {
//    }
//  }
}