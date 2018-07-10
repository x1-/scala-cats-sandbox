package com.inkenkun.x1.cats.sandbox

import cats.{Eval, Foldable, Monad, Monoid, Traverse}
import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.semigroup._
import cats.syntax.traverse._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object I_MapReduce {

  /**
   * 9.2 Implementing foldMap
   */
  def foldMap[A, B: Monoid](xs: Vector[A])(f: A => B): B = {
    xs.foldLeft(Monoid[B].empty){(acc, x) =>
      Monoid[B].combine(acc, f(x))
    }
  }

  /**
   * result
   */
  def foldMapResult[A, B : Monoid](as: Vector[A])(func: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| func(_))

  /**
   * 9.3.3 Implementing parallelFoldMap
   */
  def parallelFoldMap[A, B : Monoid]
    (values: Vector[A])
    (func: A => B): Future[B] = {
    val num = values.size / Runtime.getRuntime.availableProcessors
    val size = if (values.size % Runtime.getRuntime.availableProcessors == 0) num else num + 1

    values
      .grouped(size)
      .toList
      .map { xs: Vector[A] =>
        Future { foldMap(xs)(func) }
      }
      .sequence[Future, B]
      .map { xs: List[B] =>
        xs.foldLeft(Monoid[B].empty)(_ |+| _)
      }
  }

  /**
   * 9.3.4 parallelFoldMap with more Cats
   */
  def parallelFoldMapMoreCats[A, B : Monoid]
    (values: Vector[A])
    (func: A => B): Future[B] = {
    val num = values.size / Runtime.getRuntime.availableProcessors
    val size = if (values.size % Runtime.getRuntime.availableProcessors == 0) num else num + 1

    val mapped: Iterator[Future[B]] = values
      .grouped(size)
      .map { vec: Vector[A] =>
        Future {
          Foldable[Vector]
            .foldMap(vec)(func)
        }
      }

    Traverse[List]
      .sequence[Future, B](mapped.toList)
      .map(xs => xs.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  /**
   * results
   */
//  def parallelFoldMap[A, B: Monoid]
//    (values: Vector[A])
//    (func: A => B): Future[B] = {
//    val numCores  = Runtime.getRuntime.availableProcessors
//    val groupSize = (1.0 * values.size / numCores).ceil.toInt
//
//    values
//      .grouped(groupSize)
//      .toVector
//      .traverse(group => Future(group.toVector.foldMap(func)))
//      .map(_.combineAll)
//  }
//
//  val future: Future[Int] =
//    parallelFoldMap((1 to 1000).toVector)(_ * 1000)
//
//  Await.result(future, 1.second)
//  // res3: Int = 500500000

  def main(args: Array[String]): Unit = {
    println(foldMap(Vector(1, 2, 3))(identity))
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

    println(Monoid[Future[Int]].combine(Future(1), Future(2)))
    println((1 to 10).toList.grouped(3).toList)
    println(parallelFoldMap((1 to 10).toVector)(_.toString + "! "))
    println(parallelFoldMap(Vector(1, 2, 3))(_.toString + "! "))
//    println(parallelFoldMap(Vector.empty[Int])(_.toString + "! "))
    println(parallelFoldMapMoreCats((1 to 10).toVector)(_.toString + "! "))
  }
}
