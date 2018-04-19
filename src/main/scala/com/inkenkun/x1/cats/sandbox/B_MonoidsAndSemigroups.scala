package com.inkenkun.x1.cats.sandbox

object MonoidsAndSemigroups {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }

  trait BooleanMonoid1 extends Monoid[Boolean] {  // 論理積
  override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  trait BooleanMonoid2 extends Monoid[Boolean] {  // 論理和
  override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  trait BooleanMonoid3 extends Monoid[Boolean] {
    /** exclusive or
      * x もしくは y の値が異なるとtrue , 同じだとfalse
      */
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }
  trait BooleanMonoid4 extends Monoid[Boolean] {
    /** exclusive nor
      * x もしくは y の値が異なるとfalse , 同じだとtrue
      */
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)
  }

  trait SetSemigroup[A] extends Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] =
      x union y
  }
  trait SetMonoid1[A] extends Monoid[Set[A]] with SetSemigroup[A] {
    override def empty: Set[A] = Set.empty[A]
  }

}


object MonoidInstances {
  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.semigroup._

  Monoid[String].combine("Hi ", "there")
  val s = "Hi " |+| "there" |+| Monoid[String].empty
}

object SuperAdder {
  import cats.Monoid
  import cats.instances.boolean._
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.semigroup._
  import cats.syntax.option._

  def add(items: List[Int]): Int = items.sum
//  def addOptionInt(items: List[Option[Int]]): Option[Int] = items.foldLeft(Monoid[Option[Int]].empty)(_ |+| _)

  def addA[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)
  def addOptionInt(items: List[Option[Int]]): Option[Int] = addA(items)

  case class Order(totalCost: Double, quantity: Double)
  implicit val OrderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0d, 0d)
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
  def addOrder(items: List[Order]): Order = items.foldLeft(Monoid[Order].empty)(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(add(List(7, 8, 9)))
    println(addOptionInt(List(10.some, 11.some, 12.some)))
    println(addOrder(List(Order(1d, 10d), Order(2d, 20d), Order(3d, 40d))))
  }
}
