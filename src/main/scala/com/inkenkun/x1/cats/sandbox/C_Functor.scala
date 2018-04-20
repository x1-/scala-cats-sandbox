package com.inkenkun.x1.cats.sandbox

object FunctorFunc {

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._     // for map”

  val func1: Int => Double = (x: Int) => x.toDouble
  val func2: Double => Double = (y: Double) => y * 2

  val func =
    ((x: Int) => x.toDouble)
      .map(x => x + 1)
      .map(x => x * 2)
      .map(x => x + "!")

  def main(args: Array[String]): Unit = {
    println((func1 map func2)(1))
    println((func1 andThen func2)(1))
    println(func2(func1(1)))
    println(func(123))
  }

}

/**
  * 3.5 Functors in Cats
  */
object FunctorType {
  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._

  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)

  val func = (x: Int) => x + 1
  val liftedFunc = Functor[Option].lift(func)
  liftedFunc(Option(1))
}

object FunctorSyntax {
  import cats.Functor
  import cats.instances.function._
  import cats.instances.option._
  import cats.instances.list._
  import cats.syntax.functor._

  def main(args: Array[String]): Unit = {
    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a + 2
    val func3 = (a: Int) => a + "!"
    val func4 = func1.map(func2).map(func3)
    println(func4(123))
  }

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  doMath(Option(20))
  doMath(List(1, 2, 3))
}

object Branching {
  import cats.Functor
  import cats.syntax.functor._

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit def branchFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(l, r) => Branch(l.map(f), r.map(f))
      case Leaf(a) => Leaf(f(a))
    }
  }
}

/**
  * contravariant and invariant 反変と不変
  * covariant 共変
  * contramap 逆map
  */
object ContravariantAndInvariant {
  /**
    * 3.6 Contravariant and Invariant Functors
    */
  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String = self.format(func(value))
      }
  }

  def format[A](value: A)(implicit  p: Printable[A]): String = p.format(value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s""""$value""""
  }
  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = if (value) "yes" else "no"
  }

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)


  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def decode(value: String): B = dec(self.decode(value))
      override def encode(value: B): String = self.encode(enc(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
    stringCodec.imap(a => Box(codec.decode(a)), b => codec.encode(b.value))

  /**
    * 3.7.1 Contravariant in Cats
    */
  import cats.Contravariant
  import cats.Show
  import cats.instances.string._
  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString)((s: Symbol) => s"'${s.name}")

  /**
    * 3.7.2 Invariant in Cats
    */
  import cats.Monoid
//  import cats.instances.string._ // for Monoid
  import cats.syntax.invariant._ // for imap
  import cats.syntax.semigroup._ // for |+|

  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)

  def main(args: Array[String]): Unit = {

    println("3.6.1.1 Exercise: Showing off with Contramap")
    println(format("hello"))
    println(format(true))
    println(format(Box("hello world")))
    println(format(Box(true)))

    println("3.6.2.1 Transformative Thinking with imap")
    println(encode(123.4))
    println(decode[Double]("123.4"))
    println(encode(Box(123.4)))
    println(decode[Box[Double]]("123.4"))

    println("3.7.1 Contravariant in Cats")
    println(showSymbol.show('dave))
    import cats.syntax.contravariant._
    println(showString.contramap[Symbol](_.name).show('dave))

    println("3.7.2 Invariant in Cats")
    println(Monoid[Symbol].empty)
    println('a |+| 'few |+| 'words)
  }
}


object Aside {
  import cats.syntax.contravariant._
  import cats.instances.function._
  import cats.instances.double._

  val func1 = (a: Int) => a + 1d
  val func2 = (a: Int) => a + 2d
  val func3 = (a: Int) => a + "!"

//  val func3a: Int => Double =
//    a => func2(func1(a))

//  val func3b: Int => Double = func2.compose(func1)
//
//  import cats.syntax.contravariant._
//  val func3c = func2.contramap(func1)

  type <=[B, A] = A => B
  type F[A] = Double <= A
  val func2b: Double <= Double = (a: Double) => a + 2d
  val func3c = func2b.contramap(func1)
}
