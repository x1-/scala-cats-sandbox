package com.inkenkun.x1.cats.sandbox

trait Printable[A] {
  def format(a: A): String
}
final case class Cat(name: String, age: Int, color: String)

object PrintableInstances {

  implicit val printableString = new Printable[String] {
    override def format(a: String): String = a
  }
  implicit val printableInt = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }

  implicit val printableCat = new Printable[Cat] {
    override def format(a: Cat): String = s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }
}

object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String =
    printable.format(a)

  def print[A](a: A)(implicit printable: Printable[A]): Unit =
    println(printable.format(a))
}

object PrintableSyntax {
  implicit class PrintableOps[A](val a: A) extends AnyVal {
    def format(implicit printable: Printable[A]): String =
      printable.format(a)

    def print(implicit printable: Printable[A]): Unit =
      println(printable.format(a))
  }
}

object ReImplementCat {
  import cats._

  implicit val catShow: Show[Cat] =
    Show.show(a => s"${a.name} is a ${a.age} year-old ${a.color} cat.")
}

object Equality {
  import cats.Eq
  import cats.syntax.eq._

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      ((cat1.name == cat2.name)
      && (cat1.age == cat2.age)
      && (cat1.color == cat2.color))
    }
  implicit val optionCatEq: Eq[Option[Cat]] =
    Eq.instance[Option[Cat]] {
      case (Some(cat1), Some(cat2)) =>
        ((cat1.name == cat2.name)
          && (cat1.age == cat2.age)
          && (cat1.color == cat2.color))
      case (None, None) => true
      case (_, _) => false
    }

  def isCat1equalToCat2(cat1: Option[Cat], cat2: Option[Cat]) = {
    cat1 === cat2
  }
}
