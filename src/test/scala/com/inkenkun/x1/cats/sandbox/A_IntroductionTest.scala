package com.inkenkun.x1.cats.sandbox

import org.scalatest.{FlatSpec, Matchers}

class IntroductionTest extends FlatSpec with Matchers {

  "printableCat" should "return 'NAME is a AGE year-old COLOR cat.'" in {
    val cat = Cat("kitty", 5, "white")
    PrintableInstances.printableCat.format(cat) shouldBe "kitty is a 5 year-old white cat."
  }

  "PrintableSyntax" should "return 'NAME is a AGE year-old COLOR cat.'(same)" in {
    import PrintableSyntax._
    implicit val printable = new Printable[Cat] {
      override def format(a: Cat): String = s"${a.name} is a ${a.age} year-old ${a.color} cat."
    }
    Cat("mee", 7, "black").format shouldBe "mee is a 7 year-old black cat."
  }

  "ReImplementCat" should "return 'NAME is a AGE year-old COLOR cat.'(again)" in {
    import cats.implicits._
    import ReImplementCat.catShow
    Cat("chiyo", 2, "grey").show shouldBe "chiyo is a 2 year-old grey cat."
  }

  "123 === 123" should "return false" in {
    import cats.Eq
    import cats.instances.int._
    import cats.syntax.eq._

    val eqInt = Eq[Int]
    eqInt.eqv(123, 123) shouldBe true
    123 === "123" shouldBe false
  }
  "Some(1) === None" should "return false" in {
    Some(1) === None shouldBe false
  }
  "Some(Cat('Garfield')) === Some(Cat('Heathcliff'))" should "return false" in {
    import cats.syntax.option._

    val cat1 = Cat("Garfield",   38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    Equality.isCat1equalToCat2(cat1.some, cat2.some) shouldBe false
  }
  "Some(Cat('Garfield')) === None" should "return false" in {
    import cats.syntax.option._

    val cat1 = Cat("Garfield",   38, "orange and black")
//    val cat2 = Cat("Heathcliff", 33, "orange and black")

    Equality.isCat1equalToCat2(cat1.some, none[Cat]) shouldBe false
  }
  "None === None" should "return true" in {
    import cats.syntax.option._

    Equality.isCat1equalToCat2(none[Cat], none[Cat]) shouldBe true
  }
  "sequence" should "return List" in {
    import cats.implicits._
    val data: List[Option[Int]] = List(1.some, 10.some, None, 2.some, 20.some)
    val seq: Option[List[Int]] = data.sequence
    seq shouldBe List(1, 10, 2, 20).some
  }
}
