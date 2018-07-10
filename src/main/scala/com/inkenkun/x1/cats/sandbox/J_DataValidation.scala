package com.inkenkun.x1.cats.sandbox

import cats.Monoid
import cats.data.Validated
import cats.syntax.all._

object J_DataValidation {

  trait Check[E, A] {
    def apply(value: A): Validated[E, A]

    def or(that: Check[E, A])(implicit monoid: Monoid[E]): Check[E, A] = (a: A) => (this(a), that(a)) match {
      case (Validated.Invalid(e1), Validated.Invalid(e2)) => Validated.Invalid(monoid.combine(e1, e2))
      case (Validated.Valid(r), _) => Validated.Valid(r)
      case (_, Validated.Valid(r)) => Validated.Valid(r)
    }
  }
}
