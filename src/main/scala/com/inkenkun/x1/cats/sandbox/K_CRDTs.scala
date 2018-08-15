package com.inkenkun.x1.cats.sandbox

/**
 * 11. CRDTs
 * see: [CRDT (Conflict-free Replicated Data Type)を15分で説明してみる](https://qiita.com/everpeace/items/bb73ec64d3e682279d26)
 */
object K_CRDTs {

  /**
   * 11.2.3 Exercise: GCounter Implementation
   */
  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): GCounter =
      GCounter(this.counters.updated(machine, this.counters.getOrElse(machine, 0) + amount))

    def merge(that: GCounter): GCounter = {
      val machines = (this.counters.keys ++ that.counters.keys)(scala.collection.breakOut) : Set[String]
      GCounter(
        machines.toIterator.map { machine =>
          machine -> Seq(this.counters.get(machine), that.counters.get(machine)).max.getOrElse(0)
        } toMap
      )
    }

    def total: Int =
      this.counters.values.sum
  }

  def main(args: Array[String]): Unit = {
    val gc1 = GCounter(Map("host1" -> 1, "host2" -> 2)).increment("host1", 2)
    println(gc1)
    val gc2 = GCounter(Map("host1" -> 0, "host2" -> 3, "host3" -> 1))
    println(gc1.merge(gc2))

  }
}

/**
 * 11.3 Generalisation
 */
object Generalisation {

  /**
   * 11.3.2 Exercise: BoundedSemiLattice Instances
   */
  import cats.Monoid

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
    def max(a1: A, a2: A): A
  }

  object BoundedSemiLattice {
    implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 + a2
      def empty: Int = 0
      def max(a1: Int, a2: Int): Int = a1 max a2
    }
    implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
      def empty: Set[A] = Set.empty[A]
      def max(a1: Set[A], a2: Set[A]): Set[A] = if (a1.size >= a2.size) a1 else a2
    }
  }

  /**
   * 11.3.3 Exercise: Generic GCounter
   */
  case class GCounter[A, B](counters: Map[A, B]) {
    def increment(machine: A, amount: B)(implicit m: Monoid[B]): GCounter[A, B] = {
      GCounter(
        this.counters.updated(
          machine,
          m.combine(this.counters.getOrElse(machine, m.empty), amount))
      )
    }
    def merge(that: GCounter[A, B])(implicit b: BoundedSemiLattice[B]): GCounter[A, B] = {
      val machines = this.counters ++ that.counters
      GCounter(
        machines.map { case (key, value) =>
          key -> b.max(value, this.counters.getOrElse(key, b.empty))
        }
      )
    }
    // result
//    def merge(that: GCounter[A])
//      (implicit b: BoundedSemiLattice[A]): GCounter[A] =
//      GCounter(this.counters |+| that.counters)

    def total(implicit m: Monoid[B]): B =
      m.combineAll(this.counters.values)
  }


  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    val gc1 = GCounter(Map("host1" -> 1, "host2" -> 2)).increment("host1", 2)
    println(gc1)
    val gc2 = GCounter(Map("host1" -> 0, "host2" -> 3, "host3" -> 1))
    println(gc1.merge(gc2))

  }
}

/**
 * 11.4 Abstracting GCounter to a Type Class
 */
object AbstractingGCounter {

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.list._
  import cats.instances.map._
  import cats.syntax.semigroup._
  import cats.syntax.foldable._

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
    def max(a1: A, a2: A): A
  }
  object BoundedSemiLattice {
    implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 + a2
      def empty: Int = 0
      def max(a1: Int, a2: Int): Int = a1 max a2
    }
    implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
      def empty: Set[A] = Set.empty[A]
      def max(a1: Set[A], a2: Set[A]): Set[A] = if (a1.size >= a2.size) a1 else a2
    }
  }

  trait GCounter[F[_,_],K, V] {
    def increment(f: F[K, V])(k: K, v: V)
      (implicit m: Monoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])
      (implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])
      (implicit m: Monoid[V]): V
  }

  object GCounter {
    def apply[F[_,_], K, V]
    (implicit counter: GCounter[F, K, V]) =
      counter
  }

  implicit def mapGCounter[K, V] = new GCounter[Map, K, V] {
    def increment(f: Map[K, V])(k: K, v: V)
      (implicit m: Monoid[V]): Map[K, V] =
        f + (k -> (f.getOrElse(k, m.empty) |+| v))

    def merge(f1: Map[K, V], f2: Map[K, V])
      (implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1.map { case (key, value) =>
          key -> b.max(value, f2.getOrElse(key, b.empty))
        }

    def total(f: Map[K, V])
      (implicit m: Monoid[V]): V =
        m.combineAll(f.values)
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.int._ // for Monoid

    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter = GCounter[Map, String, Int]

    val merged = counter.merge(g1, g2)
    // merged: Map[String,Int] = Map(a -> 7, b -> 5)
    println(merged)

    val total  = counter.total(merged)
    // total: Int = 12
    println(total)
  }
}

/**
 * 11.5 Abstracting a Key Value Store
 */
object AbstractingAKeyValueStore {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.list._
  import cats.instances.map._
  import cats.syntax.semigroup._
  import cats.syntax.foldable._

  trait KeyValueStore[F[_,_]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  implicit val mapKeyValueStore = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
      f + (k -> v)

    def get[K, V](f: Map[K, V])(k: K): Option[V] =
      f.get(k)

    def values[K, V](f: Map[K, V]): List[V] =
      f.values.toList
  }


}
