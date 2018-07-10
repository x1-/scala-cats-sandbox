package com.inkenkun.x1.cats.sandbox

import cats.{Applicative, Id, Monad}
import cats.instances.future._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._  // for traverse
import cats.instances.either
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int]

}

trait TestUptimeClient extends UptimeClient[Id] {
  override def getUptime(hostname: String): Id[Int]
}
class TestUptimeClient2(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient2(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
}
class UptimeServiceTest extends WordSpec with Matchers {

  "UptimeService#getTotalUptime" when {
    "all hosts exist" should {
      "produce the sum of  uptime of all hosts" in {

        val hosts = Map(
          "host1" -> 10,
          "host2" -> 6
        )
        val client = new TestUptimeClient2(hosts)
        val service = new UptimeService(client)

        val expected = hosts.values.sum // = 16
        val actual = service.getTotalUptime(hosts.keys.toList)

        actual should equal (expected) // ??? <= 何が問題でしょうか?
      }
    }
  }
}
