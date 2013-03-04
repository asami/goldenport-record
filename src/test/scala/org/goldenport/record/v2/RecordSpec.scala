package org.goldenport.record.v2

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.matchers._

/**
 * @since   Feb. 16, 2013
 * @version Mar.  4, 2013
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PlainTableTest extends WordSpec with ShouldMatchers {
  "Record" should {
    "asString" that {
      "plain" in {
        val r = Record(List(Field(Symbol("b"), List("ab"))))
        r.asString('b) should be ("ab")
      }
      "symbol using '.'" in {
        val r = Record(List(Field(Symbol("a.b"), List("ab"))))
        r.asString('b) should be ("ab")
      }
    }
    "get" that {
      "plain" in {
        val r = Record(List(Field(Symbol("b"), List("ab"))))
        r.get("b") should be(Some(Seq("ab")))
      }
    }
    "getOne" that {
      "plain" in {
        val r = Record(List(Field(Symbol("b"), List("ab"))))
        val x = r.getOne("b")
        println("plain = " + x)
        x should be(Some("ab"))
      }
    }
  }
}
