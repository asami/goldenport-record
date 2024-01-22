package org.goldenport.record.query

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import scalaz.{Store => _, _}, Scalaz._
import org.goldenport.RAISE
import org.goldenport.record.v2.{Record => Record2, Field => Field2}

/*
 * @since   Nov.  5, 2021
 * @version Jan.  7, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class QueryExpressionSpec extends WordSpec with Matchers {
  "LikeQuery" should {
    "%abc%" which {
      val q = LikeQuery("%abc%")
      "100" in {
        q.isAccept(100) should be(false)
      }
      "abc" in {
        q.isAccept("abc") should be(true)
      }
      "0abc9" in {
        q.isAccept("0abc9") should be(true)
      }
      "a" in {
        q.isAccept("a") should be(false)
      }
    }
    "%abc" which {
      val q = LikeQuery("%abc")
      "100" in {
        q.isAccept(100) should be(false)
      }
      "abc" in {
        q.isAccept("abc") should be(true)
      }
      "0abc" in {
        q.isAccept("0abc") should be(true)
      }
      "0abc9" in {
        q.isAccept("0abc9") should be(false)
      }
      "a" in {
        q.isAccept("a") should be(false)
      }
    }
    "abc%" which {
      val q = LikeQuery("abc%")
      "100" in {
        q.isAccept(100) should be(false)
      }
      "abc" in {
        q.isAccept("abc") should be(true)
      }
      "0abc" in {
        q.isAccept("0abc") should be(false)
      }
      "abc9" in {
        q.isAccept("abc9") should be(true)
      }
      "0abc9" in {
        q.isAccept("0abc9") should be(false)
      }
      "a" in {
        q.isAccept("a") should be(false)
      }
    }
  }
  "FirstMatchQuery" should {
    "abc" which {
      val q = FirstMatchQuery("abc")
      "100" in {
        q.isAccept(100) should be(false)
      }
      "abc" in {
        q.isAccept("abc") should be(true)
      }
      "0abc" in {
        q.isAccept("0abc") should be(false)
      }
      "abc9" in {
        q.isAccept("abc9") should be(true)
      }
      "0abc9" in {
        q.isAccept("0abc9") should be(false)
      }
      "a" in {
        q.isAccept("a") should be(false)
      }
    }
  }
  "LastMatchQuery" should {
    "abc" which {
      val q = LastMatchQuery("abc")
      "100" in {
        q.isAccept(100) should be(false)
      }
      "abc" in {
        q.isAccept("abc") should be(true)
      }
      "0abc" in {
        q.isAccept("0abc") should be(true)
      }
      "0abc9" in {
        q.isAccept("0abc9") should be(false)
      }
      "a" in {
        q.isAccept("a") should be(false)
      }
    }
  }
  "parse" should {
    implicit val context = QueryExpression.Context.now()

    def activate(name: String, value: String): Any = {
      val x = Field2(Symbol(name), List(value))
      QueryExpression.activate(x).getOne.getOrElse(RAISE.noReachDefect)
    }

    def parse(name: String, value: String): Any = {
      QueryExpression.parse(name, value)._2
    }

    "parse" which {
      "normal" in {
        parse("x_query", "is-null") should be(EqualQuery("is-null"))
      }
      "is-null" in {
        parse("x__query", "is-null") should be(IsNullQuery)
      }
    }
    "activate" which {
      "normal" in {
        activate("x_query", "is-null") should be("is-null")
      }
      "is-null" in {
        activate("x__query", "is-null") should be(IsNullQuery)
      }
    }
  }
}
