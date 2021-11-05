package org.goldenport.record.query

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import scalaz.{Store => _, _}, Scalaz._

/*
 * @since   Nov.  5, 2021
 * @version Nov.  5, 2021
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
}
