package org.goldenport.record.parser

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser.ParseSuccess
import org.goldenport.record.v3.Record

/*
 * @since   Mar. 20, 2021
 * @version Mar. 20, 2021
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RecordParserSpec extends WordSpec with Matchers {
  "RecordParser" should {
    "httpQuery" which {
      val config = RecordParser.Config()
      val parser = RecordParser(config)
      "none" in {
        val query = "one=1&two=2"
        val rec = parser.httpQuery(query)
        val target = Record.data(
          "one" -> "1",
          "two" -> "2"
        )
        rec should be(ParseSuccess(target))
      }
    }
    "httpForm" ignore {
      val config = RecordParser.Config()
      val parser = RecordParser(config)
      "none" in {
        val form = Record.data(
          "one" -> "1",
          "two" -> "2"
        )
        val rec = parser.httpForm(form)
        rec should be(ParseSuccess(form))
      }
      "nest" in {
        val form = Record.data(
          "one" -> "1",
          "abc/xyz" -> "abc1xyz",
          "abc/mno" -> "abc1mno"
        )
        val rec = parser.httpForm(form)
        val target = Record.data(
          "one" -> "1",
          "abc" -> Record.data("xyz" -> "abc1xyz", "mno" -> "abc1mno")
        )
        rec should be(ParseSuccess(target))
      }
      "nest list" in {
        val form = Record.data(
          "one" -> "1",
          "abc__1/xyz" -> "abc1xyz",
          "abc__1/mno" -> "abc1mno",
          "abc__2/xyz" -> "abc2xyz",
          "abc__2/mno" -> "abc2mno"
        )
        val rec = parser.httpForm(form)
        val target = Record.data(
          "one" -> "1",
          "abc" -> List(
            Record.data("xyz" -> "abc1xyz", "mno" -> "abc1mno"),
            Record.data("xyz" -> "abc2xyz", "mno" -> "abc2mno")
          )
        )
        rec should be(ParseSuccess(target))
      }
      "nest list single" in {
        val form = Record.data(
          "one" -> "1",
          "abc__1/xyz" -> "abc1xyz",
          "abc__1/mno" -> "abc1mno"
        )
        val rec = parser.httpForm(form)
        val target = Record.data(
          "one" -> "1",
          "abc" -> List(
            Record.data("xyz" -> "abc1xyz", "mno" -> "abc1mno")
          )
        )
        rec should be(ParseSuccess(target))
      }
      "nest list 2" in {
        val form = Record.data(
          "abc__1/xyz__1/A" -> "abc1xyz1A",
          "abc__1/xyz__2/A" -> "abc1xyz2A",
          "abc__1/mno__1/B" -> "abc1mno1B",
          "abc__1/mno__2/B" -> "abc1mno2B",
          "abc__2/xyz__1/A" -> "abc2xyz1A",
          "abc__2/xyz__2/A" -> "abc2xyz2A",
          "abc__2/mno__1/B" -> "abc2mno1B",
          "abc__2/mno__2/B" -> "abc2mno2B"
        )
        val rec = parser.httpForm(form)
        val target = Record.data(
          "abc" -> List(
            Record.data(
              "xyz" -> List(
                Record.data("A" -> "abc1xyz1A"),
                Record.data("A" -> "abc1xyz2A")
              ),
              "mno" -> List(
                Record.data("B" -> "abc1mno1B"),
                Record.data("B" -> "abc1mno2B")
              )
            ),
            Record.data(
              "xyz" -> List(
                Record.data("A" -> "abc2xyz1A"),
                Record.data("A" -> "abc2xyz2A")
              ),
              "mno" -> List(
                Record.data("B" -> "abc2mno1B"),
                Record.data("B" -> "abc2mno2B")
              )
            )
          )
        )
        rec should be(ParseSuccess(target))
      }
      "record list" in {
        val form = Record.data(
          "__1/xyz" -> "xyz1",
          "__1/mno" -> "mno1",
          "__2/xyz" -> "xyz2",
          "__2/mno" -> "mno2"
        )
        val rec = parser.httpForm(form)
        val target = NonEmptyVector.create(
          Record.data(
            "xyz" -> "xyz1",
            "mno" -> "mno1"
          ),
          Record.data(
            "xyz" -> "xyz2",
            "mno" -> "mno2"
          )
        )
        rec should be(ParseSuccess(target))
      }
    }
  }
}
