package org.goldenport.record.v3

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Dec. 26, 2018
 * @version Dec. 28, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RecordSpec extends WordSpec with Matchers {
  "Record" should {
    "normalize" which {
      "none" in {
        val a = Record.data(
          "one" -> "1",
          "two" -> "2"
        )
        val b = a.http.request.build
        b should be(Right(a))
      }
      "nest" in {
        val a = Record.data(
          "one" -> "1",
          "abc/xyz" -> "abc1xyz",
          "abc/mno" -> "abc1mno"
        )
        val b = a.http.request.build
        val c = Record.data(
          "one" -> "1",
          "abc" -> Record.data("xyz" -> "abc1xyz", "mno" -> "abc1mno")
        )
        b should be(Right(c))
      }
      "nest list" in {
        val a = Record.data(
          "one" -> "1",
          "abc__1/xyz" -> "abc1xyz",
          "abc__1/mno" -> "abc1mno",
          "abc__2/xyz" -> "abc2xyz",
          "abc__2/mno" -> "abc2mno"
        )
        val b = a.http.request.build
        val c = Record.data(
          "one" -> "1",
          "abc" -> List(
            Record.data("xyz" -> "abc1xyz", "mno" -> "abc1mno"),
            Record.data("xyz" -> "abc2xyz", "mno" -> "abc2mno")
          )
        )
        b should be(Right(c))
      }
      "nest list single" in {
        val a = Record.data(
          "one" -> "1",
          "abc__1/xyz" -> "abc1xyz",
          "abc__1/mno" -> "abc1mno"
        )
        val b = a.http.request.build
        val c = Record.data(
          "one" -> "1",
          "abc" -> List(
            Record.data("xyz" -> "abc1xyz", "mno" -> "abc1mno")
          )
        )
        b should be(Right(c))
      }
      "nest list 2" in {
        val a = Record.data(
          "abc__1/xyz__1/A" -> "abc1xyz1A",
          "abc__1/xyz__2/A" -> "abc1xyz2A",
          "abc__1/mno__1/B" -> "abc1mno1B",
          "abc__1/mno__2/B" -> "abc1mno2B",
          "abc__2/xyz__1/A" -> "abc2xyz1A",
          "abc__2/xyz__2/A" -> "abc2xyz2A",
          "abc__2/mno__1/B" -> "abc2mno1B",
          "abc__2/mno__2/B" -> "abc2mno2B"
        )
        val b = a.http.request.build
        val c = Record.data(
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
        b should be(Right(c))
      }
      "record list" in {
        val a = Record.data(
          "__1/xyz" -> "xyz1",
          "__1/mno" -> "mno1",
          "__2/xyz" -> "xyz2",
          "__2/mno" -> "mno2"
        )
        val b = a.http.request.build
        val c = NonEmptyVector(
          Record.data(
            "xyz" -> "xyz1",
            "mno" -> "mno1"
          ),
          Record.data(
            "xyz" -> "xyz2",
            "mno" -> "mno2"
          )
        )
        b should be(Left(c))
      }

      // "image" in {
      //   val a = Record.create(List(
      //     "one" -> "1",
      //     "image_file__2_alt" -> "alt2",
      //     "image_file__2_link_url" -> "link2")).
      //     withInputFiles(
      //       UrlInputFile("spec1", "image_file__1_file", new URL("http://example.com/1")),
      //       UrlInputFile("spec2", "image_file__2_file", new URL("http://example.com/2"))
      //     )
      //   val b = a.normalizeMultiplicity
      //   println("RecordSet#normalizeMultiplicity multi images = " + b)
      // }
    }
  }
}
