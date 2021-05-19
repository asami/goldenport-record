package org.goldenport.record.v3

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.collection.NonEmptyVector
import org.goldenport.record.v2.UrlInputFile
import org.goldenport.record.v2.{Record => Record2}

/*
 * @since   Dec. 26, 2018
 *  version Feb. 29, 2020
 * @version May.  8, 2021
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
      "nest list with underscore" in {
        val a = Record.data(
          "one" -> "1",
          "abc_z__1/xyz_w" -> "abc1xyz",
          "abc_z__1/mno_w" -> "abc1mno",
          "abc_z__2/xyz_w" -> "abc2xyz",
          "abc_z__2/mno_w" -> "abc2mno"
        )
        val b = a.http.request.build
        val c = Record.data(
          "one" -> "1",
          "abc_z" -> List(
            Record.data("xyz_w" -> "abc1xyz", "mno_w" -> "abc1mno"),
            Record.data("xyz_w" -> "abc2xyz", "mno_w" -> "abc2mno")
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
        val c = NonEmptyVector.create(
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
    }
    "image" which {
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
      "image one" in {
        val a = Record.data(
          "image_file__1/file" -> UrlInputFile("spec1", "image_file__1_file", new URL("http://example.com/1"))
        )
        val b = a.http.request.build
        // println("Record#http.request.build = " + b)
        val c = Record.data(
          "image_file" -> Vector(
            Record.data("file" -> UrlInputFile("spec1", "image_file__1_file", new URL("http://example.com/1"))
            )
          )
        )
        b should be(Right(c))
      }
      "image two" in {
        val a = Record.data(
          "one" -> "1",
          "image_file__1/file" -> UrlInputFile("spec1", "image_file__1_file", new URL("http://example.com/1")),
          "image_file__2/alt" -> "alt2",
          "image_file__2/link_url" -> "link2",
          "image_file__2/file" -> UrlInputFile("spec2", "image_file__2_file", new URL("http://example.com/2"))
        )
        val b = a.http.request.build
        // println("Record#http.request.build = " + b)
        val c = Record.data(
          "one" -> "1",
          "image_file" -> Vector(
            Record.data(
              "file" -> UrlInputFile("spec1", "image_file__1_file", new URL("http://example.com/1"))
            ),
            Record.data(
              "alt" -> "alt2",
              "link_url" -> "link2",
              "file" -> UrlInputFile("spec2", "image_file__2_file", new URL("http://example.com/2"))
            )
          )
        )
        b should be(Right(c))
      }
    }
  }
  "to V2" should {
    "image" in {
      val r = Record.data(
        "image_file" -> Vector(
          Record.data("file" -> UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))
          )
        )
      )
      val r2 = r.toRecord2
      val c = Record2.dataApp(
        "image_file" -> Vector(
          Record2(Nil, inputFiles = List(UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))))
        )
      )
      r2 should be(c)
    }
    "image with property" in {
      val r = Record.data(
        "image_file" -> Vector(
          Record.data(
            "file" -> UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1")),
            "level" -> 1
          )
        )
      )
      val r2 = r.toRecord2
      val c = Record2.dataApp(
        "image_file" -> Vector(
          Record2.dataApp("level" -> 1).
            copy(
              inputFiles = List(UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1")))
            )
        )
      )
      r2 should be(c)
    }
    "image in nest" in {
      val r = Record.data(
        "items" -> Vector(
          Record.data(
            "image_file" ->
              Vector(
                Record.data(
                  "file" -> UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))
                )
              )
          )
        )
      )
      // println(s"r: $r")
      val r2 = r.toRecord2
      val c = Record2.dataApp(
        "items" -> List(
          Record2.dataApp(
            "image_file" -> Vector(
              Record2(Nil, inputFiles = List(UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))))
            )
          )
        )
      )
      r2 should be(c)
    }
    "image with property in nest" in {
      val r = Record.data(
        "items" -> Vector(
          Record.data(
            "image_file" -> Vector(
              Record.data(
                "file" -> UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1")),
                "level" -> 1
              )
            )
          )
        )
      )
      val r2 = r.toRecord2
      val c = Record2.dataApp(
        "items" -> List(
          Record2.dataApp(
            "image_file" -> Vector(
              Record2.dataApp("level" -> 1).
                copy(
                  inputFiles = List(UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1")))
                )
            )
          )
        )
      )
      r2 should be(c)
    }
  }
  "from V2" should {
    "image" in {
      val r2 = Record2(Nil, inputFiles = List(UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))))
      val r = Record.create(r2)
      // println(r)
      r should be(Record.data(
        "image_file__1/file" -> UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))
      ).withRecord2(r2))
      val i = r.http.request.build
      // println(i)
      val c = Record.data(
        "image_file" -> Vector(
          Record.data("file" -> UrlInputFile("spec1", "image_file__1/file", new URL("http://example.com/1"))
          )
        )
      )
      i should be(Right(c))
    }
  }
}
