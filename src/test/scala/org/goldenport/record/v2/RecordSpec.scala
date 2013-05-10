package org.goldenport.record.v2

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.matchers._

/**
 * @since   Feb. 16, 2013
 *  version Mar. 28, 2013
 * @version May. 10, 2013
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
      "empty" in {
        val r = Record(List(Field(Symbol("b"), List())))
        val x = r.getOne("b")
        println("empty = " + x)
        x should be(None)
      }
    }
    "update" that {
      "typical" in {
        val a = Record.create(List("a" -> 1, "b" -> 2))
        val x = Record.create(List("b" -> 20, "c" -> 30))
        val r = a.update(x)
        r.fields.length should be (3)
        println("RecordSet#update = " + r)
      }
      "raw" in {
        val a = Record.create(List("a" -> 1, "b" -> 2))
        val x = List("b" -> 20, "c" -> 30)
        val r = a.update(x)
        r.fields.length should be (3)
        println("RecordSet#update = " + r)
      }
    }
    "normalize multiplicity" that {
      "typical" in {
        val a = Record.create(List(
          "one" -> "1",
          "abc__1_xyz" -> "abc1xyz",
          "abc__1_mno" -> "abc1mno",
          "abc__2_xyz" -> "abc2xyz",
          "abc__2_mno" -> "abc2mno"))
        val b = a.normalizeMultiplicity
        println("RecordSet#normalizeMultiplicity = " + b)
      }
      "multiplicity only" in {
        val a = Record.create(List(
          "abc__1_xyz" -> "abc1xyz",
          "abc__1_mno" -> "abc1mno",
          "abc__2_xyz" -> "abc2xyz",
          "abc__2_mno" -> "abc2mno"))
        val b = a.normalizeMultiplicity
        println("RecordSet#normalizeMultiplicity multiplicity only = " + b)
      }
      "nest" in {
        val a = Record.create(List(
          "abc__1_xyz__1_a" -> "abc1xyz1a",
          "abc__1_xyz__2_b" -> "abc1xyz2b",
          "abc__1_mno__1_a" -> "abc1mno1a",
          "abc__1_mno__2_b" -> "abc1mno2b",
          "abc__2_xyz__1_a" -> "abc2xyz1a",
          "abc__2_xyz__2_b" -> "abc2xyz2b",
          "abc__2_mno__1_a" -> "abc2mno1a",
          "abc__2_mno__2_b" -> "abc2mno2b"))
        val b = a.normalizeMultiplicity
        println("RecordSet#normalizeMultiplicity nest = " + b)
      }
      "image" in {
        val a = Record.create(List(
          "one" -> "1",
          "image_file__2_alt" -> "alt2",
          "image_file__2_link_url" -> "link2")).
          withInputFiles(
            UrlInputFile("spec1", "image_file__1_file", new URL("http://example.com/1")),
            UrlInputFile("spec2", "image_file__2_file", new URL("http://example.com/2"))
          )
        val b = a.normalizeMultiplicity
        println("RecordSet#normalizeMultiplicity multi images = " + b)
      }
    }
  }
}
