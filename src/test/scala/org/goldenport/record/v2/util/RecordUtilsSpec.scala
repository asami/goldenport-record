package org.goldenport.record.v2.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.record.v2._

/*
 * @since   Sep. 14, 2015
 * @version Sep. 15, 2017
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RecordUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  "sum" should {
    "typical" in {
      val schema = Schema(Vector(
        Column("id"),
        Column("a", XInt),
        Column("b", XLong),
        Column("c")
      ))
      val r = RecordUtils.sum(schema, 'id, Vector('a, 'b))(Vector(
        Record.data(
          "id" -> "one",
          "a" -> 1,
          "b" -> "1"
        ),
        Record.data(
          "id" -> "one",
          "a" -> "2",
          "b" -> 2
        )))
      r should be(Map(
        "one" -> Record.data(
          "id" -> "one",
          "a" -> 3,
          "b" -> 3L
        )))
    }
  }

  "buildSchema" should {
    "order in single record" which {
      "empty" in {
        val rec = Record.dataApp(
        )
        RecordUtils.buildSchema(rec) should be(Schema(List(
        )))
      }
      "one" in {
        val rec = Record.dataApp(
          "id" -> "abc"
        )
        RecordUtils.buildSchema(rec) should be(Schema(List(
          Column("id", XString, MZeroOne)
        )))
      }
      "two" in {
        val rec = Record.dataApp(
          "id" -> "abc",
          "name" -> "taro"
        )
//        println("two")
        RecordUtils.buildSchema(rec) should be(Schema(List(
          Column("id", XString, MZeroOne),
          Column("name", XString, MZeroOne)
        )))
      }
      "three" in {
        val rec = Record.dataApp(
          "id" -> "abc",
          "name" -> "taro",
          "address" -> "yokohama"
        )
//        println("three")
        RecordUtils.buildSchema(rec) should be(Schema(List(
          Column("id", XString, MZeroOne),
          Column("name", XString, MZeroOne),
          Column("address", XString, MZeroOne)
        )))
      }
    }
    "order in multiple records" which {
      "left" in {
        val rec1 = Record.dataApp(
          "name" -> "taro"
        )
        val rec2 = Record.dataApp(
          "id" -> "abc",
          "name" -> "hanako"
        )
//        println("left")
        RecordUtils.buildSchema(rec1, rec2) should be(Schema(List(
          Column("id", XString, MZeroOne),
          Column("name", XString, MZeroOne)
        )))
      }
      "right" in {
        val rec1 = Record.dataApp(
          "name" -> "taro"
        )
        val rec2 = Record.dataApp(
          "name" -> "hanako",
          "address" -> "yokohama"
        )
//        println("right")
        RecordUtils.buildSchema(rec1, rec2) should be(Schema(List(
          Column("name", XString, MZeroOne),
          Column("address", XString, MZeroOne)
        )))
      }
      "left right" in {
        val rec1 = Record.dataApp(
          "name" -> "taro"
        )
        val rec2 = Record.dataApp(
          "id" -> "abc",
          "name" -> "hanako",
          "address" -> "yokohama"
        )
//        println("left right")
        RecordUtils.buildSchema(rec1, rec2) should be(Schema(List(
          Column("id", XString, MZeroOne),
          Column("name", XString, MZeroOne),
          Column("address", XString, MZeroOne)
        )))
      }
    }
    "datatype" which {
      "integer" in {
        val rec = Record.dataApp(
          "age" -> 20
        )
//        println("integer")
        RecordUtils.buildSchema(rec) should be(Schema(List(
          Column("age", XInt, MZeroOne)
        )))
      }
    }
  }
}
