package org.goldenport.record.v2.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.GivenWhenThen
import org.goldenport.record.v2._

/*
 * @since   Apr.  3, 2019
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class TryoutSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "json" should {
    "simple" in {
      val json = """{"a":"A"}"""
      val rec = RecordUtils.fromJsonString(json)
      rec should be(Record.dataApp("a" -> "A"))
    }
    "array" in {
      val json = """{"values":["a"]}"""
      val rec = RecordUtils.fromJsonString(json)
      rec should be(Record.dataApp("values" -> List("a")))
    }
  }
}
