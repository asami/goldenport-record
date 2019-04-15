package org.goldenport.record.v2.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.record.v2._

/*
 * @since   Apr.  3, 2019
 * @version Apr.  3, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
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
