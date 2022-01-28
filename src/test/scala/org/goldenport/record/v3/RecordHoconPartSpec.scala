package org.goldenport.record.v3

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.record.v3._

/*
 * @since   Jan. 25, 2022
 * @version Jan. 25, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RecordHoconSpec extends WordSpec with Matchers {
  "RecordHocon" should {
    "typical" which {
      "simple" in {
        val rec = Record.data(
          "a" -> 1,
          "b" -> "B"
        )
        val r = rec.toHoconString
        println(r)
      }
      "next 1" in {
        val rec = Record.data(
          "a" -> Record.data(
            "b" -> "ab",
            "c" -> "ac"
          ),
          "d"-> Record.data(
            "e" -> 100
          )
        )
        val r = rec.toHoconString
        println(r)
      }
    }
  }
}
