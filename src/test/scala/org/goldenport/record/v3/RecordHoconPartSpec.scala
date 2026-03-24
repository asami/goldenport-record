package org.goldenport.record.v3

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.goldenport.record.v3._

/*
 * @since   Jan. 25, 2022
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class RecordHoconSpec extends AnyWordSpec with Matchers {
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
