package org.goldenport.record.util

import java.net.URL
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.goldenport.RAISE
import org.goldenport.record.v2.Record

/*
 * @since   Jul.  1, 2022
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class JsonUtilsSpec extends AnyWordSpec with Matchers {
  "data2json" should {
    "data2json" which {
      "data2json" in {
        val a = Record.dataApp(
          "html" -> """<a src="x">
</a>"""
        )
        val x = JsonUtils.data2json(a)
        x should be ("""{"html":"<a src=\"x\">\n</a>"}""")
      }
    }
  }
}
