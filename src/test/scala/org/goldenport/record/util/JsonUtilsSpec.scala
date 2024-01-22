package org.goldenport.record.util

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.RAISE
import org.goldenport.record.v2.Record

/*
 * @since   Jul.  1, 2022
 * @version Jul.  1, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class JsonUtilsSpec extends WordSpec with Matchers {
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
