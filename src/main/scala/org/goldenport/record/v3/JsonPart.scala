package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.record.util.JsonUtils

/*
 * @since   Aug. 23, 2018
 * @version Aug. 24, 2018
 * @author  ASAMI, Tomoharu
 */
trait JsonPart { self: Record =>
  def toJsonString: String = {
    val buf = new StringBuilder
    buildJsonString(buf)
    buf.toString
  }

  def buildJsonString(buf: StringBuilder) {
    def buildfield(kv: (String, Any)) {
      buf.append("\"")
      buf.append(kv._1)
      buf.append("\":")
      JsonUtils.data2json(buf, kv._2)
    }

    buf.append("{")
    val fs = keyStringValues
    if (fs.nonEmpty) {
      buildfield(fs.head)
      for (f <- fs.tail) {
        buf.append(",")
        buildfield(f)
      }
    }
    buf.append("}")
  }
}

object JsonPart {
}
