package org.goldenport.record.v3

import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.util.JsonUtils

/*
 * @since   Aug. 23, 2018
 * @version Sep. 17, 2018
 * @author  ASAMI, Tomoharu
 */
trait JsonPart { self: Record =>
  def toJson: JsObject = {
    val xs = fields.flatMap(_.getJsonField)
    JsObject(xs)
  }

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
    val fs = nameStrings
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
