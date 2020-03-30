package org.goldenport.record.v3

import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.util.JsonUtils

/*
 * @since   Aug. 23, 2018
 *  version Sep. 17, 2018
 *  version Nov.  7, 2018
 * @version Mar. 21, 2020
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
    def buildfield(f: Field) {
      buf.append("\"")
      buf.append(f.name)
      buf.append("\":")
      f.getValue.map(JsonUtils.data2json(buf, _)).getOrElse("null")
    }

    buf.append("{")
    if (fields.nonEmpty) {
      buildfield(fields.head)
      for (f <- fields.tail) {
        buf.append(",")
        buildfield(f)
      }
    }
    buf.append("}")
  }
}

object JsonPart {
}
