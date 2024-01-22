package org.goldenport.record.v3

import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.util.JsonUtils

/*
 * @since   Aug. 23, 2018
 *  version Sep. 17, 2018
 *  version Nov.  7, 2018
 *  version Mar. 21, 2020
 * @version Jun. 23, 2022
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
    def _buildfield_(f: Field) {
      buf.append("\"")
      buf.append(f.name)
      buf.append("\":")
      f.getValue.map(JsonUtils.data2json(buf, _)).getOrElse("null")
    }

    def _availablep_(f: Field) = f.value != EmptyValue

    buf.append("{")
    val fs = fields.filter(_availablep_)
    if (fs.nonEmpty) {
      _buildfield_(fs.head)
      for (f <- fs.tail) {
        buf.append(",")
        _buildfield_(f)
      }
    }
    buf.append("}")
  }

  //   def _availablep_(f: (String, Any)) = {
  //     val v = f._2
  //     v != null && (v match {
  //       case Nil => false
  //       case x :: Nil => x match {
  //         case Nil => false
  //         case _ => true
  //       }
  //       case _ => true
  //     })
  //   }

  //   buf.append("{")
  //   val fs = _key_values(fields).filter(_availablep_)
  //   if (fs.nonEmpty) {
  //     _buildfield_(fs.head)
  //     for (f <- fs.tail) {
  //       buf.append(",")
  //       _buildfield_(f)
  //     }
  //   }
  //   buf.append("}")
  // }
}

object JsonPart {
}
