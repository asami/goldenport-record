package org.goldenport.record.v3

import org.goldenport.record.util.JsonUtils

/*
 * @since   Aug. 23, 2018
 *  version Oct. 16, 2018
 * @version Apr. 15, 2019
 * @author  ASAMI, Tomoharu
 */
trait CompatibilityPart { self: Record =>
  // Joda

  // /*
  //  * Log
  //  */
  // def toLog: String = {
  //   val buf = new StringBuilder
  //   def key(k: Symbol) {
  //     buf.append("\"")
  //     buf.append(k.name)
  //     buf.append("\":")
  //   }
  //   def field(k: Symbol, v: Any) {
  //     key(k)
  //     JsonUtils.data2json(buf, v)
  //   }
  //   buf.append("{")
  //   field('timestamp, timestamp)
  //   buf.append(",")
  //   key('data)
  //   buildJsonString(buf)
  //   buf.append(",")
  //   field('validation, validation)
  //   if (source.nonEmpty) {
  //     buf.append(",")
  //     key('source)
  //     buf.append("[")
  //     buf.append(source.map(_.toLog).mkString(","))
  //     buf.append("]")
  //   }
  //   for (e <- exception) {
  //     buf.append(",")
  //     field('exception, e)
  //   }
  //   buf.append("}")
  //   buf.toString
  // }
}

object CompatibilityPart {
}
