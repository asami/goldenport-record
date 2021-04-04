package org.goldenport.record.util

import java.sql.Timestamp
import org.joda.time.DateTime
import org.goldenport.extension.{IRecord => LIRecord}
import org.goldenport.record.v3.Record

/*
 * @since   May. 23, 2014
 *  version Jun.  1, 2014
 *  version Dec. 28, 2014
 *  version Jan.  1, 2015
 * @version Mar. 28, 2021
 * @author  ASAMI, Tomoharu
 */
object JsonUtils {
  def data2json(v: Any): String = {
    val buf = new StringBuilder
    data2json(buf, v)
    buf.toString
  }

  def data2json(buf: StringBuilder, v: Any) {
    def appendstring(s: String) {
      buf.append("\"")
      buf.append(escape(s))
      buf.append("\"")
    }
    v match {
      case _: Boolean => buf.append(v)
      case _: Byte => buf.append(v)
      case _: Short => buf.append(v)
      case _: Int => buf.append(v)
      case _: Long => buf.append(v)
      case _: Float => buf.append(v)
      case _: Double => buf.append(v)
      case _: BigInt => buf.append(v)
      case _: BigDecimal => buf.append(v)
      case ts: Timestamp => appendstring(DateTimeUtils.toIsoDateTimeStringJst(ts))
      case dt: DateTime => appendstring(DateTimeUtils.toIsoDateTimeStringJst(dt))
//      case d: Date => buf.append(DateTimeUtils.toString(ts))
      case rec: Record => rec.buildJsonString(buf)
      case Some(s) => data2json(buf, s)
      case None => buf.append("null")
      case xs: Seq[_] => {
        buf.append("[")
        xs.headOption.map(x => {
          data2json(buf, x)
          for (a <- xs.tail) {
            buf.append(", ")
            data2json(buf, a)
          }
        })
        buf.append("]")
      }
      case m: Map[_, _] => Record.createAnyMap(m).buildJsonString(buf)
      case m: LIRecord => Record.create(m).buildJsonString(buf)
      case _ => {
        buf.append("\"")
        buf.append(escape(v.toString))
        buf.append("\"")
      }
    }
  }

  def escape(s: String): String = {
    if ((s.indexOf('"') == -1) && (s.indexOf('\\') == -1)) s
    else {
      val buf = new StringBuilder
      for (x: Char <- s) {
        x match {
          case '"' => buf.append("""\u0022""") // TODO same as escape_extjs
          case '\\' => buf.append("""\u005C""")
          case _ => buf.append(x)
        }
      }
      buf.toString
    }
  }
}
