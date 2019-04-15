package org.goldenport.record.sql

import java.util.{Date, TimeZone}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time._
import org.goldenport.record.v2._
import org.goldenport.record.util._

/*
 * @since   Aug. 17, 2010
 *  version Jun. 26, 2011
 *  version Feb. 16, 2012
 *  version Feb. 17, 2013
 *  version Jan.  9, 2019
 * @version Apr. 15, 2019
 * @author  ASAMI, Tomoharu
 */
object SqlU {
  private def gmt = TimeUtils.gmt

  def literal(c: Column, p: Any): String = literal(c.datatype, p)

  def literal(dt: DataType, p: Any): String = {
    val a = dt match {
      case XDateTime => sql_long2datetime(XDateTime.toInstance(p))
      case XDate => sql_long2date(XDate.toInstance(p))
      case _ => p.toString
    }
    if (dt.isSqlString)
      s"'$a'"
    else
      a
  }

  def literal(p: Any): String = p match {
    case m: Boolean => m.toString
    case m: Char => s"'$m'"
    case m: Byte => m.toString
    case m: Short => m.toString
    case m: Int => m.toString
    case m: Long => m.toString
    case m: Float => m.toString
    case m: Double => m.toString
    case m: BigInt => m.toString
    case m: BigDecimal => m.toString
    case m: Timestamp => stringLiteral(m.toString) // GMT
    case m: Date => stringLiteral(DateUtils.toIsoDateString(m)) // ISO Date/GMT
    case m: DateTime => stringLiteral(m.toString)
    case m: LocalDate => stringLiteral(m.toString)
    case m: LocalDateTime => ???
    case m => stringLiteral(m)
  }

  protected def sql_long2datetime(ts: Timestamp): String = {
    sql_long2datetime(ts.getTime)
  }

  // See com.everforth.everforth.util.DateTimeUtils
  protected def sql_long2datetime(t: Long): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    format.setTimeZone(gmt)
    val d = new java.sql.Date(t)
    stringLiteral(format.format(d))
  }

  protected def sql_long2date(d: Date): String = {
    sql_long2date(d.getTime)
  }

  // See com.everforth.everforth.util.DateTimeUtils
  // Change from JST to GMT
  protected def sql_long2date(t: Long): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.setTimeZone(gmt)
    val d = new java.sql.Date(t)
    val r = stringLiteral(format.format(d))
//    println("DateTimeUtils#sql_string2date long %s = %s".format(t, r))
    r
  }

  def stringLiteral(d: Any): String = "'" + sqlEscape(d.toString) + "'"

  def sqlEscape(s: String): String = {
    if (s.indexOf("'") == -1) s.replace("\\", "\\\\")
    else s.replace("'", "''").replace("\\", "\\\\")
  }

  /*
   * Legacy
   */
  // import org.goldenport.record._
  // import org.goldenport.util._

  // def symbol2name(symbol: Symbol): String = symbol.name

  // def value2literal(value: Any): String = value match {
  //   case a: Seq[_] => "'" + make_json(a) + "'"
  //   case n: Number => n.toString
  //   case v: DataValue => v.sqlLiteral
  //   case s: String => "'" + escape_literal(s) + "'"
  //   case s => "'" + escape_literal(s.toString) + "'"
  // }

  // private def escape_literal(value: String) = {
  //   value.replace("'", "''")
  // }

  // private def make_json(value: Any): String = value match {
  //   case a: Seq[_] => a.map(make_json).mkString("[", ",", "]")
  //   case n: Number => n.toString
  //   case v: DataValue => '"' + v.sqlLiteral + '"'
  //   case s: String => '"' + escape_literal(s) + '"'
  //   case s => '"' + escape_literal(s.toString) + '"'
  // }

  // def makeExampleWhere(example: Record) = {
  //   val conds = for ((k, v) <- example.fields) yield {
  //     v match {
  //       case c: XEnumeration => sys.error("Sql")
  //       case c: XMaxInclusive => sys.error("Sql")
  //       case c: XMaxExclusive => sys.error("Sql")
  //       case c: XMinInclusive => sys.error("Sql")
  //       case c: XMinExclusive => sys.error("Sql")
  //       case c: Constraint => sys.error("Sql")
  //       case _ => k -> v
  //     }
  //   }
  //   conds.map(kv => kv._1 + " = " + value2literal(kv._2)).mkString(" OR ")
  // }
}
