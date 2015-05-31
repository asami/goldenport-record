package org.goldenport.record.v1.sql

import org.goldenport.record.v1._
import org.goldenport.util._

/**
 * @since   Aug. 17, 2010
 *  version Jun. 26, 2011
 *  version Feb. 16, 2012
 *  version Feb. 17, 2013
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
object SqlU {
  def symbol2name(symbol: Symbol): String = symbol.name

  def value2literal(value: Any): String = value match {
    case a: Seq[_] => "'" + make_json(a) + "'"
    case n: Number => n.toString
    case v: DataValue => v.sqlLiteral
    case s: String => "'" + escape_literal(s) + "'"
    case s => "'" + escape_literal(s.toString) + "'"
  }

  private def escape_literal(value: String) = {
    value.replace("'", "''")
  }

  private def make_json(value: Any): String = value match {
    case a: Seq[_] => a.map(make_json).mkString("[", ",", "]")
    case n: Number => n.toString
    case v: DataValue => '"' + v.sqlLiteral + '"'
    case s: String => '"' + escape_literal(s) + '"'
    case s => '"' + escape_literal(s.toString) + '"'
  }

  def makeExampleWhere(example: Record) = {
    val conds = for ((k, v) <- example.fields) yield {
      v match {
        case c: XEnumeration => sys.error("Sql")
        case c: XMaxInclusive => sys.error("Sql")
        case c: XMaxExclusive => sys.error("Sql")
        case c: XMinInclusive => sys.error("Sql")
        case c: XMinExclusive => sys.error("Sql")
        case c: Constraint => sys.error("Sql")
        case _ => k -> v
      }
    }
    conds.map(kv => kv._1 + " = " + value2literal(kv._2)).mkString(" OR ")
  }
}
