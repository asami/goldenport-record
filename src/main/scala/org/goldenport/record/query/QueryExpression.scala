package org.goldenport.record.query

import org.joda.time._
import org.goldenport.record.v2.{Schema, Column, DataType}
import org.goldenport.record.command.ExtensionValueCommand
import org.goldenport.record.sql.SqlU
import org.goldenport.record.util.DateUtils
import org.goldenport.values.DateTimePeriod

/*
 * @since   Jun. 25, 2018
 * @version Jan. 10, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait QueryExpression {
  def expression(column: String): String

  protected def to_literal(p: Any): String = SqlU.literal(p)

  protected def to_literal(c: Column, d: Any): String = to_literal(c.datatype, d)

  protected def to_literal(dt: DataType, d: Any): String = SqlU.literal(dt, d)
}

case object NoneQuery extends QueryExpression {
  def expression(column: String) = "1 = 1"
}

case class EqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} = ${to_literal(value)}"
}

case class NotEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} <> ${to_literal(value)}"
}

case class GreaterQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} > ${to_literal(value)}"
}

case class GreaterEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} >= ${to_literal(value)}"
}

case class LesserQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} < ${to_literal(value)}"
}

case class LesserEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} <= ${to_literal(value)}"
}

case class RangeInclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} <= ${to_literal(end)})"
}

case class RangeExclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} < ${column} AND ${column} < ${to_literal(end)})"
}

case class RangeInclusiveExclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} < ${to_literal(end)})"
}

case class RangeExclusiveInclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} < ${column} AND ${column} <= ${to_literal(end)})"
}

case class DateTimePeriodQuery(period: DateTimePeriod) extends QueryExpression {
  def expression(column: String) = s"$column ${period.toSqlBetweenDateTime}"
}

case class LikeStartQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} LIKE ${to_literal(value)}"
}

case object IsNullQuery extends QueryExpression {
  def expression(column: String) = s"${column} IS NULL"
}

case object IsNotNullQuery extends QueryExpression {
  def expression(column: String) = s"${column} IS NOT NULL"
}

case object AllQuery extends QueryExpression {
  def expression(column: String) = "1 = 1"
}

object QueryExpression {
  import scalaz._, Scalaz._

  case class Context(
    datetime: DateTime,
    timezone: DateTimeZone
  )

  def parse(schema: Schema, s: String): Option[QueryExpression] = {
    schema.columns.find(_.name === s).map(parse(_, s))
  }

  def parse(column: Column, s: String): QueryExpression = {
    _parse_expression(s) // TODO
  }

  def parse(s: String): QueryExpression = {
    _parse_expression(s) // TODO
  }

  private def _parse_expression(s: String) = {
    s match { // TODO parser
      case "(all)" => AllQuery
      case "(is_null)" => IsNullQuery
      case "(is_not_null)" => IsNotNullQuery
      case "(not_equal TODO)" => NotEqualQuery(s) // TODO
      case _ => EqualQuery(s)
    }
  }
}

