package org.goldenport.record.query

import org.joda.time._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2.{Schema, Column, DataType}
import org.goldenport.record.v2.{Record => Record2, Field => Field2}
import org.goldenport.record.v3.{Record => Record3, Field => Field3}
import org.goldenport.record.v3.{FieldValue, EmptyValue, SingleValue, MultipleValue}
import org.goldenport.record.command.ExtensionValueCommand
import org.goldenport.record.sql.SqlU
import org.goldenport.record.util.{DateUtils, AnyUtils}
import org.goldenport.values.{DateTimePeriod, ParameterKey}
import QueryExpression.Context

/*
 * @since   Jun. 25, 2018
 *  version Jan. 10, 2019
 *  version Jul. 31, 2019
 * @version Aug.  5, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait QueryExpression {
  def expression(column: String): String
  def isAccept(p: Any): Boolean

  protected final def to_literal(p: Any): String = SqlU.literal(p)

  protected final def to_literal(c: Column, d: Any): String = to_literal(c.datatype, d)

  protected final def to_literal(dt: DataType, d: Any): String = SqlU.literal(dt, d)

  protected final def to_literal_list(ps: Seq[Any]) = ps.map(to_literal).mkString(", ")
}

sealed trait QueryExpressionClass {
  def name: String

  def createOption(name: String, params: List[String], v: FieldValue)(implicit ctx: Context): Option[QueryExpression] =
    if (name == this.name)
      Some(create(params, v))
    else
      None

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression

  protected final def to_start_end(v: FieldValue): (Any, Any) =
    to_start_end_string_option(v) match {
      case (Some(l), Some(r)) => (l, r)
      case (Some(l), None) => RAISE.invalidArgumentFault(s"No end in range: ${v.asString}")
      case (None, Some(r)) => RAISE.invalidArgumentFault(s"No start in range: ${v.asString}")
      case (None, None) => RAISE.invalidArgumentFault(s"Unavailable range: ${v.asString}")
    }

  protected final def to_start_end_string_option(v: FieldValue): (Option[String], Option[String]) = {
    val a = v.asString
    val i = a.indexOf("~")
    if (i == -1) {
      RAISE.invalidArgumentFault(s"Unavailable range: ${a}")
    } else {
      val s = a.substring(0, i)
      val e = s.substring(i + 1)
      (Strings.blankopt(s), Strings.blankopt(e))
    }
  }
}

case object NoneQuery extends QueryExpression {
  def expression(column: String) = "1 = 1"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this
}

case class EqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = value match {
    case m: Seq[_] => s"""${column} IN (${to_literal_list(m)})"""
    case _ => s"${column} = ${to_literal(value)}"
  }
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object EqualQuery extends QueryExpressionClass {
  val name = "equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression =
    v match {
      case EmptyValue => EqualQuery("")
      case m: SingleValue => EqualQuery(m.value)
      case m: MultipleValue => EqualQuery(m.values)
    }
}

case class NotEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = value match {
    case m: Seq[_] => s"""${column} NOT IN (${to_literal_list(m)})"""
    case m => s"${column} <> ${to_literal(value)}"
  }
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object NotEqualQuery extends QueryExpressionClass {
  val name = "not-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = 
    v match {
      case EmptyValue => NotEqualQuery("")
      case m: SingleValue => NotEqualQuery(m.value)
      case m: MultipleValue => NotEqualQuery(m.values)
    }
}

case class GreaterQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} > ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object GreaterQuery extends QueryExpressionClass {
  val name = "greater"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = GreaterQuery(v.asString)
}

case class GreaterEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} >= ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object GreaterEqualQuery extends QueryExpressionClass {
  val name = "greater-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = GreaterEqualQuery(v.asString)
}

case class LesserQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} < ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object LesserQuery extends QueryExpressionClass {
  val name = "lesser"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LesserQuery(v.asString)
}

case class LesserEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} <= ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object LesserEqualQuery extends QueryExpressionClass {
  val name = "lesser-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LesserEqualQuery(v.asString)
}

case class RangeInclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} <= ${to_literal(end)})"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object RangeInclusiveQuery extends QueryExpressionClass {
  val name = "range-inclusive"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = {
    val (start, end) = to_start_end(v)
    RangeInclusiveQuery(start, end)
  }
}

case class RangeExclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} < ${column} AND ${column} < ${to_literal(end)})"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object RangeExclusiveQuery extends QueryExpressionClass {
  val name = "range-exclusive"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = {
    val (start, end) = to_start_end(v)
    RangeExclusiveQuery(start, end)
  }
}

case class RangeInclusiveExclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} < ${to_literal(end)})"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object RangeInclusiveExclusiveQuery extends QueryExpressionClass {
  val name = "range-inclusive-exclusive"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = {
    val (start, end) = to_start_end(v)
    RangeInclusiveExclusiveQuery(start, end)
  }
}

case class RangeExclusiveInclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} < ${column} AND ${column} <= ${to_literal(end)})"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object RangeExclusiveInclusiveQuery extends QueryExpressionClass {
  val name = "range-exclusive-inclusive"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = {
    val (start, end) = to_start_end(v)
    RangeExclusiveInclusiveQuery(start, end)
  }
}

case class DateTimePeriodQuery(period: DateTimePeriod) extends QueryExpression {
  def expression(column: String) = s"$column ${period.toSqlBetweenDateTime}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object DateTimePeriodQuery extends QueryExpressionClass {
  val name = "datetime-period"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = {
    val (start, end) = to_start_end_string_option(v)
    val inclusive = params.tail.headOption.map(AnyUtils.toBoolean).getOrElse(true)
    val builder = DateTimePeriod.Builder(ctx.datetime, ctx.timezoneJoda)
    DateTimePeriodQuery(builder.create(start, end, inclusive))
  }
}

case class LikeQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} LIKE ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object LikeQuery extends QueryExpressionClass {
  val name = "like"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LikeQuery(v.asString)
}

case class RegexQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} REGEXP ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object RegexQuery extends QueryExpressionClass {
  val name = "regex"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = RegexQuery(v.asString)
}

case class FirstMatchQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"""${column} LIKE ${to_literal(value + "%")}"""
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object FirstMatchQuery extends QueryExpressionClass {
  val name = "first-match"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = FirstMatchQuery(v.asString)
}

case class LastMatchQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"""${column} LIKE ${to_literal("%" + value)}"""
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object LastMatchQuery extends QueryExpressionClass {
  val name = "last-match"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LastMatchQuery(v.asString)
}

case class FirstLastMatchQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"""${column} LIKE ${to_literal("%" + value + "%")}"""
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect
}
object FirstLastMatchQuery extends QueryExpressionClass {
  val name = "first-last-match"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = FirstLastMatchQuery(v.asString)
}

case object IsNullQuery extends QueryExpression with QueryExpressionClass {
  val name = "is-null"

  def expression(column: String) = s"${column} IS NULL"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this
}

case object IsNotNullQuery extends QueryExpression with QueryExpressionClass {
  val name = "is-not-null"

  def expression(column: String) = s"${column} IS NOT NULL"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this
}

case object AllQuery extends QueryExpression with QueryExpressionClass {
  val name = "all"

  def expression(column: String) = "1 = 1"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this
}

object QueryExpression {
  import scalaz._, Scalaz._

  val ADORNMENT_QUERY = "query"

  case class Context(
    datetime: DateTime,
    timezoneJoda: DateTimeZone
  )

  val expressions: Vector[QueryExpressionClass] = Vector(
    AllQuery,
    IsNullQuery,
    IsNotNullQuery,
    EqualQuery,
    NotEqualQuery,
    GreaterQuery,
    GreaterEqualQuery,
    LesserQuery,
    LesserEqualQuery,
    RangeInclusiveQuery,
    RangeExclusiveQuery,
    RangeInclusiveExclusiveQuery,
    RangeExclusiveInclusiveQuery,
    DateTimePeriodQuery,
    LikeQuery,
    RegexQuery,
    FirstMatchQuery,
    LastMatchQuery,
    FirstLastMatchQuery
  )

  def activate(p: Record2)(implicit ctx: Context): Record2 = p.mapField(activate)

  def activate(p: Field2)(implicit ctx: Context): Field2 = {
    val key = ParameterKey.parse(p.key)
    key.adornment.collect {
      case ADORNMENT_QUERY => _expression(p, key.path, key.adornmentArguments)
    }.getOrElse(p)
  }

  private def _expression(p: Field2, path: String, adargs: List[String])(implicit ctx: Context): Field2 = 
    adargs.headOption.flatMap { x =>
      val fv = p.toFieldValue
      val v = expressions.toStream.flatMap(_.createOption(x, adargs.tail, fv)).headOption
      v.map(p.update(path, _))
    }.getOrElse(RAISE.invalidArgumentFault(s"Invalid query: ${p.key.name}"))

  // def parse(schema: Schema, s: String): Option[QueryExpression] = {
  //   schema.columns.find(_.name === s).map(parse(_, s))
  // }

// def parse(column: Column, s: String): QueryExpression = {
  //   _parse_expression(s) // TODO
  // }

  // def parse(s: String): QueryExpression = {
  //   _parse_expression(s) // TODO
  // }

  // private def _parse_expression(s: String) = {
  //   s match { // TODO parser
  //     case "(all)" => AllQuery
  //     case "(is_null)" => IsNullQuery
  //     case "(is_not_null)" => IsNotNullQuery
  //     case "(not_equal TODO)" => NotEqualQuery(s) // TODO
  //     case _ => EqualQuery(s)
  //   }
  // }
}

