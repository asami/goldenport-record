package org.goldenport.record.query

import org.joda.time._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2.{Schema, Column, DataType}
import org.goldenport.record.v2.{Record => Record2, Field => Field2}
import org.goldenport.record.v2.{PowertypeClass, Powertype}
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
 *  version Aug. 16, 2019
 * @version Oct. 15, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait QueryExpression {
  def expression(column: String): String
  def isAccept(p: Any): Boolean

  def mapPowertypeOrException(pt: PowertypeClass): Either[Throwable, QueryExpression] = Right(this)

  protected final def to_literal(p: Any): String = SqlU.literal(p)

  protected final def to_literal(c: Column, d: Any): String = to_literal(c.datatype, d)

  protected final def to_literal(dt: DataType, d: Any): String = SqlU.literal(dt, d)

  protected final def to_literal_list(ps: Seq[Any]) = ps.map(to_literal).mkString(", ")

  protected final def map_powertype_value(pt: PowertypeClass, v: Any): Either[Throwable, Any] =
    map_powertype_instance(pt, v).right.map(_.value)

  protected final def map_powertype_values(pt: PowertypeClass, vs: Seq[Any]): Seq[Either[Throwable, Any]] =
    vs.map(map_powertype_value(pt, _))

  protected final def map_powertype_instance(pt: PowertypeClass, v: Any): Either[Throwable, Powertype] = {
    val r = v match {
      case m: Int => pt.get(m)
      case m: String => pt.get(m)
      case m => pt.get(AnyUtils.toString(m))
    }
    r.map(x => Right(x)).getOrElse(Left(new NoSuchElementException(s"$v in ${pt.getClass.getSimpleName}")))
  }

  protected final def map_powertype_value_option(pt: PowertypeClass, a: Option[Any], b: Option[Any]): Either[Throwable, (Option[Any], Option[Any])] =
    (a, b) match {
      case (Some(l), Some(r)) => map_powertype_value(pt, l, r).right.map {
        case (a0, b0) => (Some(a0), Some(b0))
      }
      case (Some(l), None) => map_powertype_value(pt, l).right.map(x => (Some(x), None))
      case (None, Some(r)) => map_powertype_value(pt, r).right.map(x => (None, Some(x)))
      case (None, None) => RAISE.invalidArgumentFault("Invalid range")
    }

  protected final def map_powertype_value(pt: PowertypeClass, a: Any, b: Any): Either[Throwable, (Any, Any)] =
    map_powertype_instance(pt, a, b).right.map {
      case (a0, b0) => (a0.value, b0.value)
    }

  protected final def map_powertype_instance(pt: PowertypeClass, a: Any, b: Any): Either[Throwable, (Powertype, Powertype)] =
    map_powertype_instance(pt, a) match {
      case Right(a0) => map_powertype_instance(pt, b) match {
        case Right(b0) => Right((a0, b0))
        case Left(l) => Left(l)
      }
      case Left(l) => Left(l)
    }
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
      val e = a.substring(i + 1)
      (Strings.blankopt(s), Strings.blankopt(e))
    }
  }

  protected final def to_start_end_string_option_point(v: FieldValue): (Option[String], Option[String], Boolean, Boolean) = {
    val a = v.asString
    val i = a.indexOf("~")
    if (i == -1) {
      RAISE.invalidArgumentFault(s"Unavailable range: ${a}")
    } else {
      val s0 = a.substring(0, i)
      val (s, low) = _inex(s0)
      val e0 = a.substring(i + 1)
      val (e, high) = _inex(e0)
      (Strings.blankopt(s), Strings.blankopt(e), low, high)
    }
  }

  private def _inex(p: String): (String, Boolean) =
    if (p.endsWith("!"))
      (p.substring(0, p.length - 1), false)
    else
      (p, true)
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

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
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

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
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

case class EnumQuery(values: List[Any]) extends QueryExpression {
  def expression(column: String) = values match {
    case m: Seq[_] => s"""${column} IN (${to_literal_list(m)})"""
    case m => s"${column} <> ${to_literal_list(values)}"
  }
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    // map_powertype_values(pt, values).map(_.right.map(x => copy(x)))
    RAISE.notImplementedYetDefect
  }
}
object EnumQuery extends QueryExpressionClass {
  val name = "enum"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression =
    params match {
      case Nil => _create_comma(v)
      case "comma" :: Nil => _create_comma(v)
      case m => RAISE.invalidArgumentFault(s"Unknown delimiter: $m")
    }

  private def _create_comma(v: FieldValue): QueryExpression = _create(",", v)

  private def _create(delimiter: String, v: FieldValue): QueryExpression = {
    val r = v.takeList.flatMap {
      case m: String => Strings.totokens(m, delimiter)
      case m => List(m)
    }
    EnumQuery(r)
  }
}

case class EnumOrNullQuery(values: List[Any]) extends QueryExpression {
  def expression(column: String) = values match {
    case m: Seq[_] => s"""(${column} IN (${to_literal_list(m)}) OR ${column} IS NULL)"""
    case m => s"(${column} <> ${to_literal_list(values)} OR ${column} IS NULL)"
  }
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    // map_powertype_values(pt, values).map(_.right.map(x => copy(x)))
    RAISE.notImplementedYetDefect
  }
}
object EnumOrNullQuery extends QueryExpressionClass {
  val name = "enum-or-null"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression =
    params match {
      case Nil => _create_comma(v)
      case "comma" :: Nil => _create_comma(v)
      case m => RAISE.invalidArgumentFault(s"Unknown delimiter: $m")
    }

  private def _create_comma(v: FieldValue): QueryExpression = _create(",", v)

  private def _create(delimiter: String, v: FieldValue): QueryExpression = {
    val r = v.takeList.flatMap {
      case m: String => Strings.totokens(m, delimiter)
      case m => List(m)
    }
    EnumOrNullQuery(r)
  }
}

case class GreaterQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} > ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object GreaterQuery extends QueryExpressionClass {
  val name = "greater"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = GreaterQuery(v.asString)
}

case class GreaterEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} >= ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object GreaterEqualQuery extends QueryExpressionClass {
  val name = "greater-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = GreaterEqualQuery(v.asString)
}

case class LesserQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} < ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object LesserQuery extends QueryExpressionClass {
  val name = "lesser"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LesserQuery(v.asString)
}

case class LesserEqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = s"${column} <= ${to_literal(value)}"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object LesserEqualQuery extends QueryExpressionClass {
  val name = "lesser-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LesserEqualQuery(v.asString)
}

case class RangeQuery(start: Option[Any], end: Option[Any], low: Boolean, high: Boolean) extends QueryExpression {
  def expression(column: String) = {
    val l = start.map { x =>
      val operator = if (low) "<=" else "<"
      s"(${to_literal(start)} $operator ${column}"
    }
    val r = end.map { x =>
      val operator = if (high) "<=" else "<"
      s"(${column} $operator ${to_literal(start)}"
    }
    Vector(l, r).flatten.mkString(" AND ")
  }
  
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value_option(pt, start, end).right.map {
      case (a, b) => copy(start = a, end = b)
    }
  }
}
object RangeQuery extends QueryExpressionClass {
  val name = "range"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = {
    val (start, end, low, high) = to_start_end_string_option_point(v)
    RangeQuery(start, end, low, high)
  }
}

case class RangeInclusiveQuery(start: Any, end: Any) extends QueryExpression {
  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} <= ${to_literal(end)})"
  def isAccept(p: Any): Boolean = RAISE.notImplementedYetDefect

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, start, end).right.map {
      case (a, b) => copy(a, b)
    }
  }
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

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, start, end).right.map {
      case (a, b) => copy(a, b)
    }
  }
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

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, start, end).right.map {
      case (a, b) => copy(a, b)
    }
  }
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

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, start, end).right.map {
      case (a, b) => copy(a, b)
    }
  }
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
    val (start, end, low, high) = to_start_end_string_option_point(v)
    // val (low, high) = params match {
    //   case Nil => (true, true)
    //   case "inclusive" :: Nil => (true, true)
    //   case "exclusive" :: Nil => (false, false)
    //   case "inclusive" :: "inclusive" :: Nil => (true, true)
    //   case "inclusive" :: "exclusive" :: Nil => (true, false)
    //   case "exclusive" :: "inclusive" :: Nil => (false, true)
    //   case "exclusive" :: "exclusive" :: Nil => (false, true)
    //   case _ => RAISE.invalidArgumentFault(s"Invalid parameter '${params.mkString("_")}' in datetime-period")
    // }
    val builder = DateTimePeriod.Builder(ctx.datetime, ctx.timezoneJoda)
    val s = start.map(builder.toDateTime)
    val e = end.map(builder.toDateTime)
    DateTimePeriodQuery(DateTimePeriod(s, e, low, high))
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
    EnumQuery,
    GreaterQuery,
    GreaterEqualQuery,
    LesserQuery,
    LesserEqualQuery,
    RangeQuery,
    RangeInclusiveQuery, // obsolated
    RangeExclusiveQuery, // obsolated
    RangeInclusiveExclusiveQuery, // obsolated
    RangeExclusiveInclusiveQuery, // obsolated
    DateTimePeriodQuery,
    LikeQuery,
    RegexQuery,
    FirstMatchQuery,
    LastMatchQuery,
    FirstLastMatchQuery
  )

  def isDefined(key: String): Boolean = expressions.exists(_.name == key)

  def isQueryKey(key: String): Boolean =
    ParameterKey.parse(key).adornment.collect {
      case ADORNMENT_QUERY => true
    }.getOrElse(false)

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

