package org.goldenport.record.query

import java.sql.Timestamp
import org.joda.time._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.DateTimeContext
import org.goldenport.record.v2.{Schema, Column, DataType}
import org.goldenport.record.v2.{Record => Record2, Field => Field2}
import org.goldenport.record.v2.{PowertypeClass, Powertype}
import org.goldenport.record.v3.{Record => Record3, Field => Field3}
import org.goldenport.record.v3.{FieldValue, EmptyValue, SingleValue, MultipleValue}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.sql.SqlU
import org.goldenport.record.util.{DateUtils, AnyUtils}
import org.goldenport.values.{DateTimePeriod, ParameterKey}
import org.goldenport.values.{NumberInterval, LocalDateTimeInterval}
import org.goldenport.util.NumberUtils
import QueryExpression.Context

/*
 * @since   Jun. 25, 2018
 *  version Jan. 10, 2019
 *  version Jul. 31, 2019
 *  version Aug. 16, 2019
 *  version Oct. 15, 2019
 *  version Nov. 29, 2019
 *  version Jan. 26, 2020
 *  version Mar. 28, 2020
 *  version Feb. 28, 2021
 *  version Nov.  5, 2021
 *  version Jan.  7, 2022
 * @version Jun. 17, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait QueryExpression {
  import QueryExpression._

  def expression(column: String): String // XXX legacy?
  def where(schema: Schema, columnname: String)(implicit ctx: SqlContext): String =
    schema.getColumn(columnname).map(where(_)).getOrElse {
      if (ctx.isWhereUndefinedColumn)
        expression(columnname)
      else
        RAISE.syntaxErrorFault(s"Missing column '${columnname}' in the schema.")
    }
  def where(column: Column)(implicit ctx: SqlContext): String = expression(column.name)
  def isAccept(p: Any): Boolean

  def mapPowertypeOrException(pt: PowertypeClass): Either[Throwable, QueryExpression] = Right(this)

  protected final def to_literal(p: Any): String = SqlU.literal(p)

  protected final def to_literal(c: Column, d: Any): String = to_literal(c.datatype, d)

  protected final def to_literal(dt: DataType, d: Any): String = SqlU.literal(dt, d)

  protected final def to_literal_list(ps: Seq[Any]): String = ps.map(to_literal).mkString(", ")

  protected final def to_literal_list(c: Column, ps: Seq[Any]): String = to_literal_list(c.datatype, ps)

  protected final def to_literal_list(dt: DataType, ps: Seq[Any]): String = ps.map(to_literal(dt, _)).mkString(", ")

  protected final def to_literal_ctx(p: Any)(implicit ctx: SqlContext): String = SqlU.literal(p)

  protected final def to_literal_ctx(c: Column, d: Any)(implicit ctx: SqlContext): String = to_literal_ctx(c.datatype, d)

  protected final def to_literal_ctx(dt: DataType, d: Any)(implicit ctx: SqlContext): String = {
    import org.goldenport.record.v2._
    dt match {
      case XDateTime => RAISE.notImplementedYetDefect
      case XDate => RAISE.notImplementedYetDefect
      case XString => to_literal(dt, d)
      case _ => to_literal(dt, d)
    }
  }

  protected final def to_literal_list_ctx(ps: Seq[Any])(implicit ctx: SqlContext): String = ps.map(to_literal).mkString(", ")

  protected final def to_literal_list_ctx(c: Column, ps: Seq[Any])(implicit ctx: SqlContext): String = to_literal_list(c.datatype, ps)

  protected final def to_literal_list_ctx(dt: DataType, ps: Seq[Any])(implicit ctx: SqlContext): String = ps.map(to_literal(dt, _)).mkString(", ")

  protected final def map_powertype_value(pt: PowertypeClass, v: Any): Either[Throwable, Any] =
    map_powertype_instance(pt, v).right.map(_.value)

  protected final def map_powertype_value_eager(pt: PowertypeClass, v: Any): Either[Throwable, Seq[Any]] =
    v match {
      case m: String => map_powertype_values(pt, Strings.totokens(m, ","))
      case m => map_powertype_values(pt, Vector(m))
    }

  protected final def map_powertype_values_eager(pt: PowertypeClass, vs: Seq[Any]): Either[Throwable, Seq[Any]] = {
    val xs = vs.flatMap {
      case m: String => Strings.totokens(m, ",")
      case m => Vector(m)
    }
    map_powertype_values(pt, xs)
  }

  protected final def map_powertype_values(pt: PowertypeClass, vs: Seq[Any]): Either[Throwable, Seq[Any]] = {
    val a = vs.map(map_powertype_value(pt, _))
    case class Z(e: Option[Throwable] = None, xs: Vector[Any] = Vector.empty) {
      def r = e.map(Left(_)).getOrElse(Right(xs))

      def +(rhs: Either[Throwable, Any]) =
        if (e.isDefined)
          this
        else
          rhs match {
            case Right(x) => copy(xs = xs :+ x)
            case Left(e) => copy(e = Some(e))
          }
    }
    a./:(Z())(_+_).r
  }

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

trait ValueQueryExpressionClass {
  def name: String

  def createValueOption(name: String)(implicit ctx: Context): Option[QueryExpression] =
    if (this.name == name)
      Some(createValueExpression(name))
    else
      None

  def createValueExpression(name: String): QueryExpression
}

case object NoneQuery extends QueryExpression {
  def expression(column: String) = "1 = 1"
  def isAccept(p: Any): Boolean = true

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this
}

case class EqualQuery(value: Any) extends QueryExpression {
  def expression(column: String) = value match {
    case m: Seq[_] => s"""${column} IN (${to_literal_list(m)})"""
    case m: NumberInterval =>
      val (start, si) = m.toLower
      val (end, ei) = m.toUpper
      val q = RangeQuery(start, end, si, ei)
      q.expression(column)
    case m: LocalDateTimeInterval => RAISE.notImplementedYetDefect
    case _ => s"${column} = ${to_literal(value)}"
  }
  override def where(column: Column)(implicit ctx: SqlContext): String = value match {
    case m: Seq[_] => s"""${column.name} IN (${to_literal_list_ctx(column, m)})"""
    case m: NumberInterval =>
      val (start, si) = m.toLower
      val (end, ei) = m.toUpper
      val q = RangeQuery(start, end, si, ei)
      q.where(column)
    case m: LocalDateTimeInterval =>
      val dtp = m.toDateTimePeriod(ctx.dateTimeZone)
      val q = DateTimePeriodQuery(dtp)
      q.where(column)
    case _ => s"${column.name} = ${to_literal_ctx(column, value)}"
  }
  def isAccept(p: Any): Boolean = p == value

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value_eager(pt, value).right.map(x => copy(x))
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
  override def where(column: Column)(implicit ctx: SqlContext): String = value match {
    case m: Seq[_] => s"""${column.name} NOT IN (${to_literal_list(column, m)})"""
    case _ => s"${column.name} <> ${to_literal(column, value)}"
  }
  def isAccept(p: Any): Boolean = p != value

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value_eager(pt, value).right.map(x => copy(x))
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
  def expression(column: String) = s"""${column} IN (${to_literal_list(values)})"""
  override def where(column: Column)(implicit ctx: SqlContext): String =
    s"""${column.name} IN (${to_literal_list(column, values)})"""
  def isAccept(p: Any): Boolean = values.contains(p)

  override def mapPowertypeOrException(pt: PowertypeClass) =
    map_powertype_values(pt, values).right.map(xs => copy(xs.toList))
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
    val r = v.asList.flatMap {
      case m: String => Strings.totokens(m, delimiter)
      case m => List(m)
    }
    EnumQuery(r)
  }
}

case class EnumOrNullQuery(values: List[Any]) extends QueryExpression {
  def expression(column: String) =
    s"""(${column} IN (${to_literal_list(values)}) OR ${column} IS NULL)"""
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"""(${column.name} IN (${to_literal_list(column, values)}) OR ${column.name} IS NULL)"""
  def isAccept(p: Any): Boolean = p == null || p == None || values.contains(p)

  override def mapPowertypeOrException(pt: PowertypeClass) =
    map_powertype_values(pt, values).right.map(xs => copy(xs.toList))
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
    val r = v.asList.flatMap {
      case m: String => Strings.totokens(m, delimiter)
      case m => List(m)
    }
    EnumOrNullQuery(r)
  }
}

case class GreaterQuery(value: Any) extends QueryExpression {
  private lazy val _matcher = QueryExpression.CompareFunction.GreaterThan(value)

  def expression(column: String) = s"${column} > ${to_literal(value)}"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} > ${to_literal(column, value)}"
  def isAccept(p: Any): Boolean = _matcher(p)
    
  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object GreaterQuery extends QueryExpressionClass {
  val name = "greater"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = GreaterQuery(v.asString)
}

case class GreaterEqualQuery(value: Any) extends QueryExpression {
  private lazy val _matcher = QueryExpression.CompareFunction.GreaterEqualThan(value)

  def expression(column: String) = s"${column} >= ${to_literal(value)}"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} >= ${to_literal(column, value)}"
  def isAccept(p: Any): Boolean = _matcher(p)

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object GreaterEqualQuery extends QueryExpressionClass {
  val name = "greater-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = GreaterEqualQuery(v.asString)
}

case class LesserQuery(value: Any) extends QueryExpression {
  private lazy val _matcher = QueryExpression.CompareFunction.LesserThan(value)

  def expression(column: String) = s"${column} < ${to_literal(value)}"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} < ${to_literal(column, value)}"
  def isAccept(p: Any): Boolean = _matcher(p)

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object LesserQuery extends QueryExpressionClass {
  val name = "lesser"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LesserQuery(v.asString)
}

case class LesserEqualQuery(value: Any) extends QueryExpression {
  private lazy val _matcher = QueryExpression.CompareFunction.LesserEqualThan(value)

  def expression(column: String) = s"${column} <= ${to_literal(value)}"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} <= ${to_literal(column, value)}"
  def isAccept(p: Any): Boolean = _matcher(p)

  override def mapPowertypeOrException(pt: PowertypeClass) = {
    map_powertype_value(pt, value).right.map(x => copy(x))
  }
}
object LesserEqualQuery extends QueryExpressionClass {
  val name = "lesser-equal"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LesserEqualQuery(v.asString)
}

case class RangeQuery(start: Option[Any], end: Option[Any], low: Boolean, high: Boolean) extends QueryExpression {
  private lazy val _matcher = QueryExpression.IntervalFunction(start, end, low, high)

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
  override def where(column: Column)(implicit ctx: SqlContext): String = {
    val l = start.map { x =>
      val operator = if (low) "<=" else "<"
      s"${to_literal(column, x)} $operator ${column.name}"
    }
    val r = end.map { x =>
      val operator = if (high) "<=" else "<"
      s"${column.name} $operator ${to_literal(column, x)}"
    }
    (l, r) match {
      case (Some(a), Some(b)) => s"($a AND $b)"
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case (None, None) => "1 = 1"
    }
  }
  def isAccept(p: Any): Boolean = _matcher(p)

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
  private lazy val _matcher = QueryExpression.IntervalFunction(Some(start), Some(end), true, true)

  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} <= ${to_literal(end)})"
  override def where(column: Column)(implicit ctx: SqlContext): String =
    s"(${to_literal(column, start)} <= ${column.name} AND ${column.name} <= ${to_literal(column, end)})"
  def isAccept(p: Any): Boolean = _matcher(p)

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
  private lazy val _matcher = QueryExpression.IntervalFunction(Some(start), Some(end), false, false)

  def expression(column: String) = s"(${to_literal(start)} < ${column} AND ${column} < ${to_literal(end)})"
  override def where(column: Column)(implicit ctx: SqlContext): String =
    s"(${to_literal(column, start)} < ${column.name} AND ${column.name} < ${to_literal(column, end)})"
  def isAccept(p: Any): Boolean = _matcher(p)

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
  private lazy val _matcher = QueryExpression.IntervalFunction(Some(start), Some(end), true, false)

  def expression(column: String) = s"(${to_literal(start)} <= ${column} AND ${column} < ${to_literal(end)})"
  override def where(column: Column)(implicit ctx: SqlContext): String =
    s"(${to_literal(column, start)} <= ${column.name} AND ${column.name} < ${to_literal(column, end)})"
  def isAccept(p: Any): Boolean = _matcher.apply(p)

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
  private lazy val _matcher = QueryExpression.IntervalFunction(Some(start), Some(end), false, true)

  def expression(column: String) = s"(${to_literal(start)} < ${column} AND ${column} <= ${to_literal(end)})"
  override def where(column: Column)(implicit ctx: SqlContext): String =
    s"(${to_literal(column, start)} < ${column.name} AND ${column.name} <= ${to_literal(column, end)})"
  def isAccept(p: Any): Boolean = _matcher(p)

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
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} ${period.toSqlBetweenDateTime}"
  def isAccept(p: Any): Boolean = p match {
    case m: java.sql.Timestamp => period.isValid(m)
    case m: java.util.Date => period.isValid(m)
    case m: DateTime => period.isValid(m)
    case _ => false
  }
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
    val builder = DateTimePeriod.Builder(ctx.datetime)
    val s = start.map(builder.toDateTime)
    val e = end.map(builder.toDateTime)
    DateTimePeriodQuery(DateTimePeriod(s, e, low, high))
  }
}

case class LikeQuery(value: String) extends QueryExpression {
  private lazy val _matcher = QueryExpression.LikeFunction(value)

  def expression(column: String) = s"${column} LIKE ${to_literal(value)}"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} LIKE ${to_literal(column, value)}"
  def isAccept(p: Any): Boolean = _matcher(p)
}
object LikeQuery extends QueryExpressionClass {
  val name = "like"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LikeQuery(v.asString)
}

case class RegexQuery(value: String) extends QueryExpression {
  private lazy val _matcher = QueryExpression.RegexFunction(value)

  def expression(column: String) = s"${column} REGEXP ${to_literal(value)}"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} REGEXP ${to_literal(column, value)}"
  def isAccept(p: Any): Boolean = _matcher(p)
}
object RegexQuery extends QueryExpressionClass {
  val name = "regex"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = RegexQuery(v.asString)
}

case class FirstMatchQuery(value: String) extends QueryExpression {
  def expression(column: String) = s"""${column} LIKE ${to_literal(value + "%")}"""
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"""${column.name} LIKE ${to_literal(column, value + "%")}"""
  def isAccept(p: Any): Boolean = p match {
    case m: String => m.startsWith(value)
    case _ => false
  }
}
object FirstMatchQuery extends QueryExpressionClass {
  val name = "first-match"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = FirstMatchQuery(v.asString)
}

case class LastMatchQuery(value: String) extends QueryExpression {
  def expression(column: String) = s"""${column} LIKE ${to_literal("%" + value)}"""
  override def where(column: Column)(implicit ctx: SqlContext): String =
    s"""${column.name} LIKE ${to_literal(column, "%" + value)}"""
  def isAccept(p: Any): Boolean = p match {
    case m: String => m.endsWith(value)
    case _ => false
  }
}
object LastMatchQuery extends QueryExpressionClass {
  val name = "last-match"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = LastMatchQuery(v.asString)
}

case class FirstLastMatchQuery(value: String) extends QueryExpression {
  private lazy val _matcher = QueryExpression.LikeFunction(s"%${value}%")

  def expression(column: String) = s"""${column} LIKE ${to_literal("%" + value + "%")}"""
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"""${column.name} LIKE ${to_literal(column, "%" + value + "%")}"""
  def isAccept(p: Any): Boolean = _matcher(p)
}
object FirstLastMatchQuery extends QueryExpressionClass {
  val name = "first-last-match"

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = FirstLastMatchQuery(v.asString)
}

case object IsNullQuery extends QueryExpression with QueryExpressionClass with ValueQueryExpressionClass {
  val name = "is-null"

  def expression(column: String) = s"${column} IS NULL"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} IS NULL"
  def isAccept(p: Any): Boolean = p == null || p == None

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this

  def createValueExpression(name: String) = this
}

case object IsNotNullQuery extends QueryExpression with QueryExpressionClass with ValueQueryExpressionClass  {
  val name = "is-not-null"

  def expression(column: String) = s"${column} IS NOT NULL"
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} IS NOT NULL"
  def isAccept(p: Any): Boolean = true

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this

  def createValueExpression(name: String) = this
}

case object IsEmptyQuery extends QueryExpression with QueryExpressionClass with ValueQueryExpressionClass  {
  val name = "is-empty"

  def expression(column: String) = s"""(${column} IS NULL OR ${column} = """""
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"${column.name} IS NULL"
  def isAccept(p: Any): Boolean = (p == null || p == None) || p == ""

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this

  def createValueExpression(name: String) = this
}

case object IsNotEmptyQuery extends QueryExpression with QueryExpressionClass with ValueQueryExpressionClass  {
  val name = "is-not-empty"

  def expression(column: String) = s"""(${column} IS NOT NULL AND ${column} <> """""
  override def where(column: Column)(implicit ctx: SqlContext): String = 
    s"""(${column.name} IS NOT NULL AND ${column} <> """""
  def isAccept(p: Any): Boolean = true && p != ""

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this

  def createValueExpression(name: String) = this
}

case object AllQuery extends QueryExpression with QueryExpressionClass with ValueQueryExpressionClass {
  val name = "all"

  def expression(column: String) = "1 = 1"
  def isAccept(p: Any): Boolean = true

  def create(params: List[String], v: FieldValue)(implicit ctx: Context): QueryExpression = this

  def createValueExpression(name: String) = this
}

object QueryExpression {
  import scalaz._, Scalaz._

  val ADORNMENT_QUERY = "query"

  case class Context(
    datetime: DateTimeContext,
    databaseTimeZone: DateTimeZone
  ) {
    def dateTimeZone = datetime.dateTimeZone

    def toDateTime(p: LocalDateTime): DateTime = p.toDateTime(datetime.dateTimeZone).withZone(databaseTimeZone)

    def toTimestamp(p: LocalDateTime): Timestamp = new Timestamp(toDateTime(p).getMillis)
  }
  object Context {
    def apply(p: DateTime): Context = apply(DateTimeContext(p))

    def apply(p: DateTimeContext): Context = Context(p, p.dateTimeZone)

    def now() = apply(DateTimeContext.now())
  }

  val expressions: Vector[QueryExpressionClass] = Vector(
    AllQuery, // compatibility
    IsNullQuery, // compatibility
    IsNotNullQuery, // compatibility
    EqualQuery,
    NotEqualQuery,
    EnumQuery,
    EnumOrNullQuery,
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

  val valueExpressions: Vector[ValueQueryExpressionClass] = Vector(
    AllQuery,
    IsNullQuery,
    IsNotNullQuery,
    IsEmptyQuery,
    IsNotEmptyQuery
  )

  sealed trait CompareFunction {
    def value: Any
    def apply(p: Any): Boolean

    protected lazy val int_value = AnyUtils.getInt(value)
    protected lazy val long_value = AnyUtils.getLong(value)
    protected lazy val float_value = AnyUtils.getFloat(value)
    protected lazy val double_value = AnyUtils.getDouble(value)
    protected lazy val bigint_value = AnyUtils.getBigInt(value)
    protected lazy val bigdecimal_value = AnyUtils.getBigDecimal(value)
  }
  object CompareFunction {
    case class GreaterThan(value: Any) extends CompareFunction {
      def apply(p: Any): Boolean = p match {
        case m: Byte => int_value.map(m > _) getOrElse false
        case m: Short => int_value.map(m > _) getOrElse false
        case m: Int => int_value.map(m > _) getOrElse false
        case m: Long => long_value.map(m > _) getOrElse false
        case m: Float => float_value.map(m > _) getOrElse false
        case m: Double => double_value.map(m > _) getOrElse false
        case m: BigInt => bigint_value.map(m > _) getOrElse false
        case m: BigDecimal => bigdecimal_value.map(m > _) getOrElse false
        case m: java.math.BigInteger => bigint_value.map(BigInt(m) > _) getOrElse false
        case m: java.math.BigDecimal => bigdecimal_value.map(BigDecimal(m) > _) getOrElse false
        case m: java.sql.Timestamp => RAISE.notImplementedYetDefect
        case m: DateTime => RAISE.notImplementedYetDefect
      }
    }
    case class GreaterEqualThan(value: Any) extends CompareFunction {
      def apply(p: Any): Boolean = p match {
        case m: Byte => int_value.map(m >= _) getOrElse false
        case m: Short => int_value.map(m >= _) getOrElse false
        case m: Int => int_value.map(m >= _) getOrElse false
        case m: Long => long_value.map(m >= _) getOrElse false
        case m: Float => float_value.map(m >= _) getOrElse false
        case m: Double => double_value.map(m >= _) getOrElse false
        case m: BigInt => bigint_value.map(m >= _) getOrElse false
        case m: BigDecimal => bigdecimal_value.map(m >= _) getOrElse false
        case m: java.math.BigInteger => bigint_value.map(BigInt(m) >= _) getOrElse false
        case m: java.math.BigDecimal => bigdecimal_value.map(BigDecimal(m) >= _) getOrElse false
        case m: java.sql.Timestamp => RAISE.notImplementedYetDefect
        case m: DateTime => RAISE.notImplementedYetDefect
      }
    }
    case class LesserThan(value: Any) extends CompareFunction {
      def apply(p: Any): Boolean = p match {
        case m: Byte => int_value.map(m < _) getOrElse false
        case m: Short => int_value.map(m < _) getOrElse false
        case m: Int => int_value.map(m < _) getOrElse false
        case m: Long => long_value.map(m < _) getOrElse false
        case m: Float => float_value.map(m < _) getOrElse false
        case m: Double => double_value.map(m < _) getOrElse false
        case m: BigInt => bigint_value.map(m < _) getOrElse false
        case m: BigDecimal => bigdecimal_value.map(m < _) getOrElse false
        case m: java.math.BigInteger => bigint_value.map(BigInt(m) < _) getOrElse false
        case m: java.math.BigDecimal => bigdecimal_value.map(BigDecimal(m) < _) getOrElse false
        case m: java.sql.Timestamp => RAISE.notImplementedYetDefect
        case m: DateTime => RAISE.notImplementedYetDefect
      }
    }
    case class LesserEqualThan(value: Any) extends CompareFunction {
      def apply(p: Any): Boolean = p match {
        case m: Byte => int_value.map(m <= _) getOrElse false
        case m: Short => int_value.map(m <= _) getOrElse false
        case m: Int => int_value.map(m <= _) getOrElse false
        case m: Long => long_value.map(m <= _) getOrElse false
        case m: Float => float_value.map(m <= _) getOrElse false
        case m: Double => double_value.map(m <= _) getOrElse false
        case m: BigInt => bigint_value.map(m <= _) getOrElse false
        case m: BigDecimal => bigdecimal_value.map(m <= _) getOrElse false
        case m: java.math.BigInteger => bigint_value.map(BigInt(m) <= _) getOrElse false
        case m: java.math.BigDecimal => bigdecimal_value.map(BigDecimal(m) <= _) getOrElse false
        case m: java.sql.Timestamp => RAISE.notImplementedYetDefect
        case m: DateTime => RAISE.notImplementedYetDefect
      }
    }

    case object True extends CompareFunction {
      val value  = None
      def apply(p: Any): Boolean = true
    }
  }

  case class IntervalFunction(
    low: Option[Any],
    high: Option[Any],
    inlow: Boolean,
    inhigh: Boolean
  ) {
    private val _bottom_matcher = low.map(x =>
      if (inlow)
        CompareFunction.GreaterEqualThan(x)
      else
        CompareFunction.GreaterThan(x)
    ).getOrElse(CompareFunction.True)

    private val _top_matcher = high.map(x =>
      if (inhigh)
        CompareFunction.LesserEqualThan(x)
      else
        CompareFunction.LesserThan(x)
    ).getOrElse(CompareFunction.True)

    def apply(p: Any): Boolean = {
      val a = _bottom_matcher(p)
      val b = _top_matcher(p)
      a && b
    }
  }

  case class LikeFunction(value: String) {
    case class State(marks: List[State.Mark]) {
      def apply(p: Char, next: Char): Option[State] = {
//        println(s"LikeFunction#State#apply($p, $next)[${marks}]")
        val r = marks.headOption.map {
          case State.Wildcard => marks.lift(1).map {
            case State.Wildcard => RAISE.noReachDefect
            case State.OneChar(c) => if (p == c) _next2 else _this
            case State.MatchOne => _next2
          }.getOrElse(_this)
          case State.OneChar(c) => if (p == c) _next else _error
          case State.MatchOne => _next
        }.getOrElse(_error)
//        println(s"LikeFunction#State#apply($p, $next)[${marks}] => $r")
        r
      }

      private def _this = Some(this)
      private def _next = Some(State(marks.tail))
      private def _next2 = Some(State(marks.tail.tail))
      private def _error = None

      def applyEnd(p: Char): Boolean = {
//        println(s"LikeFunction#State#applyEnd($p)[${marks}]")
        val r = marks.headOption.map {
          case State.Wildcard => _is_complete
          case State.OneChar(c) => if (p == c) _is_complete else false
          case State.MatchOne => _is_complete
        }.getOrElse(false)
//        println(s"LikeFunction#State#applyEnd($p)[${marks}] => $r")
        r
      }

      private def _is_complete = marks match {
        case Nil => true
        case x :: Nil => true
        case x :: x2 :: Nil => x2 match {
          case State.Wildcard => true
          case _ => false
        }
        case _ => false
      }
    }
    object State {
      sealed trait Mark {
      }
      case object Wildcard extends Mark
      case class OneChar(c: Char) extends Mark
      case object MatchOne extends Mark

      def parse(p: String): State = {
        case class Z(
          marks: Vector[Mark] = Vector.empty,
          isEscape: Boolean = false
        ) {
          def r = State(marks.toList)

          def +(rhs: Char) =
            if (isEscape)
              copy(marks = marks :+ OneChar(rhs), isEscape = false)
            else
              rhs match {
                case '%' => _add_wildcard
                case '_' => _add(MatchOne)
                case '\\' => copy(isEscape = true)
                case m => _add(OneChar(m))
              }

          private def _add(p: Mark) = copy(marks = marks :+ p)
          private def _add_wildcard = marks.lastOption.map {
            case Wildcard => this
            case _ => _add(Wildcard)
          }.getOrElse(_add(Wildcard))
        }
        p./:(Z())(_+_).r
      }
    }

    private val _syntax = State.parse(value)

    def apply(p: Any): Boolean = p match {
      case m: String => _is_accept(m.toList, _syntax)
      case _ => false
    }

    @annotation.tailrec
    private def _is_accept(ps: List[Char], state: State): Boolean = ps match {
      case Nil => true
      case x :: Nil => state.applyEnd(x)
      case x :: x2 :: xs => state(x, x2) match {
        case Some(s) => _is_accept(x2 :: xs, s)
        case None => false
      }
    }
  }

  case class RegexFunction(value: String) {
    private val _regex = value.r

    def apply(p: Any): Boolean = p match {
      case m: String => _regex.findFirstIn(m).isDefined
      case _ => false
    }
  }

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
    }.getOrElse {
      p.getString.flatMap { x =>
        val v = valueExpressions.toStream.flatMap(_.createValueOption(x)).headOption
        v.map(p.update(path, _))
      }.getOrElse(RAISE.invalidArgumentFault(s"Invalid query: ${p.key.name}"))
    }

  def activate(pt: PowertypeClass, p: Field2)(implicit ctx: Context): Field2 = {
    val key = ParameterKey.parse(p.key)
    key.adornment.collect {
      case ADORNMENT_QUERY => _expression(pt, p, key.path, key.adornmentArguments)
    }.getOrElse {
      val fv = _to_field_value(pt, p.toFieldValue)
      p.setFieldValue(fv)
    }
  }

  private def _expression(pt: PowertypeClass, p: Field2, path: String, adargs: List[String])(implicit ctx: Context): Field2 = 
    adargs.headOption.flatMap { x =>
      val fv = _to_field_value(pt, p.toFieldValue)
      val v = expressions.toStream.flatMap(_.createOption(x, adargs.tail, fv)).headOption
      v.map(p.update(path, _))
    }.getOrElse(RAISE.invalidArgumentFault(s"Invalid query: ${p.key.name}"))

  private def _to_field_value(pt: PowertypeClass, p: FieldValue): FieldValue =
    pt.toValues(p.asListEager).toList match {
      case Nil => EmptyValue
      case x :: Nil => SingleValue(x)
      case xs => MultipleValue(xs)
    }

  def parse(
    key: String,
    value: String
  )(implicit ctx: Context): (ParameterKey, QueryExpression) =
    parse(Field3.create(key, value))

  def parse(p: Field3)(implicit ctx: Context): (ParameterKey, QueryExpression) = {
    val key = ParameterKey.parse(p.key)
    val expr = key.adornment.collect {
      case ADORNMENT_QUERY => _record_to_expression(p, key.path, key.adornmentArguments)
    }.getOrElse(_record_to_expression(p))
    (key, expr)
  }

  private def _record_to_expression(
    p: Field3,
    path: String,
    adargs: List[String]
  )(implicit ctx: Context): QueryExpression = {
    adargs.headOption.flatMap { x =>
      val fv = p.value
      expressions.toStream.flatMap(_.createOption(x, adargs.tail, fv)).headOption
    }.getOrElse {
      valueExpressions.toStream.flatMap(_.createValueOption(p.asString)).headOption.
        getOrElse(RAISE.invalidArgumentFault(s"Invalid query: ${p.key.name}"))
    }
  }

  private def _record_to_expression(
    p: Field3
  )(implicit ctx: Context): QueryExpression = {
    p.value match {
      case EmptyValue => NoneQuery
      case SingleValue(v) => EqualQuery(v)
      case MultipleValue(vs) => EqualQuery(vs)
    }
  }

  def createEqualOption(ps: Seq[Any]): Option[QueryExpression] = ps.toList match {
    case Nil => None
    case x :: Nil => Some(EqualQuery(x))
    case xs => Some(EnumQuery(xs))
  }

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
