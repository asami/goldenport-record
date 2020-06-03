package org.goldenport.record.v2

import java.util.UUID
import java.sql.{Timestamp, Date, Time}
import java.net.{URL, URI}
import java.text.SimpleDateFormat
import com.asamioffice.goldenport.io.UURL
import org.joda.time._
import scala.util.control.NonFatal
import scala.math._
import scalaz._, Scalaz._
import Validator._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.extension.IRecord
import org.goldenport.i18n.I18NString
import org.goldenport.values.DateTimePeriod
import org.goldenport.record.query._
import org.goldenport.record.util.{
  DateUtils, TimeUtils, TimestampUtils, DateTimeUtils, AnyUtils}

/*
 * @since   Nov. 23, 2012
 *  version Dec. 18, 2012
 *  version Jan. 29, 2013
 *  version Feb. 20, 2013
 *  version Mar. 12, 2013
 *  version Dec. 31, 2013
 *  version Jan. 29, 2014
 *  version Feb.  6, 2014
 *  version May. 15, 2014
 *  version Jul. 27, 2014
 *  version Sep. 25, 2015
 *  version Jun. 16, 2016
 *  version Jan. 23, 2017
 *  version Sep. 21, 2017
 *  version Oct. 22, 2017
 *  version Nov. 13, 2017
 *  version Jan. 21, 2018
 *  version Jan. 10, 2019
 *  version Jul. 31, 2019
 *  version Aug. 16, 2019
 *  version Nov.  4, 2019
 * @version Jan.  9, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait DataType {
  type InstanceType
  val name = getClass.getSimpleName.substring(1).filterNot(_ == '$').toLowerCase // eliminame 'X'
  def toInstance(v: Any): InstanceType
  def validate(d: Any): ValidationResult
  def label: String
  def labelI18N: I18NString = I18NString(label)
  def mapData(s: String): String = s
  def toDouble(s: String): Validation[Throwable, Double] = new NumberFormatException(s).failure
  def toDecimal(s: String): Validation[Throwable, BigDecimal] = {
    try {
      BigDecimal(s).success
    } catch {
      case NonFatal(e) => e.failure
    }
  }
  def isValue: Boolean = true
  def isNumber: Boolean = false
  def isReference: Boolean = !isValue
  def isSqlString: Boolean = true
  def getXmlDatatypeName: Option[String] = None
  def getHtmlInputTypeName: Option[String] = None

  def format(value: Any): String = value.toString

  protected final def normalize_long(s: String): String = {
    val i = s.lastIndexOf(".")
    if (i == -1) s
    else if (s.substring(i + 1).exists(_ != '0') == false) {
      s.substring(0, i)
    } else {
      s
    }
  }

  protected def validate_value(validator: Any => Boolean, v: Any) = {
    if (validator(v))
      Valid
    else
      DataTypeFailure.create(this, v)
  }

  def toQueryExpression(ctx: QueryExpression.Context, p: Any): QueryExpression = p match {
    case m: QueryExpression => m
    case m: String => string_to_query_exrepssion(ctx, m)
    case m => EqualQuery(m)
  }

  protected def can_period: Boolean = this match {
    case XBoolean => true
    case XByte => true
    case XShort => true
    case XInt => true
    case XLong => true
    case XFloat => true
    case XDouble => true
    case XInteger => true
    case XDecimal => true
    case XDateTime => true
    case XDate => true
    case XTime => true
    case _ => false
  }

  protected def string_to_query_exrepssion(
    ctx: QueryExpression.Context,
    p: String
  ): QueryExpression =
    if (can_period && p.contains("~"))
      period_to_query_expression(ctx, p)
    else
      EqualQuery(p)

  protected def period_to_query_expression(
    ctx: QueryExpression.Context,
    p: String
  ): QueryExpression =
    DataType.periodRegex.findFirstMatchIn(p).
      map { x =>
        val s = _option_any(x.group(1))
        val si = _inclusive(x.group(2))
        val e = _option_any(x.group(3))
        val ei = _inclusive(x.group(4))
        try {
          period_To_Query_Expression(s.map(toInstance), si, e.map(toInstance), ei)
        } catch {
          case NonFatal(e) => EqualQuery(p)
        }
      }.getOrElse(EqualQuery(p))

  protected def period_To_Query_Expression(
    s: Option[InstanceType],
    si: Boolean,
    e: Option[InstanceType],
    ei: Boolean
  ): QueryExpression = (s, e) match {
    case (Some(l), Some(r)) => _both_side(l, si, r, ei)
    case (Some(l), None) => _left_side(l, si)
    case (None, Some(r)) => _right_side(r, ei)
    case (None, None) => NoneQuery
  }

  private def _both_side(
    s: InstanceType,
    si: Boolean,
    e: InstanceType,
    ei: Boolean
  ) = (si, ei) match {
    case (true, true) => RangeInclusiveQuery(s, e)
    case (true, false) => RangeInclusiveExclusiveQuery(s, e)
    case (false, true) => RangeExclusiveInclusiveQuery(s, e)
    case (false, false) => RangeExclusiveQuery(s, e) 
  }

  private def _left_side(s: InstanceType, si: Boolean) =
    if (true)
      GreaterEqualQuery(s)
    else
      GreaterQuery(s)

  private def _right_side(s: InstanceType, si: Boolean) =
    if (true)
      LesserEqualQuery(s)
    else
      LesserQuery(s)

  private def _option_any(p: String): Option[String] =
    if (Strings.blankp(p))
      None
    else
      Some(p)

  private def _inclusive(p: String): Boolean = p != "!"
}

object DataType {
  private val _datatypes = Vector(
    XBoolean,
    XByte,
    XShort,
    XInt,
    XLong,
    XFloat,
    XFloat1,
    XDouble,
    XInteger,
    XDecimal,
    XString,
    XNonEmptyString,
    XNonBlankString,
    XToken,
    XNonEmptyToken,
    XNonBlankToken,
    XDate,
    XTime,
    XDateTime,
    XText,
    XNonEmptyText,
    XNonBlankText,
    XBase64,
    XBinary,
    XLink,
    XImageLink,
    XEMail,
    XMoney,
    XPercent,
    XUnit,
    XColor,
    XFile,
    XMonth,
    XMonthDay,
    XYear,
    XYearEffective,
    XYearPast,
    XYearMonth,
    XDay,
    XDuration,
    XPassword,
    // XRange
    XSearch,
    XTel,
    XWeek,
    XUuid,
    XEntityId,
    XEverforthid,
    XXml,
    XHtml,
    XRecordInstance
    // XEntityReference, XValue, XEverforthObjectReference, XPowertype, XPowertypeReference, XStateMachine, XStateMachineReference, XExternalDataType
  )

  val periodRegex = """([^~]+)([!]?)~([^!]+)([!]?)""".r

  def get(p: String): Option[DataType] = _datatypes.find(_.name == p) orElse {
    val pf: PartialFunction[String, DataType] = {
      case "statemachine" => XStateMachine()
    }
    pf.lift(p)
  }

  def to(p: String): DataType = get(p) getOrElse {
    throw RAISE.syntaxErrorFault(s"Unavailable datatype: $p")
  }

  def guessSeq(ps: Seq[Any]): DataType =  {
    @annotation.tailrec
    def go(p: List[Any], dt: DataType): DataType = p match {
      case Nil => dt
      case x :: xs =>
        val guessed = guess(x)
        if (dt == guessed)
          go(xs, dt)
        else
          unify(dt, guessed)
    }
    ps.toList match {
      case Nil => XString
      case x :: xs => go(xs, guess(x))
    }
  }

  def guess(p: Any): DataType = p match {
    case _: String => XString
    case _: Boolean => XBoolean
    case _: Byte => XByte
    case _: Short => XShort
    case _: Int => XInt
    case _: Long => XLong
    case _: Float => XFloat
    case _: Double => XDouble
    case _: BigInt => XInteger
    case _: BigDecimal => XDecimal
    case _: Date => XDate
    case _: LocalDate => XDate
    case _: Time => XTime
    case _: LocalTime => XTime
    case _: Timestamp => XDateTime
    case _: DateTime => XDateTime
    case _: URL => XLink
    case _: Record => XRecordInstance
    case _: IRecord => XRecordInstance
    case m: Seq[_] => guessSeq(m)
    case m: Array[_] => guessSeq(m.toList)
    case _ => XString
  }

  def unify(lhs: DataType, rhs: DataType): DataType =
    lhs match {
      case XString => XString
      case XBoolean => rhs match {
        case XString => XString
        case XBoolean => XBoolean
        case XByte => XByte
        case XShort => XShort
        case XInt => XInt
        case XLong => XLong
        case XFloat => XFloat
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XByte => rhs match {
        case XString => XString
        case XBoolean => XByte
        case XByte => XByte
        case XShort => XShort
        case XInt => XInt
        case XLong => XLong
        case XFloat => XFloat
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XShort => rhs match {
        case XString => XString
        case XBoolean => XShort
        case XByte => XShort
        case XShort => XShort
        case XInt => XInt
        case XLong => XLong
        case XFloat => XFloat
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XInt => rhs match {
        case XString => XString
        case XBoolean => XInt
        case XByte => XInt
        case XShort => XInt
        case XInt => XInt
        case XLong => XLong
        case XFloat => XFloat
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XLong => rhs match {
        case XString => XString
        case XBoolean => XLong
        case XByte => XLong
        case XShort => XLong
        case XInt => XLong
        case XLong => XLong
        case XFloat => XFloat
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XFloat => rhs match {
        case XString => XString
        case XBoolean => XFloat
        case XByte => XFloat
        case XShort => XFloat
        case XInt => XFloat
        case XLong => XFloat
        case XFloat => XFloat
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XDouble => rhs match {
        case XString => XString
        case XBoolean => XDouble
        case XByte => XDouble
        case XShort => XDouble
        case XInt => XDouble
        case XLong => XDouble
        case XFloat => XDouble
        case XDouble => XDouble
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XInteger => rhs match {
        case XString => XString
        case XBoolean => XInteger
        case XByte => XInteger
        case XShort => XInteger
        case XInt => XInteger
        case XLong => XInteger
        case XFloat => XDecimal
        case XDouble => XDecimal
        case XInteger => XInteger
        case XDecimal => XDecimal
        case _ => XString
      }
      case XDecimal => rhs match {
        case XString => XString
        case XBoolean => XDecimal
        case XByte => XDecimal
        case XShort => XDecimal
        case XInt => XDecimal
        case XLong => XDecimal
        case XFloat => XDecimal
        case XDouble => XDecimal
        case XInteger => XDecimal
        case XDecimal => XDecimal
        case _ => XString
      }
      case _ => if (lhs == rhs) lhs else XString
    }
}

case object XBoolean extends DataType {
  type InstanceType = Boolean
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Boolean => v
      case v => v.toString.toLowerCase match {
        case "1" => true
        case "0" => false
        case "true" => true
        case "false" => false
      }
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Boolean => Valid
      case x: String => {
        if (List("true", "false", "1", "0").contains(x.toLowerCase))
          Valid
        else {
          ValueDomainFailure("Invalid boolean (%s)", d.toString)
        }
      }
      case x => {
        if (List("1", "0").contains(x.toString))
          Valid
        else
          ValueDomainFailure("Invalid boolean (%s)", d.toString)
      }
    }
  }
  def label = "Bool値"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("boolean")
  override def getHtmlInputTypeName = Some("checkbox")
}

case object XByte extends DataType {
  type InstanceType = Byte
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Byte => v
      case v => v.toString.toByte
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case x: Short if Byte.MinValue <= x && x <= Byte.MaxValue => Valid
      case x: Int if Byte.MinValue <= x && x <= Byte.MaxValue => Valid
      case x: Long if Byte.MinValue <= x && x <= Byte.MaxValue => Valid
      case x: BigInt if x.isValidByte => Valid
      case x: BigDecimal if x.isValidByte => Valid
      case x: String => try {
        x.toByte
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid byte (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid byte", d.toString)
    }
  }
  def label = "整数値(-128~127)"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("byte")
  override def getHtmlInputTypeName = Some("number")
}

case object XShort extends DataType {
  type InstanceType = Short
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v => v.toString.toShort
    }    
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case x: Int if Short.MinValue <= x && x <= Short.MaxValue => Valid
      case x: Long if Short.MinValue <= x && x <= Short.MaxValue => Valid
      case x: BigInt if x.isValidShort => Valid
      case x: BigDecimal if x.isValidShort => Valid
      case x: String => try {
        x.toShort
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid short (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid short", d.toString)
    }
  }
  def label = "整数値(-32768~32767)"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("short")
  override def getHtmlInputTypeName = Some("number")
}

case object XInt extends DataType {
  type InstanceType = Int
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v => v.toString.toInt
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case _: Int => Valid
      case x: Long if Int.MinValue <= x && x <= Int.MaxValue => Valid
      case x: BigInt if x.isValidInt => Valid
      case x: BigDecimal if x.isValidInt => Valid
      case x: String => try {
        x.toInt
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid int (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure(s"Invalid int ($d/${d.getClass})", d.toString)
    }
  }
  override def toDouble(s: String) = s.parseInt.map(_.toDouble)
  override def toDecimal(s: String) = s.parseInt.map(_.toDouble)
  def label = "整数値(-2^31~2^31、約20億)"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("int")
  override def getHtmlInputTypeName = Some("number")
}

case object XLong extends DataType {
  type InstanceType = Long
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v: Long => v
      case v => v.toString.toLong
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case _: Int => Valid
      case _: Long => Valid
      case x: BigInt if x.isValidLong => Valid
      case x: BigDecimal if x.isValidLong => Valid
      case x: String => try {
        x.toLong
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid long (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid long", d.toString)
    }
  }
  override def toDouble(s: String) = s.parseLong.map(_.toDouble)
  def label = "整数値(-2^63~2^63)"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("long")
  override def getHtmlInputTypeName = Some("number")
}

case object XFloat extends DataType {
  type InstanceType = Float
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Float => v
      case v => v.toString.toFloat
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case _: Int => Valid
      case _: Long => Valid
      case _: Float => Valid
      case x: Double if Float.MinValue <= x && x <= Float.MaxValue => Valid
      case x: BigInt if x.isValidFloat => Valid
      case x: BigDecimal if x.isValidFloat => Valid
      case x: String => try {
        x.toFloat
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid float (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid float", d.toString)
    }
  }
  override def toDouble(s: String) = s.parseFloat.map(_.toDouble)
  def label = "浮動小数点値"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
  override def getHtmlInputTypeName = Some("number")
}

case object XFloat1 extends DataType {
  type InstanceType = Float
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Float => v
      case v => v.toString.toFloat
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case _: Int => Valid
      case _: Long => Valid
      case _: Float => Valid
      case x: Double if Float.MinValue <= x && x <= Float.MaxValue => Valid
      case x: BigInt if x.isValidFloat => Valid
      case x: BigDecimal if x.isValidFloat => Valid
      case x: String => try {
        x.toFloat
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid float (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid float", d.toString)
    }
  }
  override def toDouble(s: String) = s.parseFloat.map(_.toDouble)
  def label = "浮動小数点値"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
  override def getHtmlInputTypeName = Some("number")
}

case object XDouble extends DataType {
  type InstanceType = Double
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Float => v
      case v: Double => v
      case v => v.toString.toDouble
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Float => Valid
      case _: Double => Valid
      case x: BigInt if x.isValidDouble => Valid
      case x: BigDecimal if x.isValidDouble => Valid
      case x: String => try {
        x.toDouble
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid double (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid double", d.toString)
    }
  }
  override def toDouble(s: String) = s.parseDouble
  def label = "浮動小数点値"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("double")
  override def getHtmlInputTypeName = Some("number")
}

case object XInteger extends DataType {
  type InstanceType = BigInt
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Byte => BigInt(v)
      case v: Short => BigInt(v)
      case v: Int => BigInt(v)
      case v: Long => BigInt(v)
      case v: BigInt => v
      case v => BigInt(v.toString)
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case _: Int => Valid
      case _: Long => Valid
      case _: BigInt => Valid
      case _: java.math.BigInteger => Valid
      case x: String => try {
        new java.math.BigInteger(x)
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid integer (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid integer", d.toString)
    }
  }
  def label = "整数値"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("integer")
  override def getHtmlInputTypeName = Some("number")
}

case object XDecimal extends DataType {
  type InstanceType = BigDecimal
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Byte => BigDecimal(v)
      case v: Short => BigDecimal(v)
      case v: Int => BigDecimal(v)
      case v: Long => BigDecimal(v)
      case v: Float => BigDecimal(v)
      case v: Double => BigDecimal(v)
      case v: BigInt => BigDecimal(v)
      case v: BigDecimal => v
      case v => BigDecimal(v.toString)
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case _: Short => Valid
      case _: Int => Valid
      case _: Long => Valid
      case _: BigInt => Valid
      case _: java.math.BigInteger => Valid
      case _: Float => Valid
      case _: Double => Valid
      case _: BigDecimal => Valid
      case x: String => try {
        new java.math.BigDecimal(x)
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid decimal (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid decimal", d.toString)
    }
  }
  def label = "数値"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("decimal")
  override def getHtmlInputTypeName = Some("number")
}

case object XString extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "文字列"
  override def getXmlDatatypeName = Some("string")
  override def getHtmlInputTypeName = Some("text")
}

case object XNonEmptyString extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = XString.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XString.validate(d) match {
      case Valid =>
        val v: String = XString.toInstance(d)
        if (Strings.notemptyp(v))
          Valid
        else
          ValueDomainFailure(s"Empty String", v)
      case m => m
    }
  }
  def label = XString.label
  override def getXmlDatatypeName = XString.getXmlDatatypeName
  override def format(value: Any): String = XString.format(value)
}

case object XNonBlankString extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = XString.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XString.validate(d) match {
      case Valid =>
        val v: String = XString.toInstance(d)
        if (Strings.notblankp(v))
          Valid
        else
          ValueDomainFailure(s"Blank String", v)
      case m => m
    }
  }
  def label = XString.label
  override def getXmlDatatypeName = XString.getXmlDatatypeName
  override def format(value: Any): String = XString.format(value)
}

case object XToken extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case x: String =>
        if (x.contains('\n') || x.contains('\r') || x.contains('\t'))
          ValueDomainFailure("Invalid token (%s)", d.toString)
        else
          Valid
      case _ => Valid // for status_id
//      case _ => ValueDomainFailure("Invalid token (%s)", d.toString)
    }
  }
  def label = "トークン"
  override def mapData(s: String) = {
    s.replace("　", " ").trim
  }
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("text")
}

case object XNonEmptyToken extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = XToken.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XToken.validate(d) match {
      case Valid =>
        val v: String = XToken.toInstance(d)
        if (Strings.notemptyp(v))
          Valid
        else
          ValueDomainFailure(s"Empty Token", v)
      case m => m
    }
  }
  def label = XToken.label
  override def getXmlDatatypeName = XToken.getXmlDatatypeName
  override def format(value: Any): String = XToken.format(value)
}

case object XNonBlankToken extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = XToken.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XToken.validate(d) match {
      case Valid =>
        val v: String = XToken.toInstance(d)
        if (Strings.notblankp(v))
          Valid
        else
          ValueDomainFailure(s"Blank String", v)
      case m => m
    }
  }
  def label = XToken.label
  override def getXmlDatatypeName = XToken.getXmlDatatypeName
  override def format(value: Any): String = XToken.format(value)
}

case object XDate extends DataType {
  type InstanceType = Date
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Date => v
      case v: Long => new Date(v)
      case s: String => new Date(DateUtils.parse(s).getTime)
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: java.util.Date => Valid
      case x: String => try {
        val df = new SimpleDateFormat("yyyy-MM-dd")
        df.parse(x)
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid date (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid date", d.toString)
    }
  }
  def label = "日付"
  override def isSqlString = true
  override def getXmlDatatypeName = Some("date")
  override def getHtmlInputTypeName = Some("date")

  override def format(value: Any): String = {
    DateUtils.toIsoDateString(toInstance(value))
  }
}

case object XTime extends DataType {
  type InstanceType = Time
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Time => v
      case v: Long => new Time(v)
      case s: String => TimeUtils.parse(s)
    }
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case _: java.util.Date => Valid
      case x: String => try {
        DateTime.parse(x) // XXX
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid time (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid time", d.toString)
    }
  }
  def label = "時間"
  override def isSqlString = true
  override def getXmlDatatypeName = Some("time")
  override def getHtmlInputTypeName = Some("time")

  override def format(value: Any): String = {
    TimeUtils.toIsoTimeString(toInstance(value))
  }
}

// Not preserve timezone
case object XDateTime extends DataType {
  type InstanceType = Timestamp
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: Int => new Timestamp(v)
      case v: Long => new Timestamp(v)
      case v: java.util.Date => new Timestamp(v.getTime)
      case v: DateTime => new Timestamp(v.getMillis)
      case v: String => TimestampUtils.parseIso(v)
    }
  }

  override val name = "dateTime"
  def validate(d: Any): ValidationResult = {
    d match {
      case _: java.util.Date => Valid
      case _: DateTime => Valid
      case x: String => try {
        DateTime.parse(x)
        Valid
      } catch {
        case NonFatal(e) => {
          ValueDomainFailure("Invalid dateTime (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid dateTime", d.toString)
    }
  }
  def label = "日時"
  override def isSqlString = true
  override def getXmlDatatypeName = Some("dateTime")
  override def getHtmlInputTypeName = Some("datetime-local")

  override def format(value: Any): String = {
    TimestampUtils.toIsoDateTimeString(toInstance(value))
  }

  override protected def period_to_query_expression(ctx: QueryExpression.Context, p: String): QueryExpression = {
    val builder = DateTimePeriod.Builder(ctx.datetime, ctx.timezoneJoda)
    DateTimePeriodQuery(builder.fromExpression(p))
  }
}

case object XText extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "テキスト"
}

case object XNonEmptyText extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = XText.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XString.validate(d) match {
      case Valid =>
        val v: String = XText.toInstance(d)
        if (Strings.notemptyp(v))
          Valid
        else
          ValueDomainFailure(s"Empty Text", v)
      case m => m
    }
  }
  def label = XString.label
  override def getXmlDatatypeName = XText.getXmlDatatypeName
  override def format(value: Any): String = XText.format(value)
}

case object XNonBlankText extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = XText.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XString.validate(d) match {
      case Valid =>
        val v: String = XText.toInstance(d)
        if (Strings.notblankp(v))
          Valid
        else
          ValueDomainFailure(s"Blank Text", v)
      case m => m
    }
  }
  def label = XString.label
  override def getXmlDatatypeName = XText.getXmlDatatypeName
  override def format(value: Any): String = XText.format(value)
}

case object XBase64 extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = x.toString

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "BASE64"
}

case object XBinary extends DataType {
  type InstanceType = Any
  def toInstance(x: Any): InstanceType = x

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "Binary"
}

case object XLink extends DataType {
  type InstanceType = URL
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: URL => v
      case v: String => UURL.getURLFromFileOrURLName(v)
    }
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "リンク"
  override def getXmlDatatypeName = Some("anyURI")
  override def getHtmlInputTypeName = Some("url")
}

case object XImageLink extends DataType {
  type InstanceType = URL
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: URL => v
      case v: String => UURL.getURLFromFileOrURLName(v)
    }
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "画像"
  override def getXmlDatatypeName = Some("anyURI")
  override def getHtmlInputTypeName = Some("image")
}

case object XEMail extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "メール"
}

case object XMoney extends DataType {
  type InstanceType = BigDecimal
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: BigDecimal => v
      case v: BigInt => new BigDecimal(new java.math.BigDecimal(v.underlying))
      case v => new BigDecimal(new java.math.BigDecimal(v.toString))
    }
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "金額"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("decimal")
  override def getHtmlInputTypeName = Some("number")
}

case object XPercent extends DataType {
  type InstanceType = Double
  def toInstance(x: Any): InstanceType = XDouble.toInstance(x)

  def validate(d: Any): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseDouble
  def label = "パーセント"
  override def isNumber: Boolean = true
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
  override def getHtmlInputTypeName = Some("number")
}

case object XUnit extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "単位"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("text")
}

// HTML
case object XColor extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "色"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("color")
}

// HTML
case object XFile extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "ファイル"
  override def getHtmlInputTypeName = Some("file")
}

// HTML
case object XMonth extends DataType {
  type InstanceType = Int
  def toInstance(x: Any): InstanceType = XInt.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XInt.validate(d) match {
      case Valid =>
        val v: Int = XInt.toInstance(d)
        if (1 <= v && v <= 12)
          Valid
        else
          ValueDomainFailure(s"Invalid month: $v", v.toString)
      case m => m
    }
  }
  def label = "月"
  override def getXmlDatatypeName = Some("gMonth")
  override def getHtmlInputTypeName = Some("month")
  override def format(value: Any): String = XInt.format(value)
}

// XML Datatype
case object XMonthDay extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "月日"
  override def getXmlDatatypeName = Some("gMonthDay")
  override def getHtmlInputTypeName = Some("text")
}

// XML Datatype
case object XYear extends DataType {
  type InstanceType = Int
  def toInstance(x: Any): InstanceType = AnyUtils.toInt(x)
  def validate(d: Any): ValidationResult = Valid
  def label = "年"
  override def getXmlDatatypeName = Some("gYear")
  override def getHtmlInputTypeName = Some("number")
}

case object XYearEffective extends DataType {
  type InstanceType = Int
  def toInstance(x: Any): InstanceType = XInt.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XInt.validate(d) match {
      case Valid =>
        val v: Int = XInt.toInstance(d)
        if (1900 <= v && v <= 2100)
          Valid
        else
          ValueDomainFailure(s"Invalid year: $v", v.toString)
      case m => m
    }
  }
  def label = "年"
  override def getXmlDatatypeName = Some("gYear")
  override def getHtmlInputTypeName = Some("number")
  override def format(value: Any): String = XInt.format(value)
}

case object XYearPast extends DataType {
  type InstanceType = Int
  def toInstance(x: Any): InstanceType = XInt.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XInt.validate(d) match {
      case Valid =>
        val v: Int = XInt.toInstance(d)
        RAISE.notImplementedYetDefect
      case m => m
    }
  }
  def label = "年"
  override def getXmlDatatypeName = Some("gYear")
  override def getHtmlInputTypeName = Some("number")
  override def format(value: Any): String = XInt.format(value)
}

// XML Datatype
case object XYearMonth extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "年"
  override def getXmlDatatypeName = Some("gYearMonth")
  override def getHtmlInputTypeName = Some("text")
}

// XML Datatype
case object XDay extends DataType {
  type InstanceType = Int
  def toInstance(x: Any): InstanceType = XInt.toInstance(x)
  def validate(d: Any): ValidationResult = {
    XInt.validate(d) match {
      case Valid =>
        val v: Int = XInt.toInstance(d)
        if (1 <= v && v <= 31)
          Valid
        else
          ValueDomainFailure(s"Invalid month: $v", v.toString)
      case m => m
    }
  }
  def label = "日"
  override def getXmlDatatypeName = Some("gDay")
  override def getHtmlInputTypeName = Some("number")
  override def format(value: Any): String = XInt.format(value)
}

// XML Datatype
case object XDuration extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "年"
  override def getXmlDatatypeName = Some("duration")
  override def getHtmlInputTypeName = Some("text")
}

// HTML
case object XPassword extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "パスワード"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("password")
}

// HTML
case class XRange(min: Long, max: Long, step: Long) extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "範囲"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("range")
}

// HTML
case object XSearch extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "検索"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("searchbox")
}

// HTML
case object XTel extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "電話番号"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("tel")
}

// HTML
case object XWeek extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "週"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("week")
}

case object XUuid extends DataType {
  type InstanceType = UUID
  def toInstance(x: Any): InstanceType = {
    x match {
      case v: UUID => v
      case _ => UUID.fromString(x.toString)
    }
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "UUID"
  override def getXmlDatatypeName = Some("token")
  override def getHtmlInputTypeName = Some("text")
}

case object XEntityId extends DataType { // XEntityReference
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "EntityID"
  override def getXmlDatatypeName = Some("token")
}

case object XEverforthid extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "EverforthID"
  override def getXmlDatatypeName = Some("token")
}

case object XXml extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "XML"
}

case object XHtml extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "HTML"
}

case class XEntityReference(schema: Schema, reader: (java.sql.Connection, Schema, Record) => RecordSet) extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "参照"
  override def isSqlString = false // typical case
  override def isValue = false
}

case class XValue(schema: Schema) extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "埋込み値"
  override def isSqlString = false // typical case
  override def isValue = true
  override def isReference = false
}

case object XRecordInstance extends DataType {
  type InstanceType = Record
  def toInstance(x: Any): InstanceType = x match {
    case m: Record => m
  }

  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "レコード"
  override def isSqlString = false // typical case
  override def isValue = true
  override def isReference = false
}

case class XEverforthObjectReference(schema: Schema, reader: (java.sql.Connection, Schema, Record) => RecordSet) extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "参照"
  override def isSqlString = true
  override def isValue = false
}

case class XPowertype(powertype: PowertypeClass) extends DataType {
  type InstanceType = Int
  def toInstance(v: Any): InstanceType = powertype.toInstance(v)
  def validate(v: Any): ValidationResult = validate_value(powertype.validate, v)

  def label = "区分"
  override def isSqlString = false
  override def isValue = true
  override def isReference = false
}

object XPowertype {
  def apply(): XPowertype = XPowertype(NumberPowertype)
}

case class XPowertypeReference() extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "区分"
}

case class XStateMachine() extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "ワークフロー"
}

case class XStateMachineReference() extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "ワークフロー"
}

trait XExternalDataType extends DataType {
}
