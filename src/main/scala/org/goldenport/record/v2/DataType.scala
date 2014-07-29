package org.goldenport.record.v2

import java.util.UUID
import java.sql.{Timestamp, Date, Time}
import java.net.URL
import java.text.SimpleDateFormat
import com.asamioffice.goldenport.io.UURL
import org.joda.time._
import scala.util.control.NonFatal
import scala.math._
import scalaz._, Scalaz._
import Validator._
import org.goldenport.record.util.{
  DateUtils, TimeUtils, TimestampUtils, DateTimeUtils}

/*
 * @snice   Nov. 23, 2012
 *  version Dec. 18, 2012
 *  version Jan. 29, 2013
 *  version Feb. 20, 2013
 *  version Mar. 12, 2013
 *  version Dec. 31, 2013
 *  version Jan. 29, 2014
 *  version Feb.  6, 2014
 *  version May. 15, 2014
 * @version Jul. 27, 2014
 * @author  ASAMI, Tomoharu
 */
sealed trait DataType {
  type InstanceType
  val name = getClass.getSimpleName.substring(1).filterNot(_ == '$').toLowerCase // eliminame 'X'
  def toInstance(v: Any): InstanceType
  def validate(d: Any): ValidationResult
  def label: String
  def mapData(s: String): String = s
  def toDouble(s: String): Validation[Throwable, Double] = new NumberFormatException(s).fail
  def toDecimal(s: String): Validation[Throwable, BigDecimal] = {
    try {
      BigDecimal(s).success
    } catch {
      case NonFatal(e) => e.fail
    }
  }
  def isValue: Boolean = true
  def isReference: Boolean = !isValue
  def isSqlString: Boolean = true
  def getXmlDatatypeName: Option[String] = None

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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("boolean")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("byte")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("short")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("int")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("long")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("double")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("integer")
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("decimal")
}

case object XString extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "文字列"
  override def getXmlDatatypeName = Some("string")
}

case object XToken extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = {
    d match {
      case x: String => try {
        if (x.contains('\n') || x.contains('\r') || x.contains('\t'))
          ValueDomainFailure("Invalid token (%s)", d.toString)
        else
          Valid
      }
      case _ => Valid // for status_id
//      case _ => ValueDomainFailure("Invalid token (%s)", d.toString)
    }
  }
  def label = "トークン"
  override def mapData(s: String) = {
    s.replace("　", " ").trim
  }
  override def getXmlDatatypeName = Some("token")
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

  override def format(value: Any): String = {
    TimeUtils.toIsoTimeString(toInstance(value))
  }
}

case object XDateTime extends DataType {
  type InstanceType = Timestamp
  def toInstance(x: Any): InstanceType = {
    x match {
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

  override def format(value: Any): String = {
    TimestampUtils.toIsoDateTimeString(toInstance(value))
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
  override def isSqlString = false
  override def getXmlDatatypeName = Some("decimal")
}

case object XPercent extends DataType {
  type InstanceType = Double
  def toInstance(x: Any): InstanceType = XDouble.toInstance(x)

  def validate(d: Any): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseDouble
  def label = "パーセント"
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
}

case object XUnit extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "単位"
  override def getXmlDatatypeName = Some("token")
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

case class XPowertype() extends DataType {
  type InstanceType = String
  def toInstance(x: Any): InstanceType = {
    x.toString
  }

  def validate(d: Any): ValidationResult = Valid
  def label = "区分"
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
