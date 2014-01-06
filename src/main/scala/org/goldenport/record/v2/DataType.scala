package org.goldenport.record.v2

import java.text.SimpleDateFormat
import org.joda.time._
import scala.math._
import scalaz._, Scalaz._
import Validator._

/*
 * @snice   Nov. 23, 2012
 *  version Dec. 18, 2012
 *  version Jan. 29, 2013
 *  version Feb. 20, 2013
 *  version Mar. 12, 2013
 *  version Dec. 31, 2013
 * @version Jan.  6, 2014
 * @author  ASAMI, Tomoharu
 */
sealed trait DataType {
  def validate(d: Any): ValidationResult
  def label: String
  def mapData(s: String): String = s
  def toDouble(s: String): Validation[Throwable, Double] = new NumberFormatException(s).fail
  def toDecimal(s: String): Validation[Throwable, BigDecimal] = {
    try {
      BigDecimal(s).success
    } catch {
      case e => e.fail
    }
  }
  def isValue: Boolean = true
  def isReference: Boolean = !isValue
  def isSqlString: Boolean = true
  def getXmlDatatypeName: Option[String] = None

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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Boolean => Valid
      case x: String => {
        if (List("true", "false").contains(x.toLowerCase))
          Valid
        else
          ValueDomainFailure("Invalid boolean", d.toString)
      }
      case _ => ValueDomainFailure("Invalid boolean", d.toString)
    }
  }
  def label = "Bool値"
  override def isSqlString = false
  override def getXmlDatatypeName = Some("boolean")
}

case object XByte extends DataType {
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Byte => Valid
      case x: String => try {
        x.toByte
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Short => Valid
      case x: String => try {
        x.toShort
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Int => Valid
      case x: String => try {
        x.toInt
        Valid
      } catch {
        case e => {
          ValueDomainFailure("Invalid int (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid int", d.toString)
    }
  }
  override def toDouble(s: String) = s.parseInt.map(_.toDouble)
  override def toDecimal(s: String) = s.parseInt.map(_.toDouble)
  def label = "整数値(-2^31~2^31、約20億)"
  override def isSqlString = false
  override def getXmlDatatypeName = Some("int")
}

case object XLong extends DataType {
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Long => Valid
      case x: String => try {
        x.toLong
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Float => Valid
      case x: String => try {
        x.toFloat
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Float => Valid
      case x: String => try {
        x.toFloat
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: Double => Valid
      case x: String => try {
        x.toDouble
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: BigInt => Valid
      case _: java.math.BigInteger => Valid
      case x: String => try {
        new java.math.BigInteger(x)
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = {
    d match {
      case _: BigDecimal => Valid
      case x: String => try {
        new java.math.BigDecimal(x)
        Valid
      } catch {
        case e => {
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
  def validate(d: Any): ValidationResult = Valid
  def label = "文字列"
  override def getXmlDatatypeName = Some("string")
}

case object XToken extends DataType {
  def validate(d: Any): ValidationResult = {
    d match {
      case x: String => try {
        if (x.contains('\n') || x.contains('\r') || x.contains('\t'))
          ValueDomainFailure("Invalid token", d.toString)
        else
          Valid
      }
      case _ => ValueDomainFailure("Invalid token", d.toString)
    }
  }
  def label = "トークン"
  override def mapData(s: String) = {
    s.replace("　", " ").trim
  }
  override def getXmlDatatypeName = Some("token")
}

case object XDate extends DataType {
  def validate(d: Any): ValidationResult = {
    d match {
      case _: java.util.Date => Valid
      case x: String => try {
        val df = new SimpleDateFormat("yyyy-MM-dd")
        df.parse(x)
        Valid
      } catch {
        case e => {
          ValueDomainFailure("Invalid date (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid date", d.toString)
    }
  }
  def label = "日付"
  override def isSqlString = true
  override def getXmlDatatypeName = Some("date")
}

case object XTime extends DataType {
  def validate(d: Any): ValidationResult = {
    d match {
      case _: java.util.Date => Valid
      case x: String => try {
        DateTime.parse(x) // XXX
        Valid
      } catch {
        case e => {
          ValueDomainFailure("Invalid time (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid time", d.toString)
    }
  }
  def label = "時間"
  override def isSqlString = true
  override def getXmlDatatypeName = Some("time")
}

case object XDateTime extends DataType {
  def validate(d: Any): ValidationResult = {
    d match {
      case _: java.util.Date => Valid
      case _: DateTime => Valid
      case x: String => try {
        DateTime.parse(x)
        Valid
      } catch {
        case e => {
          ValueDomainFailure("Invalid dateTime (%s)".format(e.getMessage), d.toString)
        }
      }
      case _ => ValueDomainFailure("Invalid dateTime", d.toString)
    }
  }
  def label = "日時"
  override def isSqlString = true
  override def getXmlDatatypeName = Some("dateTime")
}

case object XText extends DataType {
  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "テキスト"
}

case object XLink extends DataType {
  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "リンク"
  override def getXmlDatatypeName = Some("anyURI")
}

case object XEMail extends DataType {
  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "メール"
}

case object XMoney extends DataType {
  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "金額"
  override def isSqlString = false
  override def getXmlDatatypeName = Some("decimal")
}

case object XPercent extends DataType {
  def validate(d: Any): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseDouble
  def label = "パーセント"
  override def isSqlString = false
  override def getXmlDatatypeName = Some("float")
}

case object XUnit extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "単位"
  override def getXmlDatatypeName = Some("token")
}

case object XUuid extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "UUID"
  override def getXmlDatatypeName = Some("token")
}

case object XEverforthid extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "EverforthID"
  override def getXmlDatatypeName = Some("token")
}

case object XXml extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "XML"
}

case object XHtml extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "HTML"
}

case class XEntityReference(schema: Schema, reader: (java.sql.Connection, Schema, Record) => RecordSet) extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "参照"
  override def isSqlString = false // typical case
  override def isValue = false
}

case class XValue(schema: Schema) extends DataType {
  def validate(d: Any): ValidationResult = Valid // TODO
  def label = "埋込み値"
  override def isSqlString = false // typical case
  override def isValue = true
  override def isReference = false
}

case class XEverforthObjectReference(schema: Schema, reader: (java.sql.Connection, Schema, Record) => RecordSet) extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "参照"
  override def isSqlString = true
  override def isValue = false
}

case class XPowertype() extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "区分"
}

case class XPowertypeReference() extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "区分"
}

case class XStateMachine() extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "ワークフロー"
}

case class XStateMachineReference() extends DataType {
  def validate(d: Any): ValidationResult = Valid
  def label = "ワークフロー"
}
