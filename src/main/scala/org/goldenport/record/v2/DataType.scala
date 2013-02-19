package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._

/*
 * @snice   Nov. 23, 2012
 *  version Dec. 18, 2012
 *  version Jan. 29, 2013
 * @version Feb. 20, 2013
 * @author  ASAMI, Tomoharu
 */
sealed trait DataType {
  def validate(s: String): ValidationResult
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
  def isSqlString: Boolean = true

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
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "Bool値"
  override def isSqlString = false
}

case object XByte extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "整数値(-128~127)"
  override def isSqlString = false
}

case object XShort extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "整数値(-32768~32767)"
  override def isSqlString = false
}

case object XInt extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseInt.map(_.toDouble)
  override def toDecimal(s: String) = s.parseInt.map(_.toDouble)
  def label = "整数値(-2^31~2^31、約20億)"
  override def isSqlString = false
}

case object XLong extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseLong.map(_.toDouble)
  def label = "整数値(-2^63~2^63)"
  override def isSqlString = false
  def extjsName = "int"
}

case object XFloat extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseFloat.map(_.toDouble)
  def label = "浮動小数点値"
  override def isSqlString = false
}

case object XFloat1 extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseFloat.map(_.toDouble)
  def label = "浮動小数点値"
  override def isSqlString = false
}

case object XDouble extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseDouble
  def label = "浮動小数点値"
  override def isSqlString = false
}

case object XDecimal extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "整数値"
  override def isSqlString = false
}

case object XString extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "文字列"
}

case object XToken extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "文字列"
  override def mapData(s: String) = {
    s.replace("　", " ").trim
  }
}

case object XDate extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "日付"
  override def isSqlString = true
}

case object XTime extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "時間"
  override def isSqlString = true
}

case object XDateTime extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "日時"
  override def isSqlString = true
}

case object XText extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "テキスト"
}

case object XLink extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "リンク"
}

case object XEMail extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "メール"
}

case object XMoney extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  def label = "金額"
  override def isSqlString = false
}

case object XPercent extends DataType {
  def validate(s: String): ValidationResult = Valid // TODO
  override def toDouble(s: String) = s.parseDouble
  def label = "パーセント"
  override def isSqlString = false
}

case object XUnit extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "単位"
}

case object XUuid extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "UUID"
  def extjsName = "string"
}

case object XEverforthid extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "EverforthID"
  def extjsName = "string"
}

case object XXml extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "XML"
  def extjsName = "string"
}

case object XHtml extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "HTML"
  def extjsName = "string"
}

case class XEntityReference() extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "参照"
  override def isSqlString = false // XXX assume typical case
  def extjsName = "string"
}

case class XPowertype() extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "区分"
  def extjsName = "string"
}

case class XPowertypeReference() extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "区分"
  def extjsName = "string"
}

case class XStateMachine() extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "ワークフロー"
  def extjsName = "string"
}

case class XStateMachineReference() extends DataType {
  def validate(s: String): ValidationResult = Valid
  def label = "ワークフロー"
  def extjsName = "string"
}
