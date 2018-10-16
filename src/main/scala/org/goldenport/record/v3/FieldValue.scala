package org.goldenport.record.v3

import java.sql.Timestamp
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.json.JsonUtils
import org.goldenport.record.util.TimestampUtils
import org.joda.time.DateTime

/*
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jun.  9, 2010
 *  version Jul.  3, 2011
 *  version Nov. 29, 2011
 *  version Feb. 16, 2012
 *  version Jul. 28, 2012
 *  version Feb. 20, 2013
 *  version Mar. 28, 2013
 *  version Apr. 26, 2013
 *  version May. 30, 2013
 *  version Jul. 22, 2013
 *  version Aug.  7, 2013
 *  version Sep.  6, 2013
 *  version Oct. 22, 2013
 *  version Jan. 30, 2014
 *  version Feb.  6, 2014
 *  version May. 15, 2014
 *  version Aug. 10, 2014
 *  version Sep. 28, 2014
 *  version Oct.  2, 2014
 *  version Nov. 29, 2014
 *  version Dec. 31, 2014
 *  version Jan.  2, 2015
 *  version Jul. 14, 2015
 *  version Sep.  4, 2018
 * @version Oct. 16, 2018
 * @author  ASAMI, Tomoharu
 */
sealed abstract class FieldValue {
  def asString: String
  def asInt: Int = asString.toInt
  def asLong: Long = asString.toLong
  def asTimestamp: Timestamp = RAISE.notImplementedYetDefect
  def asDateTime: DateTime = RAISE.notImplementedYetDefect
  def asRecord: Record
  def asRecordList: List[Record]
  def asTable: Table
  def getValue: Option[Any]

  def string: String = asString
  def int: Int = asInt
  def long: Long = asLong
  def timestamp: Timestamp = asTimestamp
  def datetime: DateTime = asDateTime
  def getJson: Option[JsValue]
  def normalizeHttp: FieldValue
}

case class SingleValue(value: Any) extends FieldValue {
  def asString: String = value.toString
  override def asTimestamp = value match {
    case x: Timestamp => x
    case l: Long => new Timestamp(l)
    case s: String => TimestampUtils.parse(s)
  }
  def asRecord = value match {
    case m: IRecord => m.toRecord
    case m => RAISE.noReachDefect
  }
  def asRecordList = value match {
    case m: IRecord => List(m.toRecord)
    case m: ITable => m.toRecordList
    case m: Seq[_] => m.toList.map {
      case mm: IRecord => mm.toRecord
      case mm => RAISE.noReachDefect
    }
    case m => RAISE.noReachDefect
  }
  def asTable = value match {
    case m: ITable => m.toTable
    case m => RAISE.noReachDefect
  }
  def getValue = Some(value)
  def getJson: Option[JsValue] = Some(JsonUtils.anyToJsValue(value))
  def normalizeHttp: FieldValue = value match {
    case None => EmptyValue
    case m: String =>
      if (Strings.blankp(m))
        EmptyValue
      else
        Strings.totokens(m, ",").filterNot(Strings.blankp) match {
          case Nil => EmptyValue
          case x :: Nil => SingleValue(x)
          case xs => MultipleValue(xs)
        }
    case _ => this
  }
}

case class MultipleValue(values: Seq[Any]) extends FieldValue {
  def asString: String = values.mkString(",")
  def asRecord = RAISE.notImplementedYetDefect
  def asRecordList = values.toList.map {
    case m: IRecord => m.toRecord
    case m => RAISE.noReachDefect
  }
  def asTable: Table = RAISE.notImplementedYetDefect
  def getValue = Some(values)
  def getJson: Option[JsValue] = Some(JsArray(values.map(JsonUtils.anyToJsValue)))
  def normalizeHttp: FieldValue = {
    val xs = values.flatMap {
      case None => None
      case m: String => 
        if (Strings.blankp(m))
          Nil
        else
          Strings.totokens(m, ",").filterNot(Strings.blankp)
    }
    xs match {
      case Nil => EmptyValue
      case x :: Nil => SingleValue(x)
      case xs => MultipleValue(xs)
    }
  }
}

case object EmptyValue extends FieldValue {
  def asString: String = ""
  def asRecord: Record = Record.empty
  def asRecordList: List[Record] = Nil
  def asTable: Table = Table.empty
  def getValue = None
  def getJson: Option[JsValue] = None
  def normalizeHttp: FieldValue = this
}

object FieldValue {
  def create(v: Any): FieldValue = SingleValue(v)

  def create(p: JsValue): FieldValue = p match {
    case null => EmptyValue
    case JsNull => EmptyValue
    case o: JsObject => SingleValue(Record.create(o))
    case a: JsArray => MultipleValue(a.value.map(FieldValue.create))
    case s: JsString => SingleValue(s.value)
    case b: JsBoolean => SingleValue(b.value)
    case n: JsNumber => SingleValue(n.value)
  }
}
