package org.goldenport.record.v3

import scalaz._, Scalaz._
import java.sql.Timestamp
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.json.JsonUtils
import org.goldenport.record.util.AnyUtils
import org.goldenport.record.v2.{Record => Record2, RecordRecord}

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
 *  version Oct. 30, 2018
 *  version Dec. 28, 2018
 * @version Jan. 21, 2019
 * @author  ASAMI, Tomoharu
 */
sealed abstract class FieldValue {
  protected lazy val as_string = getValue.map(AnyUtils.toString).getOrElse("")

  def getValue: Option[Any]
  def getList: Option[List[Any]]
  def getVector: Option[Vector[Any]]
  def takeList: List[Any]
  def takeVector: Vector[Any]
  def asString: String = as_string
  def asInt: Int = getValue.map(AnyUtils.toInt).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asLong: Long = getValue.map(AnyUtils.toInt).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asTimestamp: Timestamp = getValue.map(AnyUtils.toTimestamp).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asRecord: Record
  def asRecordList: List[Record]
  def asRecordVector: Vector[Record]
  def asTable: Table
  def getJson: Option[JsValue]
  def normalizeHttp: FieldValue
  def +(p: FieldValue): FieldValue
  def toMulti: MultipleValue
}

case class SingleValue(value: Any) extends FieldValue {
  def getValue = Some(value)
  def getList = Some(List(value))
  def getVector = Some(Vector(value))
  def takeList = List(value)
  def takeVector = Vector(value)
  // override def asTimestamp = value match {
  //   case x: Timestamp => x
  //   case l: Long => new Timestamp(l)
  //   case s: String => TimestampUtils.parse(s)
  // }
  def asRecord = value match {
    case m: IRecord => m.toRecord
    case m => RAISE.noReachDefect
  }
  def asRecordList = value match {
    case m: IRecord => List(m.toRecord)
    case m: ITable => m.toRecordList
    case m: Seq[_] => m.toList.map {
      case mm: IRecord => mm.toRecord
      case mm: Record2 => RecordRecord.toRecord3(mm)
      case mm => RAISE.noReachDefect
    }
    case m: Record2 => List(RecordRecord.toRecord3(m))
    case m => RAISE.noReachDefect
  }
  def asRecordVector = value match {
    case m: IRecord => Vector(m.toRecord)
    case m: ITable => m.toRecordVector
    case m: Seq[_] => m.toVector.map {
      case mm: IRecord => mm.toRecord
      case mm: Record2 => RecordRecord.toRecord3(mm)
      case mm => RAISE.noReachDefect
    }
    case m: Record2 => Vector(RecordRecord.toRecord3(m))
    case m => RAISE.noReachDefect
  }
  def asTable = value match {
    case m: ITable => m.toTable
    case m => RAISE.noReachDefect
  }
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
  def +(p: FieldValue): FieldValue = MultipleValue(value +: p.takeVector)
  def toMulti: MultipleValue = MultipleValue(Vector(value))
}

case class MultipleValue(values: Seq[Any]) extends FieldValue {
  protected lazy val as_string_sequence = values.map(AnyUtils.toString).mkString(",")

  def getValue = Some(values)
  def getList = Some(takeList)
  def getVector = Some(takeVector)
  def takeList = values.toList
  def takeVector = values.toVector
  override def asString: String = as_string_sequence
  def asRecord = RAISE.notImplementedYetDefect
  def asRecordList = values.toList.map {
    case m: IRecord => m.toRecord
    case m: Record2 => RecordRecord.toRecord3(m)
    case m => RAISE.noReachDefect
  }
  def asRecordVector = values.toVector.map {
    case m: IRecord => m.toRecord
    case m: Record2 => RecordRecord.toRecord3(m)
    case m => RAISE.noReachDefect
  }
  def asTable: Table = RAISE.notImplementedYetDefect
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
  def +(p: FieldValue): FieldValue = MultipleValue(values ++ p.takeVector)
  def toMulti = this
}

case object EmptyValue extends FieldValue {
  def getValue = None
  def getList = None
  def getVector = None
  def takeList = Nil
  def takeVector = Vector.empty
  def asRecord: Record = Record.empty
  def asRecordList: List[Record] = Nil
  def asRecordVector: Vector[Record] = Vector.empty
  def asTable: Table = Table.empty
  def getJson: Option[JsValue] = None
  def normalizeHttp: FieldValue = this
  def +(p: FieldValue): FieldValue = p
  def toMulti = MultipleValue(Vector.empty)
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

  implicit object FieldValueMonoid extends Monoid[FieldValue] {
    def zero = EmptyValue
    def append(lhs: FieldValue, rhs: => FieldValue) = lhs + rhs
  }
}
