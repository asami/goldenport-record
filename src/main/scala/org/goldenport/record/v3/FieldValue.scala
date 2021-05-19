package org.goldenport.record.v3

import scalaz._, Scalaz._
import scala.util.control.NonFatal
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
 *  version Jan. 21, 2019
 *  version May.  9, 2019
 *  version Jul. 31, 2019
 *  version Aug. 23, 2019
 *  version Oct.  7, 2019
 *  version Nov. 29, 2019
 *  version Jan. 11, 2020
 *  version Apr. 21, 2021
 * @version May. 20, 2021
 * @author  ASAMI, Tomoharu
 */
sealed abstract class FieldValue {
  override def toString() = try {
    s"${getClass.getSimpleName}(${as_string})"
  } catch {
    case NonFatal(e) => try {
      s"""${getClass.getSimpleName}#toString(${getValue.map(_.getClass.getSimpleName).getOrElse("#Empty")}): $e"""
    } catch {
      case NonFatal(ee) => s"""FieldValue#toString: $e"""
    }
  }

  protected lazy val as_string = getValue.map(AnyUtils.toString).getOrElse("")

  def getValue: Option[Any]
  def getList: Option[List[Any]]
  def getVector: Option[Vector[Any]]
  def asList: List[Any]
  def asListEager: List[Any] = asList.flatMap(x => Strings.totokens(AnyUtils.toString(x), ","))
  def asVector: Vector[Any]
  def asString: String = as_string
  def asBoolean: Boolean = getValue.map(AnyUtils.toBoolean).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asInt: Int = getValue.map(AnyUtils.toInt).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asLong: Long = getValue.map(AnyUtils.toLong).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asFloat: Float = getValue.map(AnyUtils.toFloat).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asDouble: Double = getValue.map(AnyUtils.toDouble).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asBigDecimal: BigDecimal = getValue.map(AnyUtils.toBigDecimal).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asTimestamp: Timestamp = getValue.map(AnyUtils.toTimestamp).getOrElse(RAISE.invalidArgumentFault("empty"))
  def asRecord: Record
  def asRecordList: List[Record]
  def asRecordVector: Vector[Record]
  def asTable: Table
  def getJson: Option[JsValue]
  def normalizeHttp: FieldValue
  def +(p: FieldValue): FieldValue
  def toMulti: MultipleValue
  def mapContent(p: Any => Any): FieldValue
}

case class SingleValue(value: Any) extends FieldValue {
  def getValue = Some(value)
  def getList = Some(List(value))
  def getVector = Some(Vector(value))
  def asList = List(value)
  def asVector = Vector(value)
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
  def +(p: FieldValue): FieldValue = MultipleValue(value +: p.asVector)
  def toMulti: MultipleValue = MultipleValue(Vector(value))
  def mapContent(p: Any => Any): FieldValue = copy(p(value))

  def isSimpleData: Boolean = !isComplexData
  def isComplexData: Boolean = value match {
    case _: IRecord => true
    case _: ITable => true
    case _: Seq[_] => true
    case _: Array[_] => true
    case _ => false
  }
}

case class MultipleValue(values: Seq[Any]) extends FieldValue {
  protected lazy val as_string_sequence = values.map(AnyUtils.toString).mkString(",")

  def getValue = Some(values)
  def getList = Some(asList)
  def getVector = Some(asVector)
  def asList = values.toList
  def asVector = values.toVector
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
  def +(p: FieldValue): FieldValue = MultipleValue(values ++ p.asVector)
  def toMulti = this
  def mapContent(p: Any => Any): FieldValue = copy(values = values.map(p))
}

case object EmptyValue extends FieldValue {
  def getValue = None
  def getList = None
  def getVector = None
  def asList = Nil
  def asVector = Vector.empty
  def asRecord: Record = Record.empty
  def asRecordList: List[Record] = Nil
  def asRecordVector: Vector[Record] = Vector.empty
  def asTable: Table = Table.empty
  def getJson: Option[JsValue] = None
  def normalizeHttp: FieldValue = this
  def +(p: FieldValue): FieldValue = p
  def toMulti = MultipleValue(Vector.empty)
  def mapContent(p: Any => Any): FieldValue = this
}

object FieldValue {
  def create(v: Any): FieldValue = SingleValue(v)

  def create(p: JsValue): FieldValue = p match {
    case null => EmptyValue
    case JsNull => EmptyValue
    case o: JsObject => SingleValue(Record.create(o))
    case a: JsArray => MultipleValue(a.value.map(_create))
    case s: JsString => SingleValue(s.value)
    case b: JsBoolean => SingleValue(b.value)
    case n: JsNumber => SingleValue(n.value)
  }

  private def _create(p: JsValue): Any = p match {
    case null => ""
    case JsNull => ""
    case o: JsObject => Record.create(o)
    case a: JsArray => a.value.map(_create)
    case s: JsString => s.value
    case b: JsBoolean => b.value
    case n: JsNumber => n.value
  }

  implicit object FieldValueMonoid extends Monoid[FieldValue] {
    def zero = EmptyValue
    def append(lhs: FieldValue, rhs: => FieldValue) = lhs + rhs
  }
}
