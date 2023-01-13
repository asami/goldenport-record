package org.goldenport.record.v2.util

import java.sql.Timestamp
import java.util.Date
import java.text.ParseException
import org.joda.time.DateTime
import play.api.libs.json._
import org.goldenport.util._
import org.goldenport.record.v2._

/*
 * Migrated from com.everforth.everforth.util
 * 
 * @since   Feb. 20, 2013
 *  version Apr. 11, 2013
 *  version Sep.  9, 2013
 *  version Nov. 29, 2013
 *  version Feb.  6, 2014
 *  version Mar. 26, 2014
 *  version Apr. 19, 2014
 *  version Aug.  5, 2014
 *  version Nov. 15, 2016
 *  version Sep.  1, 2017
 *  version Jun. 17, 2022
 * @version Jan.  5, 2023
 * @author  ASAMI, Tomoharu
 */
object JsValueUtils {
  def toJsValue(records: RecordSet): JsArray = {
    toJsValue(records.records)
  }

  def toJsValue(records: Seq[Record]): JsArray = {
    JsArray(records.map(toJsValue))
  }

  def toJsValue(record: Record): JsObject = {
    val a = toJsValueSeq(record)
    JsObject(a)
  }

  def toJsValueSeq(record: Record): Seq[(String, JsValue)] = {
    for (f <- record.fields) yield {
      val v: JsValue = f.values match {
        case Nil => JsNull
        case x :: Nil => anyToJsValue(x)
        case xs => JsArray(xs.map(anyToJsValue))
      }
      (f.key.name, v)
    }
  }

  def anyToJsValue(v: Any): JsValue = {
    v match {
      case null => JsNull
      case None => JsNull
      case Some(x) => anyToJsValue(x)
      case x: String => JsString(x)
      case x: Boolean => JsBoolean(x)
      case x: Byte => JsNumber(x)
      case x: Short => JsNumber(x)
      case x: Int => JsNumber(x)
      case x: Long => JsNumber(x)
      case x: Float => JsNumber(x)
      case x: Double => JsNumber(x)
      case x: BigInt => JsNumber(BigDecimal(x))
      case x: BigDecimal => JsNumber(x)
      case x: java.sql.Timestamp => JsString(DateTimeUtils.toIsoDateTimeStringJst(x))
      case x: java.sql.Date => JsString(DateUtils.toIsoDateString(x))
      case x: java.util.Date => JsString(DateUtils.toIsoDateString(x))
//      case x: DateTime => JsString(DateTimeUtils.toIsoDateTimeStringJst(x))
      case x: java.math.BigInteger => JsNumber(BigDecimal(x.toString))
      case x: Record => toJsValue(x)
      case x: RecordSet => toJsValue(x)
      case xs: Seq[_] => JsArray(xs.map(anyToJsValue))
      case x: JsValue => x
      case x => JsString(AnyUtils.toPrint(x))
    }
  }

  /*
   * for iOS
   */
  def toJsArrayNoNull(records: RecordSet): JsArray = {
    toJsArrayNoNull(records.records)
  }

  def toJsArrayNoNull(records: Seq[Record]): JsArray = {
    JsArray(records.map(toJsObjectNoNull))
  }

  def toJsObjectNoNull(record: Record): JsObject = {
    val b = for (f <- record.fields) yield {
      val c: Option[JsValue] = f.values match {
        case Nil => None
        case x :: Nil => anyToJsValueNoNull(x)
        case xs => Some(JsArray(xs.flatMap(anyToJsValueNoNull)))
      }
      c.map(f.key.name -> _)
    }
    JsObject(b.flatten)
  }

  def toJsObjectNoNull(p: Map[_, _]): JsObject = {
    val a = p.asInstanceOf[Map[String, Any]]
    val b = for ((k, v) <- a.toVector) yield {
      val c: Option[JsValue] = anyToJsValueNoNull(v)
      c.map(k -> _)
    }
    JsObject(b.flatten)
  }

  def anyToJsValueNoNull(v: Any): Option[JsValue] = {
    v match {
      case null => None
      case None => None
      case Some(x) => anyToJsValueNoNull(x)
      case x: Record => Some(toJsObjectNoNull(x))
      case x: RecordSet => Some(toJsArrayNoNull(x))
      case xs: Seq[_] => Some(JsArray(xs.flatMap(anyToJsValueNoNull)))
      case x: Map[_, _] => Some(toJsObjectNoNull(x))
      case x => Some(anyToJsValue(x))
    }
  }

  def stripNull(o: JsObject): JsObject = {
    new JsObject(_strip_null_field(o.fields))
  }

  private def _strip_null_field(fields: Seq[(String, JsValue)]) = {
    fields.flatMap(kv => _strip_null_value(kv._2).map((kv._1, _)))
  }

  private def _strip_null_value(value: JsValue): Option[JsValue] = {
    value match {
      case null => None
      case JsNull => None
      case v: JsUndefined => None
      case o: JsObject => Some(stripNull(o))
      case a: JsArray => Some(JsArray(a.value.flatMap(_strip_null_value)))
      case v => Some(v)
    }
  }

  object Implicits {
    implicit object TimestampReads extends Reads[Timestamp] {
      def reads(json: JsValue): JsResult[Timestamp] = json match {
        case JsString(s) => {
          val dt = DateTimeUtils.parseDateTimeJst(s)
          JsSuccess(new Timestamp(dt.getMillis))
        }
        case _: JsUndefined => JsError()
        case x => throw new ParseException("String expected: " + x, 0)
      }
    }

    implicit object DateReads extends Reads[Date] {
      def reads(json: JsValue): JsResult[Date] = json match {
        case JsString(s) => {
          JsSuccess(DateUtils.parse(s)) // GMT
        }
        case _: JsUndefined => JsError()
        case x => throw new ParseException("String expected: " + x, 0)
      }
    }
  }
}
