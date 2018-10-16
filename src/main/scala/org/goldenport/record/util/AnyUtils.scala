package org.goldenport.record.util

import java.util.Date
import java.sql.Timestamp
import java.net.{URL, URI}
import java.io.File
import org.joda.time.LocalTime
import play.api.libs.json.JsValue
import com.asamioffice.goldenport.io.UURL
import org.goldenport.record.v2.Record
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.util.{AnyUtils => LibAnyUtils}

/*
 * See com.everforth.lib.util.AnyUtils
 * 
 * @since   Jun. 10, 2014
 *  version Jan. 28, 2015
 *  version Oct. 24, 2015
 *  version Apr. 29, 2016
 *  version May. 25, 2017
 *  version Nov. 13, 2017
 *  version Jun. 27, 2018
 * @version Jul. 18, 2018
 * @author  ASAMI, Tomoharu
 */
object AnyUtils {
  def toString(x: Any): String = {
    x match {
      case v: Timestamp => DateTimeUtils.toIsoDateTimeStringJst(v)
      case v: Symbol => v.name
      case m: Seq[_] => m.map(toString(_)).mkString(",")
      case m: Array[_] => m.map(toString(_)).mkString(",")
      case m: Record => RecordUtils.toJsonString(m)
      case m: JsValue => m.toString
      case _ => x.toString
    }
  }
  def toBoolean(x: Any): Boolean = {
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
  def toByte(x: Any): Byte = {
    x match {
      case v: Byte => v
      case v => v.toString.toByte
    }
  }
  def toShort(x: Any): Short = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v => v.toString.toShort
    }
  }
  def toInt(x: Any): Int = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v => v.toString.toInt
    }
  }
  def toLong(x: Any): Long = {
    x match {
      case v: Byte => v
      case v: Short => v
      case v: Int => v
      case v: Long => v
      case v => v.toString.toLong
    }
  }
  def toFloat(x: Any): Float = {
    x match {
      case v: Float => v
      case v => v.toString.toFloat
    }
  }
  def toDouble(x: Any): Double = {
    x match {
      case v: Float => v
      case v: Double => v
      case v => v.toString.toDouble
    }
  }
  def toBigInt(x: Any): BigInt = {
    x match {
      case v: Byte => BigInt(v)
      case v: Short => BigInt(v)
      case v: Int => BigInt(v)
      case v: Long => BigInt(v)
      case v: BigInt => v
      case v => BigInt(v.toString)
    }
  }
  def toBigDecimal(x: Any): BigDecimal = {
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
  def toTimestamp(x: Any): Timestamp = {
    x match {
      case v: Timestamp => v
      case v: Long => new Timestamp(v)
      case s: String => TimestampUtils.parse(s)
    }
  }
  def toDate(x: Any): Date = {
    x match {
      case v: Date => v
      case v: Long => new Date(v)
      case s: String => DateUtils.parse(s)
    }
  }
  def toLocalTime(x: Any): LocalTime = org.goldenport.util.AnyUtils.toLocalTime(x)
  def toUrl(x: Any): URL = {
    x match {
      case m: URL => m
      case m: URI => m.toURL
      case m: File => m.toURI.toURL
      case s: String => UURL.getURLFromFileOrURLName(s)
    }
  }
  def toUri(x: Any): URI = LibAnyUtils.toUri(x)
  // V2
  def toRecord(x: Any): Record = x match {
    case m: Record => m
    case m: String => org.goldenport.record.v2.util.RecordUtils.fromJsonString(m)
    case m => throw new IllegalArgumentException(s"No record: $x")
  }
}
