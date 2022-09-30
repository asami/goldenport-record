package org.goldenport.record.util

import java.util.Date
import java.sql.Timestamp
import java.net.{URL, URI}
import java.io.File
import org.joda.time.LocalTime
import com.asamioffice.goldenport.io.UURL
import org.goldenport.record.v2.Record
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.extension.Showable
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
 *  version Jul. 18, 2018
 *  version Nov. 27, 2019
 *  version Nov.  5, 2021
 *  version Feb. 23, 2022
 * @version Sep. 27, 2022
 * @author  ASAMI, Tomoharu
 */
object AnyUtils {
  def toString(x: Any): String = {
    x match {
      // case m: String => m
      // case v: Timestamp => DateTimeUtils.toIsoDateTimeStringJst(v)
      // case v: Symbol => v.name
      // case m: NodeSeq => m.toString
      // case m: Seq[_] => m.map(toString(_)).mkString(",")
      // case m: Array[_] => m.map(toString(_)).mkString(",")
      case m: Record => RecordUtils.toJsonString(m)
      case _ => LibAnyUtils.toString(x)
    }
  }
  def toPrint(x: Any): String = x match {
    case m: Showable => m.print
    case m => toString(m)
  }
//  def toPrint(x: Any, width: Int): String = LibAnyUtils.toPrint(x, width)
  def toDisplay(x: Any): String = x match {
    case m: Showable => m.display
    case m => toString(m)
  }
  def toShow(x: Any): String = x match {
    case m: Showable => m.show
    case m => toString(m)
  }
  def toEmbed(x: Any): String = x match {
    case m: Showable => m.embed
    case m => toString(m)
  }
  def toEmbed(x: Any, width: Int): String = LibAnyUtils.toEmbed(x, width)
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
  def toByte(x: Any): Byte = LibAnyUtils.toByte(x)
  def toShort(x: Any): Short = LibAnyUtils.toShort(x)
  def toInt(x: Any): Int = LibAnyUtils.toInt(x)
  def toLong(x: Any): Long = LibAnyUtils.toLong(x)
  def toFloat(x: Any): Float = LibAnyUtils.toFloat(x)
  def toDouble(x: Any): Double = LibAnyUtils.toDouble(x)
  def toBigInt(x: Any): BigInt = LibAnyUtils.toBigInt(x)
  def toBigDecimal(x: Any): BigDecimal = LibAnyUtils.toBigDecimal(x)
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

  def getByte(p: Any): Option[Byte] = LibAnyUtils.getByte(p)
  def getShort(p: Any): Option[Short] = LibAnyUtils.getShort(p)
  def getInt(p: Any): Option[Int] = LibAnyUtils.getInt(p)
  def getLong(p: Any): Option[Long] = LibAnyUtils.getLong(p)
  def getFloat(p: Any): Option[Float] = LibAnyUtils.getFloat(p)
  def getDouble(p: Any): Option[Double] = LibAnyUtils.getDouble(p)
  def getBigInt(p: Any): Option[BigInt] = LibAnyUtils.getBigInt(p)
  def getBigDecimal(p: Any): Option[BigDecimal] = LibAnyUtils.getBigDecimal(p)
}
