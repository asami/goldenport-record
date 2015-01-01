package org.goldenport.record.util

import org.joda.time._
import org.joda.time.format.ISODateTimeFormat

/*
 * @since   Jul. 25, 2014
 * @version Jan.  1, 2015
 * @author  ASAMI, Tomoharu
 */
object DateTimeUtils {
  val jodagmt = DateTimeZone.forID("GMT")
  val jodajst = DateTimeZone.forID("Asia/Tokyo")
  val isoFormatter = ISODateTimeFormat.dateTimeNoMillis()
  val isoUtcFormatter = isoFormatter.withZoneUTC
  val isoJstFormatter = isoFormatter.withZone(jodajst)

  def toIsoDateTimeString(dt: DateTime, tz: DateTimeZone): String = {
    toIsoDateTimeString(dt.getMillis, tz)
  }

  def toIsoDateTimeString(dt: java.sql.Timestamp, tz: DateTimeZone): String = {
    toIsoDateTimeString(dt.getTime, tz)
  }

  def toIsoDateTimeString(dt: Long, tz: DateTimeZone): String = {
    val fmt = if (tz == jodajst)
      isoJstFormatter
    else if (tz == jodagmt)
      isoUtcFormatter
    else
      isoFormatter.withZone(tz)
    fmt.print(dt)
  }

  def toIsoDateTimeStringJst(dt: DateTime): String = {
    toIsoDateTimeString(dt, jodajst)
  }

  def toIsoDateTimeStringJst(dt: java.sql.Timestamp): String = {
    toIsoDateTimeString(dt, jodajst)
  }

  def toIsoDateTimeStringJst(dt: Long): String = {
    toIsoDateTimeString(dt, jodajst)
  }
}
