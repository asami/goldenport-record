package org.goldenport.record.util

import scala.util.Try
import java.util.{Date, Locale}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time.format.ISODateTimeFormat

/*
 * @since   Jun. 10, 2014
 * @version Jul. 27, 2014
 * @author  ASAMI, Tomoharu
 */
object TimestampUtils {
  private val _iso_datetime_parser = ISODateTimeFormat.dateTimeParser
  private val _rss_datatime_format = {
    val sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH)
//    sdf.setTimeZone(gmt)
    sdf
  }

  def parse(s: String): Timestamp = {
    parseIsoOption(s) orElse parseRssOption(s) getOrElse {
      throw new IllegalArgumentException(s"Illegal datetime '$s'")
    }
  }

  def parseIsoOption(s: String): Option[Timestamp] = {
    Try(parseIso(s)).toOption
  }

  def parseIso(s: String): Timestamp = {
    val a = _iso_datetime_parser.parseMillis(s)
    new Timestamp(a)
  }

  def parseRssOption(s: String): Option[Timestamp] = {
    Try(parseRss(s)).toOption
  }

  def parseRss(s: String): Timestamp = {
    synchronized {
      val a = _rss_datatime_format.parse(s)
      new Timestamp(a.getTime)
    }
  }

  def toIsoDateTimeString(ts: Timestamp): String = {
    DateTimeUtils.toIsoDateTimeString(ts, DateTimeUtils.jodagmt)
  }
}
