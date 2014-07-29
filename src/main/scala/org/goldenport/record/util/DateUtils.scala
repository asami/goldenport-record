package org.goldenport.record.util

import scala.util.Try
import java.util.{Date, Locale, TimeZone}
import java.sql.Timestamp
import java.text.SimpleDateFormat
import org.joda.time.format.ISODateTimeFormat

/*
 * @since   Jun. 17, 2014
 * @version Jul. 27, 2014
 * @author  ASAMI, Tomoharu
 */
object DateUtils {
  private val _gmt = TimeZone.getTimeZone("GMT")
  private val _df = new SimpleDateFormat("yyyy-MM-dd")
  _df.setTimeZone(_gmt) // java.util.Date holds date information as GMT in DB
  val isoFormatter = ISODateTimeFormat.date().withZoneUTC

  def parse(s: String): Date = {
    synchronized {
      _df.parse(s)
    }
  }

  /**
   * GMT
   */
  def toIsoDateString(dt: java.sql.Date): String = {
    isoFormatter.print(dt.getTime)
  }

  /**
   * GMT
   */
  def toIsoDateString(dt: java.util.Date): String = {
    isoFormatter.print(dt.getTime)
  }
}
