package org.goldenport.record.util

import java.sql.Time
import java.util.TimeZone
import java.text.SimpleDateFormat
import org.joda.time.format.ISODateTimeFormat

/*
 * @since   Jul. 27, 2014
 * @version Jul. 27, 2014
 * @author  ASAMI, Tomoharu
 */
object TimeUtils {
  val gmt = TimeZone.getTimeZone("GMT")
  val formatter = ISODateTimeFormat.timeNoMillis()
  val isoParser = ISODateTimeFormat.timeParser()

  /**
   * GMT
   */
  def parse(s: String): Time = {
    val lt = isoParser.parseLocalTime(s)
    new Time(lt.toDateTimeToday.getMillis)
  }

  /**
   * GMT
   */
  def toIsoTimeString(t: Time): String = {
    val format = new SimpleDateFormat("HH:mm:ss")
    format.setTimeZone(gmt)
    format.format(t)
  }
}
