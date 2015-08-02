package org.goldenport.record.v3

import java.sql.Timestamp
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
 * @version Jul. 14, 2015
 * @author  ASAMI, Tomoharu
 */
sealed abstract class FieldValue {
  def asString: String
  def asInt: Int = asString.toInt
  def asLong: Long = asString.toLong
  def asTimestamp: Timestamp = sys.error("???")
  def asDateTime: DateTime = sys.error("???")
  def getValue: Option[Any]

  def string: String = asString
  def int: Int = asInt
  def long: Long = asLong
  def timestamp: Timestamp = asTimestamp
  def datetime: DateTime = asDateTime
}

case class SingleValue(value: Any) extends FieldValue {
  def asString: String = value.toString
  override def asTimestamp = value match {
    case x: Timestamp => x
    case l: Long => new Timestamp(l)
    case s: String => TimestampUtils.parse(s)
  }
  def getValue = Some(value)
}

case class MultipleValue(values: Seq[Any]) extends FieldValue {
  def asString: String = values.mkString(",")
  def getValue = Some(values)
}

case object EmptyValue extends FieldValue {
  def asString: String = ""
  def getValue = None
}

object FieldValue {
  def create(v: Any): FieldValue = {
    SingleValue(v)
  }
}
