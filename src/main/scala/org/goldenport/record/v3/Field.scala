package org.goldenport.record.v3

import java.sql.Timestamp
import org.joda.time.DateTime
import org.goldenport.record.v2.{ValidationResult, Valid, Warning, Invalid}

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
 *  version Jun. 21, 2015
 * @version Jul. 14, 2015
 * @author  ASAMI, Tomoharu
 */
case class Field(
  key: Symbol,
  value: FieldValue,
  validation: ValidationResult = Valid
) {
  def isValid: Boolean = validation == Valid
  def isValidOrWarning: Boolean = validation match {
    case Valid => true
    case _: Warning => true
    case _ => false
  }
  def isInvalid: Boolean = validation.isInstanceOf[Invalid]

  def asString: String = value.asString
  def asInt: Int = value.asInt
  def asLong: Long = value.asLong
  def asTimestamp: Timestamp = value.asTimestamp
  def asDateTime: DateTime = value.asDateTime

  def string: String = value.string
  def int: Int = value.int
  def long: Long = value.long
  def timestamp: Timestamp = value.timestamp
  def datetime: DateTime = value.datetime

  def keyValue: Option[(Symbol, Any)] = {
    val data = value.getValue
    data.map(x => key -> x)
  }

  def keyStringValue: Option[(String, Any)] = {
    keyValue map {
      case (k, v) => k.name -> v
    }
  }

  def toLtsv: String = {
    val v = asString // TODO normalize (replace with space)
    key.name + ":" + v
  }
}

object Field {
  def create(key: Symbol, value: Any): Field = {
    value match {
      case Some(x) => create(key, x)
      case None => Field(key, EmptyValue)
      case xs: Seq[_] => Field(key, MultipleValue(xs))
      case x => Field(key, SingleValue(x))
    }
  }

  def create(data: (Symbol, Any)): Field = {
    create(data._1, data._2)
  }

  def fromData(data: (String, Any)): Field = {
    create(Symbol(data._1), data._2)
  }
}
