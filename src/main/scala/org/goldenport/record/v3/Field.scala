package org.goldenport.record.v3

import java.sql.Timestamp
import java.util.Locale
import org.joda.time.DateTime
import org.goldenport.record.v2.Column

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
 * @version Aug. 24, 2018
 * @author  ASAMI, Tomoharu
 */
case class Field(
  key: Symbol,
  value: FieldValue,
  meta: Field.MetaData = Field.MetaData.empty
) {
  def asString: String = value.asString
  def asInt: Int = value.asInt
  def asLong: Long = value.asLong
  def asTimestamp: Timestamp = value.asTimestamp
  def asDateTime: DateTime = value.asDateTime

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
  case class MetaData(
    column: Option[Column]
  ) {
    def name = column.map(_.name)
    def datatype = column.map(_.datatype)
    def multiplicity = column.map(_.multiplicity)
    def label(locale: Locale) = column.map(_.label(locale))
    def constraints = column.map(_.constraints)
    def displayFormat = column.flatMap(_.displayFormat) // CSS
    def layout = column.map(_.layout) // Bootstrap grid
    def form = column.map(_.form) // HTML FORM
  }
  object MetaData {
    val empty = MetaData(None)
  }

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
