package org.goldenport.record.v3

import java.sql.Timestamp
import java.util.Locale
import org.joda.time.DateTime
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.{Column, Field => Field2}
import org.goldenport.record.util.AnyUtils

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
 *  version Aug. 31, 2018
 *  version Sep. 17, 2018
 *  version Oct. 30, 2018
 *  version Dec. 29, 2018
 *  version Jan.  7, 2019
 *  version Mar. 23, 2019
 *  version May.  9, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
case class Field(
  key: Symbol,
  value: FieldValue,
  meta: Field.MetaData = Field.MetaData.empty
) {
  def name: String = key.name
  def asString: String = value.asString
  def asStringList: List[String] = RAISE.unsupportedOperationFault
  def asInt: Int = value.asInt
  def asLong: Long = value.asLong
  def asTimestamp: Timestamp = value.asTimestamp
  def asRecord: Record = value.asRecord
  def asRecordList: List[Record] = value.asRecordList

  def withKey(name: String): Field = copy(key = Symbol(name))

  def withValue(p: FieldValue): Field = copy(value = p)

  def mapValue(p: FieldValue => FieldValue): Field = copy(value = p(value))

  def mapContent(p: Any => Any): Field = copy(value = value.mapContent(p))

  def keyValue: Option[(Symbol, Any)] = {
    val data = value.getValue
    data.map(x => key -> x)
  }

  def nameValue: Option[(String, Any)] = {
    val data = value.getValue
    data.map(x => key.name -> x)
  }

  def nameString: Option[(String, String)] = {
    val data = value.getValue
    data.map(x => key.name -> AnyUtils.toString(x))
  }

  def symbolAny: Option[(Symbol, Any)] = value.getValue.map(x => key -> x)

  def getJsonField: Option[(String, JsValue)] = value.getJson.map(x => name -> x)

  def isAttribute = value match {
    case EmptyValue => true
    case _: SingleValue => true
    case _: MultipleValue => false
  }

  def toField2: Field2 = value match {
    case EmptyValue => Field2(key, Nil)
    case SingleValue(v) => Field2(key, List(_to_field2_value(v)))
    case MultipleValue(vs) => Field2(key, List(vs.map(_to_field2_value)))
  }

  private def _to_field2_value(p: Any): Any = p match {
    case m: Record => m.toRecord2
    case ms: Seq[_] => ms.map(_to_field2_value)
    case ms: Array[_] => ms.map(_to_field2_value)
    case ms: Some[_] => ms.map(_to_field2_value)
    case m => m
  }

  def toLtsv: String = {
    val v = asString // TODO normalize (replace with space)
    key.name + ":" + v
  }

  def normalizeHttp: Field = copy(value = value.normalizeHttp)
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

    def apply(p: Column): MetaData = MetaData(Some(p))
  }

  def apply(kv: (String, FieldValue)): Field = apply(kv._1, kv._2)

  def apply(key: String, value: FieldValue): Field = Field(Symbol(key), value)

  def apply(c: Column, value: FieldValue): Field = Field(c.key, value, MetaData(c))

  def create(key: String, value: Any): Field = create(Symbol(key), value)

  def create(key: Symbol, value: Any): Field = {
    value match {
      case m: FieldValue => apply(key, m)
      case Some(x) => create(key, x)
      case None => Field(key, EmptyValue)
      case xs: Seq[_] => Field(key, MultipleValue(xs))
      case x => Field(key, SingleValue(x))
    }
  }

  def create(data: (Symbol, Any)): Field = {
    create(data._1, data._2)
  }

  def createData(data: (String, Any)): Field = {
    create(Symbol(data._1), data._2)
  }

  def create(key: String, p: JsValue): Field = Field(Symbol(key), FieldValue.create(p))

  def create(c: Column, p: Any): Field = {
    import org.goldenport.record.v2.{MOne, MZeroOne, MOneMore, MZeroMore, MRange, MRanges}
    val v = c.multiplicity match {
      case MOne => _single_value(c, p)
      case MZeroOne => _single_value(c, p)
      case MOneMore => _multiple_value(c, p)
      case MZeroMore => _multiple_value(c, p)
      case m: MRange => _multiple_value(c, p)
      case m: MRanges => _multiple_value(c, p)
    }
    Field(c, v)
  }

  private def _single_value(c: Column, p: Any) = SingleValue(c.datatype.toInstance(p))

  private def _multiple_value(c: Column, p: Any): MultipleValue = p match {
    case m: Array[_] => MultipleValue(m.map(c.datatype.toInstance(_)))
    case m: Iterable[_] => MultipleValue(m.toVector.map(c.datatype.toInstance(_)))
    case m: Iterator[_] => MultipleValue(m.toVector.map(c.datatype.toInstance(_)))
    case m => MultipleValue(Vector(c.datatype.toInstance(m)))
  }
}
