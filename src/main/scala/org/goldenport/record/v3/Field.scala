package org.goldenport.record.v3

import scala.util.control.NonFatal
import scala.collection.JavaConverters._
import java.sql.Timestamp
import java.util.Locale
import org.joda.time.DateTime
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.{Column => Column2, Field => Field2}
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
 *  version Jul.  7, 2019
 *  version Aug. 23, 2019
 *  version Sep. 30, 2019
 *  version Oct.  7, 2019
 *  version Nov. 29, 2019
 *  version Sep.  7, 2020
 *  version Mar. 25, 2021
 *  version Apr. 21, 2021
 *  version Oct. 24, 2021
 *  version Aug. 30, 2022
 *  version Oct. 30, 2022
 * @version Dec. 16, 2022
 * @author  ASAMI, Tomoharu
 */
case class Field(
  key: Symbol,
  value: FieldValue,
  meta: Field.MetaData = Field.MetaData.empty
) {
  override def toString() = try {
    s"Field($name: ${value})"
  } catch {
    case NonFatal(e) => s"Field#toString($name, ): $e"
  }

  def show: String = s"$name -> ${value.show}"

  def name: String = key.name
  def getValue: Option[Any] = value.getValue // Object or Seq
  def getRecord: Option[Record] = value.getRecord
  def asString: String = value.asString
  def asStringList: List[String] = RAISE.unsupportedOperationFault
  def asBoolean: Boolean = value.asBoolean
  def asInt: Int = value.asInt
  def asLong: Long = value.asLong
  def asFloat: Float = value.asFloat
  def asDouble: Double = value.asDouble
  def asBigDecimal: BigDecimal = value.asBigDecimal
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

  def isAttributeOption = meta.column.flatMap(_.xml.isAttribute)

  def isAttribute = isAttributeOption.getOrElse(
    value match {
      case EmptyValue => true
      case m: SingleValue => m.isSimpleData
      case _: MultipleValue => false
    }
  )

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
  def normalizeHttpPlain: Field = copy(value = value.normalizeHttpPlain)
}

object Field {
  case class MetaData(
    column: Option[Column2] = None
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
    val empty = MetaData()

    def apply(p: Column2): MetaData = MetaData(Some(p))
  }

  def apply(kv: (String, FieldValue)): Field = apply(kv._1, kv._2)

  def apply(key: String, value: FieldValue): Field = Field(Symbol(key), value)

  def apply(c: Column2, value: FieldValue): Field = Field(c.key, value, MetaData(c))

  def create(key: String, value: Any): Field = create(Symbol(key), value)

  def create(key: String, value: Any, column: Column2): Field = create(Symbol(key), value, column)

  def create(key: Symbol, value: Any): Field = {
    value match {
      case null => Field(key, EmptyValue)
      case m: FieldValue => apply(key, m)
      case Some(x) => create(key, x)
      case None => Field(key, EmptyValue)
      case xs: Seq[_] => Field(key, MultipleValue(xs.map(_normalize_value)))
      case xs: java.util.List[_] => Field(key, MultipleValue(xs.asScala.map(_normalize_value)))
      case x => Field(key, SingleValue(_normalize_value(x)))
    }
  }

  private def _multiple_value(ps: Seq[Any]): MultipleValue =
    MultipleValue(ps.map(_normalize_value))

  private def _normalize_value(p: Any): Any = p match {
    case m: Map[_, _] => Record(m.map {
      case (k, v) =>
        val key = k match {
          case mm: Symbol => mm
          case mm: String => Symbol(mm)
          case mm => Symbol(AnyUtils.toString(mm))
        }
        key -> _normalize_value(v)
    })
    case m: java.util.Map[_, _] => _normalize_value(m.asScala.toMap)
    case m => m
  }

  def create(key: Symbol, value: Any, column: Column2): Field = {
    create(key, value).copy(meta = MetaData(column))
  }

  def create(data: (Symbol, Any)): Field = {
    create(data._1, data._2)
  }

  def createData(data: (String, Any)): Field = {
    create(Symbol(data._1), data._2)
  }

  def create(key: String, p: JsValue): Field = Field(Symbol(key), FieldValue.create(p))

  def create(c: Column2, p: Any): Field = {
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

  private def _single_value(c: Column2, p: Any) = SingleValue(c.datatype.toInstance(p))

  private def _multiple_value(c: Column2, p: Any): MultipleValue = p match {
    case m: Array[_] => MultipleValue(m.map(c.datatype.toInstance(_)))
    case m: Iterable[_] => MultipleValue(m.toVector.map(c.datatype.toInstance(_)))
    case m: Iterator[_] => MultipleValue(m.toVector.map(c.datatype.toInstance(_)))
    case m => MultipleValue(Vector(c.datatype.toInstance(m)))
  }

  def createEmpty(name: String): Field = Field(name, EmptyValue)

  def createInt(key: String, value: Int): Field = Field(key, SingleValue(value))
}
