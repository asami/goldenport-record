package org.goldenport.record.v3

import scalaz._, Scalaz._
import java.sql.Timestamp
import org.goldenport.record.util.StringUtils
import org.joda.time.DateTime
import org.goldenport.record.v2.Schema

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
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class Record(
  fields: Seq[Field],
  meta: Record.MetaData = Record.MetaData.empty
) extends IRecord
    with HttpPart with XmlPart with JsonPart with CsvPart with LtsvPart
    with CompatibilityPart {
  def isDefined(key: Symbol): Boolean = {
    fields.exists(_.key == key)
  }

  def nonDefined(key: Symbol): Boolean = {
    !isDefined(key)
  }

  def getField(key: Symbol): Option[Field] = {
    fields.find(_.key == key)
  }

  def getValue(key: Symbol): Option[FieldValue] = {
    getField(key).map(_.value)
  }

  def getString(key: Symbol): Option[String] = {
    getField(key).map(_.asString)
  }

  def getInt(key: Symbol): Option[Int] = {
    getField(key).map(_.asInt)
  }

  def getLong(key: Symbol): Option[Long] = {
    getField(key).map(_.asLong)
  }

  def getTimestamp(key: Symbol): Option[Timestamp] = {
    getField(key).map(_.asTimestamp)
  }

  def getDateTime(key: Symbol): Option[DateTime] = {
    getField(key).map(_.asDateTime)
  }

  def asString(key: Symbol): String = {
    getString(key) getOrElse {
      throw new IllegalArgumentException(s"Missing string '$key.name'")
    }
  }

  def asInt(key: Symbol): Int = {
    getInt(key) getOrElse {
      throw new IllegalArgumentException(s"Missing int '$key.name'")
    }
  }

  def asLong(key: Symbol): Long = {
    getLong(key) getOrElse {
      throw new IllegalArgumentException(s"Missing int '$key.name'")
    }
  }

  def asDateTime(key: Symbol): DateTime = {
    getDateTime(key) getOrElse {
      throw new IllegalArgumentException(s"Missing datetime '$key.name'")
    }
  }

  def getString(key: String): Option[String] = {
    getString(Symbol(key))
  }

  def keyStringValues: Seq[(String, Any)] = {
    fields.flatMap(_.keyStringValue)
  }

  /*
   * Mutation
   */
  def +(rhs: Record): Record = update(rhs)

  def update(rec: Record): Record =
    rec.fields.foldLeft(this)((z, x) => z.updateField(x.key, x.value))

  def update(kv: (Symbol, Any)*): Record = {
    kv.foldLeft(this)((z, x) => z.update(x._1, x._2))
  }

  def update(key: Symbol, value: Any): Record = {
    val (prefix, suffix) = fields.span(_.key != key) // XXX isMatch?
    val r = suffix match {
      case Nil => prefix :+ Field.create(key, value)
      case x :: xs => prefix ++ (Field.create(key, value) :: xs)
    }
    copy(fields = r)
  }

  def updateField(key: Symbol, value: FieldValue): Record = {
    val (prefix, suffix) = fields.span(_.key != key) // XXX isMatch?
    val r = suffix match {
      case Nil => prefix :+ Field.create(key, value)
      case x :: xs => prefix ++ (Field.create(key, value) :: xs)
    }
    copy(fields = r)
  }
}

object Record {
  val empty = Record(Vector.empty)

  case class MetaData(
    schema: Option[Schema]
  ) {
    def columns: Option[List[Field.MetaData]] = schema.map(_.columns.map(x => Field.MetaData(Some(x))).toList)
  }
  object MetaData {
    val empty = MetaData(None)
  }

  implicit object RecordMonoid extends Monoid[Record] {
    def zero = Record.empty
    def append(lhs: Record, rhs: => Record) = lhs + rhs
  }

  def fromDataSeq(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.fromData).toList)
  }

  def fromLtsv(ltsv: String): Record = {
    Record.fromDataSeq(StringUtils.ltsv2seq(ltsv))
  }

  def fromLtsv(ltsv: Option[String]): Record = {
    ltsv.map(fromLtsv).getOrElse(Record.empty)
  }
}
