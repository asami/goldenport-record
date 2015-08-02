package org.goldenport.record.v3

import java.sql.Timestamp
import org.goldenport.record.util.StringUtils
import org.goldenport.record.util.JsonUtils
import org.joda.time.DateTime
import org.goldenport.record.v2.{
  ValidationResult, Valid, Warning, Invalid,
  Schema
}

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
 *  version May. 29, 2015
 *  version Jun. 21, 2015
 *  version Jul. 31, 2015
 * @version Aug.  2, 2015
 * @author  ASAMI, Tomoharu
 */
case class Record(
  fields: IndexedSeq[Field],
  timestamp: Long = System.currentTimeMillis,
  validation: ValidationResult = Valid,
  source: IndexedSeq[Record] = IndexedSeq.empty,
  exception: Option[Throwable] = None
) {
  override def equals(o: Any): Boolean = {
    o match {
      case rec: Record if length == rec.length =>
        fields.forall(x => rec.getField(x.key) == Some(x))
      case _ => false
    }
  }

  def length(): Int = fields.length
  def isEmpty() = fields.isEmpty
  def nonEmpty() = fields.nonEmpty

  def isDefined(key: Symbol): Boolean = {
    fields.exists(_.key == key)
  }

  def nonDefinded(key: Symbol): Boolean = {
    !isDefined(key)
  }

  def isValid: Boolean = {
    validation == Valid && fields.forall(_.isValid)
  }

  def isValidOrWarning: Boolean = {
    (validation == Valid || validation.isInstanceOf[Warning]) && 
    fields.forall(_.isValidOrWarning)
  }

  def isInvalid: Boolean = {
    validation.isInstanceOf[Invalid] || fields.exists(_.isInvalid)
  }

  def isDeadLetter: Boolean = isInvalid || exception.isDefined

  def getField(key: Symbol): Option[Field] = {
    fields.find(_.key == key)
  }

  def getField(key: String): Option[Field] = {
    getField(Symbol(key))
  }

  def getValue(key: Symbol): Option[FieldValue] = {
    getField(key).map(_.value)
  }

  def getValue(key: String): Option[FieldValue] = {
    getValue(Symbol(key))
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

  def getInt(key: String): Option[Int] = {
    getInt(Symbol(key))
  }

  /*
   * Predicates
   */
  def intp(key: Symbol)(p: (Int) => Boolean): Boolean = {
    getInt(key).map(p).getOrElse(false)
  }

  def intp(key: String)(p: (Int) => Boolean): Boolean = {
    getInt(key).map(p).getOrElse(false)
  }

  def stringp(key: Symbol)(p: (String) => Boolean): Boolean = {
    getString(key).map(p).getOrElse(false)
  }

  def stringp(key: String)(p: (String) => Boolean): Boolean = {
    getString(key).map(p).getOrElse(false)
  }

  /*
   * Mutation
   */
  def update(kv: (Symbol, Any)*): Record = {
    kv.foldLeft(this)((z, x) => z.update(x._1, x._2))
  }

  def update(key: Symbol, value: Any): Record = {
    val (prefix, suffix) = fields.span(_.key != key) // XXX isMatch?
    val r = if (suffix.isEmpty) {
      prefix :+ Field.create(key, value)
    } else {
      prefix ++ (Field.create(key, value) +: suffix.tail)
    }
    copy(timestamp = System.currentTimeMillis, fields = r)
  }

  def updateValue(key: Symbol, value: FieldValue): Record = {
    val (prefix, suffix) = fields.span(_.key != key) // XXX isMatch?
    val r = if (suffix.isEmpty) {
      prefix :+ Field(key, value)
    } else {
      prefix ++ (Field(key, value) +: suffix.tail)
    }
    copy(timestamp = System.currentTimeMillis, fields = r)
  }

  def updateValue(
    key: Symbol,
    value: FieldValue,
    validation: ValidationResult
  ): Record = {
    val (prefix, suffix) = fields.span(_.key != key) // XXX isMatch?
    val r = if (suffix.isEmpty) {
      prefix :+ Field(key, value, validation)
    } else {
      prefix ++ (Field(key, value, validation) +: suffix.tail)
    }
    copy(timestamp = System.currentTimeMillis, fields = r)
  }

  def updateValueOne(key: Symbol, value: Any): Record = {
    updateValue(key, SingleValue(value))
  }

  def updateValueOne(
    key: Symbol,
    value: Any,
    validation: ValidationResult
  ): Record = {
    updateValue(key, SingleValue(value), validation)
  }

  def updateValueOne(key: String, value: Any): Record = {
    updateValueOne(Symbol(key), value)
  }

  def updateValueOne(
    key: String,
    value: Any,
    validation: ValidationResult
  ): Record = {
    updateValueOne(Symbol(key), value, validation)
  }

  def removeFields(keys: Seq[Symbol]) = {
    copy(timestamp = System.currentTimeMillis, fields = fields.filterNot(x => keys.contains(x.key)))
  }

  def removeFields(keys: Set[Symbol]) = {
    copy(timestamp = System.currentTimeMillis, fields = fields.filterNot(x => keys.contains(x.key)))
  }

  def withException(e: Throwable) = {
    copy(timestamp = System.currentTimeMillis, exception = Some(e))
  }

  def withException(e: Throwable, source: Record) = {
//    assert (source.isEmpty, "Source should not be overwrited.")
    copy(timestamp = System.currentTimeMillis, exception = Some(e), source = this.source :+ source)
  }

  def withValidation(v: ValidationResult) = {
    copy(timestamp = System.currentTimeMillis, validation = validation + v)
  }

  def withSource(s: Record) = {
    copy(timestamp = System.currentTimeMillis, source = Vector(s))
  }

  def addSource(s: Record) = {
    copy(timestamp = System.currentTimeMillis, source = source :+ s)
  }

  def addSourceAtMostOnce(s: Record) = {
    if (source.contains(s))
      this
    else
      copy(timestamp = System.currentTimeMillis, source = source :+ s)
  }

  /*
   * String Vector
   */
  def toStrings: Vector[String] = {
    fields.map(_.asString).toVector
  }

  def toStrings(schema: Schema): Vector[String] = {
    schema.columns.toVector.map(c => getString(c.name) getOrElse "")
  }

  /*
   * LTSV
   */
  def toLtsv: String = {
    fields.map(_.toLtsv).mkString("\t")
  }

  def toLtsvPart: String = {
    fields.map(x => "\t" + x.toLtsv).mkString
  }

  /*
   * Json
   */
  def toJsonString: String = {
    val buf = new StringBuilder
    buildJsonString(buf)
    buf.toString
  }

  def buildJsonString(buf: StringBuilder) {
    def buildfield(kv: (String, Any)) {
      buf.append("\"")
      buf.append(kv._1)
      buf.append("\":")
      JsonUtils.data2json(buf, kv._2)
    }

    buf.append("{")
    val fs = keyStringValues
    if (fs.nonEmpty) {
      buildfield(fs.head)
      for (f <- fs.tail) {
        buf.append(",")
        buildfield(f)
      }
    }
    buf.append("}")
  }

  def keyStringValues: Seq[(String, Any)] = {
    fields.flatMap(_.keyStringValue)
  }

  /*
   * Log
   */
  def toLog: String = {
    val buf = new StringBuilder
    def key(k: Symbol) {
      buf.append("\"")
      buf.append(k.name)
      buf.append("\":")
    }
    def field(k: Symbol, v: Any) {
      key(k)
      JsonUtils.data2json(buf, v)
    }
    buf.append("{")
    field('timestamp, timestamp)
    buf.append(",")
    key('data)
    buildJsonString(buf)
    buf.append(",")
    field('validation, validation)
    if (source.nonEmpty) {
      buf.append(",")
      key('source)
      buf.append("[")
      buf.append(source.map(_.toLog).mkString(","))
      buf.append("]")
    }
    for (e <- exception) {
      buf.append(",")
      field('exception, e)
    }
    buf.append("}")
    buf.toString
  }
}

object Record {
  val empty = Record(Vector.empty)

  def data(xs: (Symbol, Any)*): Record = {
    Record(xs.map(Field.create).toVector)
  }

  def fromDataSeq(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.fromData).toVector)
  }

  def fromLtsv(ltsv: String): Record = {
    Record.fromDataSeq(StringUtils.ltsv2seq(ltsv))
  }

  def fromLtsv(ltsv: Option[String]): Record = {
    ltsv.map(fromLtsv).getOrElse(Record.empty)
  }

  def fromException(e: Throwable): Record = {
    Record(Vector.empty, exception = Some(e))
  }
}
