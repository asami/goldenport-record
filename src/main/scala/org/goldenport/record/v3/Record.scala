package org.goldenport.record.v3

import java.sql.Timestamp
import org.goldenport.record.util.StringUtils
import org.goldenport.record.util.JsonUtils
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
 * @version May. 29, 2015
 * @author  ASAMI, Tomoharu
 */
case class Record(
  fields: Seq[Field]
) {
  def isDefined(key: Symbol): Boolean = {
    fields.exists(_.key == key)
  }

  def nonDefinded(key: Symbol): Boolean = {
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

  /*
   * Mutation
   */
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

  def removeFields(keys: Seq[Symbol]) = {
    copy(fields = fields.filterNot(x => keys.contains(x.key)))
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
}

object Record {
  val empty = Record(Vector.empty)

  def data(xs: (Symbol, Any)*): Record = {
    Record(xs.map(Field.create).toList)
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
