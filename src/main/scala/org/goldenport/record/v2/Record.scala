package org.goldenport.record.v2

import org.goldenport.Strings

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jun.  9, 2010
 *  version Jul.  3, 2011
 *  version Nov. 29, 2011
 *  version Feb. 16, 2012
 *  version Jul. 28, 2012
 *  version Feb. 20, 2013
 * @version Mar.  7, 2013
 * @author  ASAMI, Tomoharu
 */
case class RecordSet(records: Seq[Record]) {
  def map(f: Record => Record): RecordSet = {
    RecordSet(records.map(f))
  }
}

case class Record(
  fields: List[Field],
  principal: Option[Principal] = None,
  timestamp: Long = System.currentTimeMillis
) {
  def get(key: Symbol): Option[List[Any]] = {
    fields.find(_.isMatch(key)).map(_.values)
  }

  def getOne(key: Symbol): Option[Any] = {
    get(key).map(_(0))
/*
    println("getOne = " + get(key))
    println("getOne 2 = " + get(key).headOption)
    val a = get(key) match {
      case Some(x) => {
        println("getOne 3 = " + x)
        x.headOption
      }
      case None => None
    }
    println("getOne r = " + a)
    a
*/
  }

  def get(key: String): Option[List[Any]] = {
    get(Symbol(key))
  }

  def getOne(key: String): Option[Any] = {
    getOne(Symbol(key))
  }

  def asString(key: Symbol): String = {
    getOne(key).get.toString
  }

  def asInt(key: Symbol): Int = {
    getOne(key).get.toString.toInt // XXX
  }

  def asLong(key: Symbol): Long = {
    getOne(key).get.toString.toLong // XXX
  }

  def asString(key: String): String = {
    getOne(key).get.toString
  }

  def asInt(key: String): Int = {
    getOne(key).get.toString.toInt // XXX
  }

  def asLong(key: String): Long = {
    getOne(key).get.toString.toLong // XXX
  }

  def inputFiles: Seq[InputFile] = {
    fields.collect { case x: InputFile => x }
  }

  //
  def +::(f: (String, Any)): Record = {
    copy(Field.create(f) +: fields)
  }

  def ::+(f: (String, Any)): Record = {
    copy(fields :+ Field.create(f))
  }

  def ::++(f: Seq[(String, Any)]): Record = {
    copy(fields ++ f.map(Field.create))
  }

  def +:(f: (String, Seq[Any])): Record = {
    copy(Field.create(f) +: fields)
  }

  def :+(f: (String, Seq[Any])): Record = {
    copy(fields :+ Field.create(f))
  }
}

case class Field(key: Symbol, values: List[Any]) {
  def isMatch(k: Symbol): Boolean = {
    k == key ||
    {
      val i = key.name.lastIndexOf(".")
      if (i != -1) {
        val n = key.name.substring(i + 1)
        n == k.name
      } else false
    }
  }

  def isEmpty: Boolean = {
    values.isEmpty || (values(0) match {
      case null => true
      case x: String => Strings.blankp(x)
      case _ => false
    })
  }

  def update(v: Seq[Any]): Field = {
    Field(key, v.toList)
  }

  def mapDouble(f: Double => Double): Field = {
    try {
      values match {
        case Nil => this
        case v => {
          val a = v.map(x => f(x.toString.toDouble))
          Field(key, a)
        }
      }
    } catch {
      case e => this
    }
  }

  def mapDecimal(f: BigDecimal => BigDecimal): Field = {
    try {
      values match {
        case Nil => this
        case v => {
          val a = v.map(x => f(scala.math.BigDecimal(x.toString)))
          Field(key, a)
        }
      }
    } catch {
      case e => this
    }
  }

  def mapColumnDecimal(
    schema: Schema, p: Column => Boolean, f: BigDecimal => BigDecimal
  ): Field = {
    schema.columns.find(c => c.name == key && p(c)) match {
      case Some(c) => mapDecimal(f)
      case None => this
    }
  }
}

object RecordSet {
  def create(map: Seq[scala.collection.Map[String, Any]]): RecordSet = {
    RecordSet(map.map(Record.create))
  }
}

object Record {
  def create(map: scala.collection.Map[String, Any]): Record = {
    create(map.toList)
  }

  def create(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.create).toList)
  }

  def createSingle(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.createSingle).toList)
  }
}

object Field {
  def create(data: (String, Any)): Field = {
    data._2 match {
      case xs: Seq[_] => Field(Symbol(data._1), xs.toList)
      case x => Field(Symbol(data._1), List(x))
    }
  }

  def createSingle(data: (String, Any)): Field = {
    Field(Symbol(data._1), List(data._2))
  }

  def create(data: Seq[(String, Any)]): List[Field] = {
    data.map(create).toList
  }

  def createSingle(data: Seq[(String, Any)]): List[Field] = {
    data.map(createSingle).toList
  }
}
