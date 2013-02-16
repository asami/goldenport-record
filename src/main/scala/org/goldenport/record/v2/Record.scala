package org.goldenport.record.v2

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jun.  9, 2010
 *  version Jul.  3, 2011
 *  version Nov. 29, 2011
 *  version Feb. 16, 2012
 *  version Jul. 28, 2012
 * @version Feb. 16, 2013
 * @author  ASAMI, Tomoharu
 */
case class RecordSet(records: Seq[Record]) {
}

case class Record(fields: List[Field]) {
  def find(key: Symbol): Option[List[Any]] = {
    fields.find(_.isMatch(key)).map(_.values)
  }

  def findOne(key: Symbol): Option[Any] = {
    find(key).map(_(0))
  }

  def find(key: String): Option[List[Any]] = {
    find(Symbol(key))
  }

  def findOne(key: String): Option[Any] = {
    find(Symbol(key))
  }

  def asString(key: Symbol): String = {
    findOne(key).get.toString
  }

  def asInt(key: Symbol): Int = {
    findOne(key).get.toString.toInt // XXX
  }

  def asLong(key: Symbol): Long = {
    findOne(key).get.toString.toLong // XXX
  }

  def asString(key: String): String = {
    findOne(key).get.toString
  }

  def asInt(key: String): Int = {
    findOne(key).get.toString.toInt // XXX
  }

  def asLong(key: String): Long = {
    findOne(key).get.toString.toLong // XXX
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
}

object Field {
  def create(data: (String, Any)): Field = {
    data._2 match {
      case xs: Seq[_] => Field(Symbol(data._1), xs.toList)
      case x => Field(Symbol(data._1), List(x))
    }
  }
}
