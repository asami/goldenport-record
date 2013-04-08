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
 *  version Mar. 28, 2013
 * @version Apr.  8, 2013
 * @author  ASAMI, Tomoharu
 */
case class RecordSet(records: Seq[Record],
  total: Option[Long] = None,
  opaque: AnyRef = null
) {
  def length(): Long = records.length
  def isEmpty() = records.isEmpty
  def nonEmpty() = records.nonEmpty

  def map(f: Record => Record): RecordSet = {
    RecordSet(records.map(f))
  }

  def withOpaque(o: AnyRef): RecordSet = {
    copy(opaque = o)
  }
}

case class Record(
  fields: List[Field],
  principal: Option[Principal] = None,
  timestamp: Long = System.currentTimeMillis,
  inputFiles: Seq[InputFile] = Nil,
  opaque: AnyRef = null
) {
  def get(key: Symbol): Option[List[Any]] = {
    fields.find(_.isMatchKey(key)).map(_.values)
  }

  def getOne(key: Symbol): Option[Any] = {
    get(key) match {
      case None => None
      case Some(Nil) => None
      case Some(x :: _) => Some(x)
    }
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

  def getList(key: Symbol): List[Any] = {
    get(key) getOrElse Nil
  }

  def getRecords(key: Symbol): List[Record] = {
    get(key) match {
      case None => Nil
      case Some(xs) => xs.map(_.asInstanceOf[Record])
    }
  }

  def get(key: String): Option[List[Any]] = {
    get(Symbol(key))
  }

  def getOne(key: String): Option[Any] = {
    getOne(Symbol(key))
  }

  def getList(key: String): List[Any] = {
    getList(Symbol(key))
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

  def length(): Int = fields.length
  def isEmpty() = fields.isEmpty
  def nonEmpty() = fields.nonEmpty

  def isDefined(key: Symbol): Boolean = fields.exists(_.isMatchKey(key))
  def isDefined(key: String): Boolean = isDefined(Symbol(key))

  /*
   * This record is subset of the target record.
   */
  def isMatch(rhs: Record): Boolean = {
    fields.forall(x =>
      rhs.get(x.key) match {
        case Some(s) => x.values == s
        case None => false
      }
    )
  }

  /**
   * String oriented compare.
   */
  def diffSubset(
    rhs: Record,
    comp: Map[Symbol, (Symbol, Record, Record) => Boolean] = Map.empty
  ): List[(Symbol, String, String)] = {
    fields.foldLeft(List[(Symbol, List[Any], List[Any])]())((a, x) =>
      rhs.get(x.key) match {
        case Some(s) if _compare(comp, x.key, rhs) => a
        case Some(s) => (x.key, x.values, s) :: a
        case None => (x.key, x.values, Nil) :: a
      }
    ).map(x => (x._1, x._2.toString, x._3.toString))
  }

  private def _compare(
    comp: Map[Symbol, (Symbol, Record, Record) => Boolean],
    key: Symbol,
    rhs: Record
  ): Boolean = {
    comp.get(key) match {
      case Some(f) => f(key, this, rhs)
      case None => _compare(this.getList(key), rhs.getList(key))
    }
  }

  private def _compare(lhs: List[Any], rhs: List[Any]): Boolean = {
    lhs.corresponds(rhs)((a, b) => a.toString == b.toString)
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

/*
  def ::++(f: Seq[(Symbol, Any)]): Record = {
    copy(fields ++ f.map(Field.create))
  }
*/

  def +:(f: (String, Seq[Any])): Record = {
    copy(Field.create(f) +: fields)
  }

  def :+(f: (String, Seq[Any])): Record = {
    copy(fields :+ Field.create(f))
  }

  def +(r: Record): Record = update(r)

  def update(r: Record): Record = {
    if (fields.isEmpty) r
    else if (r.isEmpty) this
    else {
      val keys: List[Symbol] = r.fields.map(_.key)
      val b = fields.filterNot(x => keys.contains(x.key))
      copy(fields = b ++ r.fields)
    }
  }

/*
  def update(fs: Seq[(Symbol, Any)]): Record = {
    val keys: Seq[Symbol] = fs.map(_._1)
    val a = fs.map(Field.create)
    val b = fields.filterNot(x => keys.contains(x.key))
    copy(fields = b ++ a)
  }
*/

  def update(fs: Seq[(String, Any)]): Record = {
    val keys: Seq[Symbol] = fs.map(x => Symbol(x._1))
    val a = fs.map(Field.create)
    val b = fields.filterNot(x => keys.contains(x.key))
    copy(fields = b ++ a)
  }

/*
  def complements(f: Seq[(Symbol, Any)]): Record = {
    val a = f.filterNot(x => exists(x._1))
    this ::++ a
  }
*/

  def complements(f: Seq[(String, Any)]): Record = {
    val a = f.filterNot(x => isDefined(x._1))
    this ::++ a
  }

  def normalizeImages(fieldname: String): Record = {
    val a = Symbol(fieldname)
    fields.find(_.key == a) match {
      case Some(f) => {
        val b = fields.filter(_.key != a)
        val c = for (x <- f.values) yield {
          val n = "a"
          val k = "a0"
          InputFile.createByUrlString(n, k, x.toString)
        }
        this.copy(b, inputFiles = c)
      }
      case None => this
    }
  }

  def withOpaque(o: AnyRef): Record = {
    copy(opaque = o)
  }
}

case class Field(key: Symbol, values: List[Any]) {
  def isMatchKey(k: Symbol): Boolean = {
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
  val empty = Record(Nil)

  def create(map: scala.collection.Map[String, Any]): Record = {
    create(map.toList)
  }

  def create(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.create).toList)
  }

  def createSingle(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.createSingle).toList)
  }

  def data(data: (String, Any)*): Record = {
    create(data)
  }
}

object Field {
/*
  def create(data: (Symbol, Any)): Field = {
    data._2 match {
      case Some(x) => Field(data._1, List(x))
      case None => Field(data._1, Nil)
      case xs: Seq[_] => Field(data._1, xs.toList)
      case x => Field(data._1, List(x))
    }
  }
*/

  def create(data: (String, Any)): Field = {
    data._2 match {
      case Some(x) => Field(Symbol(data._1), List(x))
      case None => Field(Symbol(data._1), Nil)
      case xs: Seq[_] => Field(Symbol(data._1), xs.toList)
      case x => Field(Symbol(data._1), List(x))
    }
  }

  def createSingle(data: (String, Any)): Field = {
    data._2 match {
      case Some(x) => Field(Symbol(data._1), List(x))
      case None => Field(Symbol(data._1), Nil)
      case xs: Seq[_] => Field(Symbol(data._1), xs.toList) // TODO modify
      case x => Field(Symbol(data._1), List(x))
    }
  }

  def create(data: Seq[(String, Any)]): List[Field] = {
    data.map(create).toList
  }

  def createSingle(data: Seq[(String, Any)]): List[Field] = {
    data.map(createSingle).toList
  }
}
