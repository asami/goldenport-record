package org.goldenport.record.v3

import scalaz._, Scalaz._
import java.sql.Timestamp
import play.api.libs.json._
import org.goldenport.record.util.StringUtils
import org.joda.time.DateTime
import org.goldenport.record.v2.{Record => Record2, Schema}
import org.goldenport.record.v2.util.RecordUtils

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
 * @version Sep. 17, 2018
 * @author  ASAMI, Tomoharu
 */
case class Record(
  fields: Seq[Field],
  meta: Record.MetaData = Record.MetaData.empty
) extends IRecord
    with XmlPart with JsonPart with CsvPart with LtsvPart
    with HttpPart with SqlPart
    with CompatibilityPart {
  def toRecord = this
  def toRecord2: Record2 = Record2.createApp(nameValues)

  def isEmpty: Boolean = fields.isEmpty
  def isDefined(key: Symbol): Boolean = fields.exists(_.key == key)
  def isDefined(key: String): Boolean = isDefined(Symbol(key))

  def nonDefined(key: Symbol): Boolean = !isDefined(key)
  def nonDefined(key: String): Boolean = !isDefined(key)

  def get(key: String): Option[Any] = getField(key).flatMap(_.value.getValue)

  def get(key: Symbol): Option[Any] = getField(key).flatMap(_.value.getValue)

  def getField(key: Symbol): Option[Field] = {
    fields.find(_.key == key)
  }

  def getField(key: String): Option[Field] = {
    val s = Symbol(key)
    fields.find(_.key == s)
  }

  def getValue(key: Symbol): Option[FieldValue] = {
    getField(key).map(_.value)
  }

  def getString(key: Symbol): Option[String] = {
    getField(key).map(_.asString)
  }

  def getString(key: String): Option[String] = {
    getString(Symbol(key))
  }

  def asString(key: Symbol): String = {
    getString(key) getOrElse {
      throw new IllegalArgumentException(s"Missing string '$key.name'")
    }
  }

  def takeStringList(key: Symbol): List[String] = ???

  def takeStringList(key: String): List[String] = ???

  def getInt(key: Symbol): Option[Int] = getField(key).map(_.asInt)
  def getInt(key: String): Option[Int] = getField(key).map(_.asInt)

  def asInt(key: Symbol): Int = getInt(key) getOrElse {
    throw new IllegalArgumentException(s"Missing int '$key.name'")
  }

  def getLong(key: Symbol): Option[Long] = {
    getField(key).map(_.asLong)
  }

  def asLong(key: Symbol): Long = {
    getLong(key) getOrElse {
      throw new IllegalArgumentException(s"Missing int '$key.name'")
    }
  }

  def getTimestamp(key: Symbol): Option[Timestamp] = {
    getField(key).map(_.asTimestamp)
  }

  def getDateTime(key: Symbol): Option[DateTime] = {
    getField(key).map(_.asDateTime)
  }

  def asDateTime(key: Symbol): DateTime = {
    getDateTime(key) getOrElse {
      throw new IllegalArgumentException(s"Missing datetime '$key.name'")
    }
  }

  def getRecord(key: Symbol): Option[Record] = getField(key).map(_.asRecord)
  def getRecord(key: String): Option[Record] = getField(key).map(_.asRecord)

  def takeRecordList(key: Symbol): List[Record] = getField(key).map(_.asRecordList).getOrElse(Nil)
  def takeRecordList(key: String): List[Record] = getField(key).map(_.asRecordList).getOrElse(Nil)

  def keyValues: Seq[(Symbol, Any)] = fields.flatMap(_.keyValue)
  def nameValues: Seq[(String, Any)] = fields.flatMap(_.nameValue)
  def nameStrings: Seq[(String, String)] = fields.flatMap(_.nameString)

  /*
   * Mutation
   */
  def +(rhs: Record): Record = update(rhs)
  def +(rhs: IRecord): IRecord = update(rhs.toRecord)

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

  def data(data: (String, Any)*): Record = createDataSeq(data)

  def dataOption(data: (String, Option[Any])*): Record = {
    val xs = data.collect {
      case (k, Some(v)) => k -> v
    }
    create(xs)
  }

  def create(map: scala.collection.Map[String, Any]): Record = create(map.toVector)

  def create(data: Seq[(String, Any)]): Record = createDataSeq(data)

  def createDataSeq(data: Seq[(String, Any)]): Record =
    Record(data.map(Field.createData).toList)

  def createHttp(data: Map[String, List[String]]): Record =
    create(data).http.request.normalize

  def createHttp(data: Seq[(String, List[String])]): Record =
    create(data).http.request.normalize

  def fromLtsv(ltsv: String): Record = {
    Record.createDataSeq(StringUtils.ltsv2seq(ltsv))
  }

  def fromLtsv(ltsv: Option[String]): Record = {
    ltsv.map(fromLtsv).getOrElse(Record.empty)
  }

  def fromJson(p: String): Record = create(Json.parse(p))

  def create(p: JsValue): Record = p match {
    case null => Record.empty
    case JsNull => Record.empty
    case m: JsUndefined => Record.empty
    case m: JsObject => create(m)
    case _: JsArray => throw new IllegalArgumentException(s"Array: $p")
    case _ => throw new IllegalArgumentException(s"Not object: $p")
  }

  def create(p: JsObject): Record = {
    val xs = p.fields.map {
      case (k, v) => Field.create(k, v)
    }
    Record(xs)
  }

  def buildSchema(p: IRecord): Schema = RecordUtils.buildSchema(p.toRecord.toRecord2)
  def buildSchema(ps: Seq[IRecord]): Schema = {
    val xs = ps.map(_.toRecord.toRecord2)
    RecordUtils.buildSchema(xs)
  }

  object json {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._
    import org.goldenport.json.JsonUtils.Implicits._
    import org.goldenport.record.v2.util.RecordUtils

    implicit object RecordFormat extends Format[Record] {
      def reads(json: JsValue): JsResult[Record] = json match {
        case m: JsObject => JsSuccess(create(m))
        case _ => JsError(s"Invalid Record($json)")
      }
      def writes(o: Record): JsValue = o.toJson
    }
  }
}
