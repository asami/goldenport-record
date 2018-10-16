package org.goldenport.record.v3

import scalaz._, Scalaz._
import java.sql.Timestamp
import play.api.libs.json._
import org.joda.time.DateTime
import org.goldenport.record.util.{StringUtils, JsonUtils}
import org.goldenport.record.v2.{
  ValidationResult, Valid, Warning, Invalid,
  Schema
}
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
 *  version May. 29, 2015
 *  version Jun. 21, 2015
 *  version Jul. 31, 2015
 *  version Aug. 10, 2015
 *  version Aug. 31, 2018
 *  version Sep. 17, 2018
 * @version Oct. 16, 2018
 * @author  ASAMI, Tomoharu
 */
case class Record(
  fields: IndexedSeq[Field],
  meta: Record.MetaData = Record.MetaData.empty, // XXX (composite record?)
  timestamp: Long = System.currentTimeMillis, // XXX (composite record?)
  validation: ValidationResult = Valid, // XXX (composite record?)
  source: IndexedSeq[Record] = IndexedSeq.empty, // XXX (composite record?)
  exception: Option[Throwable] = None // XXX (composite record?)
) extends IRecord
    with XmlPart with JsonPart with CsvPart with LtsvPart
    with HttpPart with SqlPart
    with CompatibilityPart {
  def toRecord = this
  def toRecord2: Record2 = Record2.createApp(nameValues)

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
  def isDefined(key: Symbol): Boolean = fields.exists(_.key == key)
  def isDefined(key: String): Boolean = isDefined(Symbol(key))

  def nonDefined(key: Symbol): Boolean = !isDefined(key)
  def nonDefined(key: String): Boolean = !isDefined(key)

  def get(key: String): Option[Any] = getField(key).flatMap(_.value.getValue)

  def get(key: Symbol): Option[Any] = getField(key).flatMap(_.value.getValue)

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
  def +(rhs: Record): Record = update(rhs)
  def +(rhs: IRecord): IRecord = update(rhs.toRecord)

  def update(rec: Record): Record =
    rec.fields.foldLeft(this)((z, x) => z.updateField(x.key, x.value))

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
      prefix :+ Field(key, value, validation = validation)
    } else {
      prefix ++ (Field(key, value, validation = validation) +: suffix.tail)
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

  def updateField(key: Symbol, value: FieldValue): Record = {
    val (prefix, suffix) = fields.span(_.key != key) // XXX isMatch?
    val r = suffix.toList match {
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

  def buildJsonString(buf: StringBuilder) {
    def buildfield(kv: (String, Any)) {
      buf.append("\"")
      buf.append(kv._1)
      buf.append("\":")
      JsonUtils.data2json(buf, kv._2)
    }
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

  def data(xs: (Symbol, Any)*): Record = {
    Record(xs.map(Field.create).toVector)
  }

  def dataOption(xs: (Symbol, Option[Any])*): Record = {
    val a = xs flatMap {
      case (k, Some(v)) => Some(k -> v)
      case (k, None) => None
    }
    data(a: _*)
  }

  // def fromDataSeq(data: Seq[(String, Any)]): Record = {
  //   Record(data.map(Field.fromData).toVector)
  // }

  // def fromDataOptionSeq(data: Seq[(String, Any)]): Record = {
  //   val a = data flatMap {
  //     case (k, Some(v)) => Some(k -> v)
  //     case (k, None) => None
  //   }
  //   fromDataSeq(a)
  // }

  def createDataSeq(data: Seq[(String, Any)]): Record =
    Record(data.map(Field.createData).toVector)

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

  def fromException(e: Throwable): Record = {
    Record(Vector.empty, exception = Some(e))
  }

  def fromJson(p: String): Record = create(Json.parse(p))

  def create(p: JsValue): Record = p match {
    case null => Record.empty
    case JsNull => Record.empty
    case m: JsObject => create(m)
    case _: JsArray => throw new IllegalArgumentException(s"Array: $p")
    case _ => throw new IllegalArgumentException(s"Not object: $p")
  }

  def create(p: JsObject): Record = {
    val xs = p.fields.map {
      case (k, v) => Field.create(k, v)
    }
    Record(xs.toVector)
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
