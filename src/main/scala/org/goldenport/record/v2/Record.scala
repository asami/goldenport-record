package org.goldenport.record.v2

import java.util.Date
import java.net.URL
import java.sql.Timestamp
import scala.util.control.NonFatal
import scala.math.{BigInt, BigDecimal}
import org.goldenport.Strings
import org.goldenport.Strings.notblankp
import org.goldenport.record.util.{TimestampUtils, DateUtils}
import org.goldenport.record.util.{AnyUtils}

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
 *  version Jan. 28, 2015
 *  version Aug. 28, 2015
 *  version Sep. 17, 2015
 *  version Oct. 28, 2015
 *  version Nov. 30, 2015
 * @version Dec.  3, 2015
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
  fields: List[Field], // TODO Seq
  // context
  principal: Option[Principal] = None,
  timestamp: Long = System.currentTimeMillis,
  inputFiles: Seq[InputFile] = Nil,
  opaque: AnyRef = null,
  source: Option[Record] = None
) extends CommandPart with EagerListPart {
  override def equals(o: Any): Boolean = {
    o match {
      case rec: Record if length == rec.length =>
        fields.forall(x => rec.get(x.key) == Some(x.values))
      case _ => false
    }
  }

  def getField(key: Symbol): Option[Field] = {
    fields.find(_.isMatchKey(key))
  }

  def getField(key: String): Option[Field] = {
    getField(Symbol(key))
  }

  def get(key: Symbol): Option[List[Any]] = {
    fields.find(_.isMatchKey(key)).map(_.values)
  }

  // TODO unify Field
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

  def getFormOne(key: Symbol): Option[Any] = {
    getOne(key) filter {
      case "" => false
      case _ => true
    }
  }

  // TODO unify Field
  def getString(key: Symbol): Option[String] = {
    getOne(key).map(AnyUtils.toString)
  }

  def getFormString(key: Symbol): Option[String] = {
    getOne(key) flatMap {
      case "" => None
      case x => Some(AnyUtils.toString(x))
    }
  }

  def getConcreteString(key: Symbol): Option[String] = {
    getField(key).flatMap(_.getConcreteString)
  }

  def getBoolean(key: Symbol): Option[Boolean] = {
    getOne(key).map { _.toString.toLowerCase match {
      case "1" => true
      case "0" => false
      case "true" => true
      case "false" => false
    }}
  }

  def getFormBoolean(key: Symbol): Option[Boolean] = {
    getOne(key) flatMap {
      case "" => None
      case x => getBoolean(key)
    }
  }

  def getInt(key: Symbol): Option[Int] = {
    getOne(key).map(AnyUtils.toInt)
  }

  def getFormInt(key: Symbol): Option[Int] = {
    getOne(key) flatMap {
      case "" => None
      case x => Some(AnyUtils.toInt(x))
    }
  }

  def getLong(key: Symbol): Option[Long] = {
    getOne(key).map(AnyUtils.toLong)
  }

  def getFormLong(key: Symbol): Option[Long] = {
    getOne(key) flatMap {
      case "" => None
      case x => Some(AnyUtils.toLong(x))
    }
  }

  def getFloat(key: Symbol): Option[Float] = {
    getOne(key).map(AnyUtils.toFloat)
  }

  def getFormFloat(key: Symbol): Option[Float] = {
    getOne(key) flatMap {
      case "" => None
      case x => Some(AnyUtils.toFloat(x))
    }
  }

  def getDouble(key: Symbol): Option[Double] = {
    getOne(key).map(AnyUtils.toDouble)
  }

  def getFormDouble(key: Symbol): Option[Double] = {
    getOne(key) flatMap {
      case "" => None
      case x => Some(AnyUtils.toDouble(x))
    }
  }

  def getTimestamp(key: Symbol): Option[Timestamp] = {
    getOne(key).map {
      case x: Timestamp => x
      case l: Long => new Timestamp(l)
      case s: String => TimestampUtils.parse(s)
    }
  }

  def getFormTimestamp(key: Symbol): Option[Timestamp] = {
    getOne(key) flatMap {
      case "" => None
      case x => getTimestamp(key)
    }
  }

  def getDate(key: Symbol): Option[Date] = {
    getOne(key).map {
      case x: Date => x
      case s: String => DateUtils.parse(s)
    }
  }

  def getBigDecimal(key: Symbol): Option[BigDecimal] = {
    getOne(key) map {
      case v: Byte => BigDecimal(v)
      case v: Short => BigDecimal(v)
      case v: Int => BigDecimal(v)
      case v: Long => BigDecimal(v)
      case v: Float => BigDecimal(v)
      case v: Double => BigDecimal(v)
      case v: BigInt => BigDecimal(v)
      case v: BigDecimal => v
      case v => BigDecimal(v.toString)
    }
  }

  def getList(key: Symbol): List[Any] = {
    get(key) getOrElse Nil
  }

  def getFormList(key: Symbol): List[Any] = {
    getList(key) flatMap {
      case s: String => if (Strings.blankp(s)) None else Some(s)
      case m => Some(m)
    }
  }

  def getStringList(key: Symbol): List[String] = {
    getList(key).map(AnyUtils.toString)
  }

  def getFormStringList(key: Symbol): List[String] = {
    getStringList(key).filter(notblankp)
  }

  def getIntList(key: Symbol): List[Int] = {
    getList(key).map(AnyUtils.toInt)
  }

  def getFormIntList(key: Symbol): List[Int] = {
    getFormStringList(key).map(AnyUtils.toInt)
  }

  def getLongList(key: Symbol): List[Long] = {
    getList(key).map(AnyUtils.toLong)
  }

  def getFormLongList(key: Symbol): List[Long] = {
    getFormStringList(key).map(_.toLong)
  }

  def getFloatList(key: Symbol): List[Float] = {
    getList(key).map(AnyUtils.toFloat)
  }

  def getFormFloatList(key: Symbol): List[Float] = {
    getFormStringList(key).map(AnyUtils.toFloat)
  }

  def getDoubleList(key: Symbol): List[Double] = {
    getList(key).map(AnyUtils.toDouble)
  }

  def getFormDoubleList(key: Symbol): List[Double] = {
    getFormStringList(key).map(AnyUtils.toDouble)
  }

  def getUrlList(key: Symbol): List[URL] = {
    getList(key).map(AnyUtils.toUrl)
  }

  def getFormUrlList(key: Symbol): List[URL] = {
    getFormList(key).map(AnyUtils.toUrl)
  }

  def getRecordList(key: Symbol): List[Record] = getRecords(key)

  def getRecords(key: Symbol): List[Record] = {
    effectiveList(key) collect {
      case xs: Record => xs
    }
  }

  def get(key: String): Option[List[Any]] = {
    get(Symbol(key))
  }

  def getOne(key: String): Option[Any] = {
    getOne(Symbol(key))
  }

  def getFormOne(key: String): Option[Any] = {
    getFormOne(Symbol(key))
  }

  def getString(key: String): Option[String] = {
    getString(Symbol(key))
  }

  def getFormString(key: String): Option[String] = {
    getFormString(Symbol(key))
  }

  def getConcreteString(key: String): Option[String] = {
    getConcreteString(Symbol(key))
  }

  def getBoolean(key: String): Option[Boolean] = {
    getBoolean(Symbol(key))
  }

  def getFormBoolean(key: String): Option[Boolean] = {
    getFormBoolean(Symbol(key))
  }

  def getInt(key: String): Option[Int] = {
    getInt(Symbol(key))
  }

  def getFormInt(key: String): Option[Int] = {
    getFormInt(Symbol(key))
  }

  def getLong(key: String): Option[Long] = {
    getLong(Symbol(key))
  }

  def getFormLong(key: String): Option[Long] = {
    getFormLong(Symbol(key))
  }

  def getFloat(key: String): Option[Float] = {
    getFloat(Symbol(key))
  }

  def getFormFloat(key: String): Option[Float] = {
    getFormFloat(Symbol(key))
  }

  def getDouble(key: String): Option[Double] = {
    getDouble(Symbol(key))
  }

  def getFormDouble(key: String): Option[Double] = {
    getFormDouble(Symbol(key))
  }

  def getTimestamp(key: String): Option[Timestamp] = {
    getTimestamp(Symbol(key))
  }

  def getFormTimestamp(key: String): Option[Timestamp] = {
    getFormTimestamp(Symbol(key))
  }

  def getDate(key: String): Option[Date] = {
    getDate(Symbol(key))
  }

  def getBigDecimal(key: String): Option[BigDecimal] = {
    getBigDecimal(Symbol(key))
  }

  def getList(key: String): List[Any] = {
    getList(Symbol(key))
  }

  def getFormList(key: String): List[Any] = {
    getFormList(Symbol(key))
  }

  def getStringList(key: String): List[String] = {
    getList(key).map(AnyUtils.toString)
  }

  def getFormStringList(key: String): List[String] = {
    getStringList(key).filter(notblankp)
  }

  def getIntList(key: String): List[Int] = {
    getList(key).map(AnyUtils.toInt)
  }

  def getFormIntList(key: String): List[Int] = {
    getFormStringList(key).map(_.toInt)
  }

  def getLongList(key: String): List[Long] = {
    getList(key).map(AnyUtils.toLong)
  }

  def getFormLongList(key: String): List[Long] = {
    getFormStringList(key).map(AnyUtils.toLong)
  }

  def getFloatList(key: String): List[Float] = {
    getList(key).map(AnyUtils.toFloat)
  }

  def getFormFloatList(key: String): List[Float] = {
    getStringList(key).map(AnyUtils.toFloat)
  }

  def getDoubleList(key: String): List[Double] = {
    getList(key).map(AnyUtils.toDouble)
  }

  def getFormDoubleList(key: String): List[Double] = {
    getStringList(key).map(AnyUtils.toDouble)
  }

  def getUrlList(key: String): List[URL] = {
    getList(key).map(AnyUtils.toUrl)
  }

  def getFormUrlList(key: String): List[URL] = {
    getFormList(key).map(AnyUtils.toUrl)
  }

  def getRecordList(key: String): List[Record] = {
    getRecordList(Symbol(key))
  }

  private def _no_such_element(key: Symbol): Nothing = {
    val msg = getString('id) match {
      case Some(s) => s"No such element '${key.name}' in '$s'"
      case None => s"No such element '${key.name}'"
    }
    throw new NoSuchElementException(msg)
  }

  def asString(key: Symbol): String = {
    getOne(key).map(AnyUtils.toString) getOrElse {
      _no_such_element(key)
    }
  }

  def asBoolean(key: Symbol): Boolean = {
    getBoolean(key) getOrElse _no_such_element(key)
  }

  def asInt(key: Symbol): Int = {
    AnyUtils.toInt(getOne(key).getOrElse(_no_such_element(key)))
  }

  def asLong(key: Symbol): Long = {
    AnyUtils.toLong(getOne(key).getOrElse(_no_such_element(key)))
  }

  def asBigDecimal(key: Symbol): BigDecimal = {
    getBigDecimal(key).getOrElse(_no_such_element(key))
  }

  def asTimestamp(key: Symbol): Timestamp = {
    getTimestamp(key) getOrElse {
      throw new IllegalArgumentException("bad timestamp = " + getOne(key))
    }
  }

  def asDate(key: Symbol): Date = {
    getDate(key) getOrElse {
      throw new IllegalArgumentException("bad date = " + getOne(key))
    }
  }

  def asString(key: String): String = {
//    AnyUtils.toString(getOne(key).get)
    asString(Symbol(key))
  }

  def asBoolean(key: String): Boolean = {
//    getBoolean(key).get
    asBoolean(Symbol(key))
  }

  def asInt(key: String): Int = {
//    AnyUtils.toInt(getOne(key).get)
    asInt(Symbol(key))
  }

  def asLong(key: String): Long = {
//    AnyUtils.toLong(getOne(key).get)
    asLong(Symbol(key))
  }

  def asBigDecimal(name: String): BigDecimal = {
    asBigDecimal(Symbol(name))
  }

  def asTimestamp(name: String): Timestamp = {
    asTimestamp(Symbol(name))
  }

  def asDate(name: String): Date = {
    asDate(Symbol(name))
  }

  def paramString(key: Symbol): String = {
    getString(key) getOrElse {
      throw new IllegalArgumentException(s"Missing parameter '${key.name}'")
    }
  }

  def paramBoolean(key: Symbol): Boolean = {
    getBoolean(key).get
  }

  def paramInt(key: Symbol): Int = {
    getInt(key) getOrElse {
      throw new IllegalArgumentException(s"Missing parameter '${key.name}'")
    }
  }

  def paramLong(key: Symbol): Long = {
    getLong(key) getOrElse {
      throw new IllegalArgumentException(s"Missing parameter '${key.name}'")
    }
  }

  def paramBigDecimal(key: Symbol): BigDecimal = {
    getBigDecimal(key) getOrElse {
      throw new IllegalArgumentException(s"Missing parameter '${key.name}'")
    }
  }

  def paramTimestamp(key: Symbol): Timestamp = {
    getTimestamp(key) getOrElse {
      throw new IllegalArgumentException(s"Missing parameter '${key.name}'")
    }
  }

  def paramDate(key: Symbol): Date = {
    getDate(key) getOrElse {
      throw new IllegalArgumentException(s"Missing parameter '${key.name}'")
    }
  }

  def paramString(key: String): String = {
    paramString(Symbol(key))
  }

  def paramBoolean(key: String): Boolean = {
    paramBoolean(Symbol(key))
  }

  def paramInt(key: String): Int = {
    paramInt(Symbol(key))
  }

  def paramLong(key: String): Long = {
    paramLong(Symbol(key))
  }

  def paramBigDecimal(name: String): BigDecimal = {
    paramBigDecimal(Symbol(name))
  }

  def paramTimestamp(name: String): Timestamp = {
    paramTimestamp(Symbol(name))
  }

  def paramDate(name: String): Date = {
    paramDate(Symbol(name))
  }

  def getValue(column: Column): Option[List[Any]] = {
    if (column.isSingle)
      getOne(column.name).map(x => List(column.datatype.toInstance(x)))
    else
      get(column.name).map(_.map(column.datatype.toInstance(_)))
  }

  def length(): Int = fields.length
  def isEmpty() = fields.isEmpty
  def nonEmpty() = fields.nonEmpty

  def isDefined(key: Symbol): Boolean = fields.exists(_.isMatchKey(key))
  def isDefined(key: String): Boolean = isDefined(Symbol(key))

  def isSourceDefined(key: Symbol): Boolean = {
    source.map(x => x.isDefined(key) || x.isSourceDefined(key)) getOrElse false
  }
  def isSourceDefined(key: String): Boolean = isSourceDefined(Symbol(key))

  def effectiveList(key: Symbol): List[Any] = {
    getField(key).map(_.effectiveList) getOrElse Nil
  }

  def effectiveList(key: String): List[Any] = {
    getField(key).map(_.effectiveList) getOrElse Nil
  }

  def keyStringValues: Seq[(String, Any)] = {
    fields.flatMap(_.keyStringValue)
  }

  def isOnlyKeys(keys: Seq[String]): Boolean = {
    val a = fields.map(_.key.name)
    val b = a diff keys
    b.isEmpty
  }

  def isOnlyKeys(f: String => Boolean): Boolean = {
    val a = fields.map(_.key.name)
    val b = a.filterNot(f)
    b.isEmpty
  }

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

  def :+(f: Field): Record = {
    copy(fields :+ f)
  }

  def +(r: Record): Record = update(r)

  def monoidAppend(rhs: Record): Record = {
    this + rhs // TODO associative law
  }

  // updatePreserve
  def update(r: Record): Record = {
    if (fields.isEmpty) r
    else if (r.isEmpty) this
    else {
      val keys: List[Symbol] = r.fields.map(_.key)
      val b = fields.filterNot(x => keys.contains(x.key))
      copy(fields = b ++ r.fields)
    }
  }

  // updatePreserve
  def updateS(f: (Symbol, Any)): Record =
    updateS(Vector(f))

  // updatePreserve
  def updateS(fs: Seq[(Symbol, Any)]): Record = {
    val keys: Seq[Symbol] = fs.map(_._1)
    val a = fs.map(Field.createS)
    val b = fields.filterNot(x => keys.contains(x.key))
    copy(fields = b ++ a)
  }

  // updatePreserve
  def update(f: (String, Any)): Record =
    update(Vector(f))

  // updatePreserve
  def update(fs: Seq[(String, Any)]): Record = {
    if (fs.isEmpty) {
      this
    } else {
      val keys: Seq[Symbol] = fs.map(x => Symbol(x._1))
      val a = fs.map(Field.create)
      val b = fields.filterNot(x => keys.contains(x.key))
      copy(fields = b ++ a)
    }
  }

  // updatePreserve
  def updateAppS(f: (Symbol, Any)): Record =
    updateAppS(Vector(f))

  // updatePreserve
  def updateAppS(fs: Seq[(Symbol, Any)]): Record = {
    if (fs.isEmpty) {
      this
    } else {
      val keys: Seq[Symbol] = fs.map(_._1)
      val a = fs.map(Field.createSingleS)
      val b = fields.filterNot(x => keys.contains(x.key))
      copy(fields = b ++ a)
    }
  }

  // updatePreserve
  def updateApp(f: (String, Any)): Record =
    updateApp(Vector(f))

  // updatePreserve
  def updateApp(fs: Seq[(String, Any)]): Record = {
    if (fs.isEmpty) {
      this
    } else {
      val keys: Seq[Symbol] = fs.map(x => Symbol(x._1))
      val a = fs.map(Field.createSingle)
      val b = fields.filterNot(x => keys.contains(x.key))
      copy(fields = b ++ a)
    }
  }

  def updatePreserve(f: (String, Any)): Record = {
    updatePreserveS(f.copy(_1 = Symbol(f._1)))
  }

  def updatePreserveS(f: (Symbol, Any)): Record = {
    val (prefix, suffix) = fields.span(_.key != f._1) // XXX isMatch?
    val r = suffix match {
      case Nil => prefix :+ Field.createS(f)
      case x :: xs => prefix ::: (Field.createS(f) :: xs)
    }
    copy(fields = r)
  }

  def isActive(key: Symbol): Boolean = {
    getOne(key).map {
      case x: String => Strings.notblankp(x)
      case _ => true
    } getOrElse false
  }

  def isActive(key: String): Boolean = {
    isActive(Symbol(key))
  }

/*
  def complements(f: Seq[(Symbol, Any)]): Record = {
    val a = f.filterNot(x => exists(x._1))
    this ::++ a
  }
*/

  def complements(f: Seq[(String, Any)]): Record = {
    if (f.isEmpty) {
      this
    } else {
      val a = f.filterNot(x => isDefined(x._1))
      this ::++ a
    }
  }

  def normalizeImages(fieldname: String): Record = {
    val a = Symbol(fieldname)
    fields.find(_.key == a) match {
      case Some(f) => {
        val b = fields.filter(_.key != a)
        val c = for (x <- f.values) yield {
          InputFile.createByUrlStringAutoName(fieldname, x.toString)
        }
        this.copy(b, inputFiles = c)
      }
      case None => this
    }
  }

  def withInputFiles(files: InputFile*): Record = {
    copy(inputFiles = files)
  }

  def withOpaque(o: AnyRef): Record = {
    copy(_with_opaque_fields(o), opaque = o)
  }

  private def _with_opaque_fields(o: AnyRef): List[Field] = {
    for (f <- fields) yield {
      if (f.values.exists(_.isInstanceOf[Record])) {
        f.copy(values = _with_opqaue_values(o, f.values))
      } else f
    }
  }

  private def _with_opqaue_values(o: AnyRef, vs: List[Any]): List[Any] = {
    vs.map {
      case rec: Record => rec.withOpaque(o)
      case v => v
    }
  }

  def withSource(source: Record): Record = {
    copy(source = Some(source))
  }

  def normalizeMultiplicity(): Record = {
    val files = Record.files2Fields(inputFiles)
//    println("Record#normalizeMultiplicity files = " + files)
    val multiplied = Record.normalizeMultiplicity(fields ++ files)
//    println("Record#normalizeMultiplicity multiplied = " + multiplied)
    val (a, b) = Record.toFieldsAndFiles(multiplied)
//    println("Record#normalizeMultiplicity a, b = " + a + "/" + b)
    copy(a, inputFiles = b)
  }

  def transform(f: Field => List[Field]): Record = {
    copy(fields = fields.flatMap(f))
  }

  def replace(f: Field => Option[Field]): Record = {
    copy(fields = fields.map(x => f(x) match {
      case Some(s) => s
      case None => x
    }))
  }

  def replaceKey(from: String, to: String): Record = {
    replaceKey(Symbol(from), Symbol(to))
  }

  def replaceKey(from: Symbol, to: Symbol): Record = {
    copy(fields = fields.map {
      case f if f.key == from => f.copy(key = to)
      case f => f
    })
  }

  def replaceValueOne(key: String, f: Any => Any): Record = {
    replaceValueOne(Symbol(key), f)
  }

  def replaceValueOne(key: Symbol, f: Any => Any): Record = {
    fields.find(_.key == key) match {
      case Some(s) =>
        s.getOne match {
          case Some(x) =>
            val v = List(f(x))
            val xs: List[Field] = fields.takeWhile(_ != s) ::: (Field(key, v) +: fields.dropWhile(_ != s).tail)
          	copy(fields = xs)
          case None => this
        }
      case None => this
    }
  }

  def replaceValueString(key: String, f: String => String): Record = {
    replaceValueString(Symbol(key), f)
  }

  def replaceValueString(key: Symbol, f: String => String): Record = {
    fields.find(_.key == key) match {
      case Some(s) =>
        s.getString match {
          case Some(x) =>
            val v = List(f(x))
            val xs = fields.takeWhile(_ != s) ::: (Field(key, v) :: fields.dropWhile(_ != s).tail)
            copy(fields = xs)
          case None => this
        }
      case None => this
    }
  }

  def activateFields(keys: Seq[Symbol]) = {
    copy(fields = fields.filter(x => keys.contains(x.key)))
  }

  def removeField(key: String): Record = {
    removeField(Symbol(key))
  }

  def removeField(key: Symbol): Record = {
    removeFields(Vector(key))
  }

  def removeFields(keys: Seq[Symbol]): Record = {
    copy(fields = fields.filterNot(x => keys.contains(x.key)))
  }

  def removeFields(f: Field => Boolean): Record = {
    copy(fields = fields.filter(f))
  }

  def removeFieldsByName(p: String => Boolean): Record = {
    copy(fields = fields.filterNot(x => p(x.key.name)))
  }

  override def toString(): String = {
    "Record(" + fields + ", " + inputFiles + ")"
  }

  def toMap: Map[String, Any] = {
    Map.empty ++ fields.flatMap(f => f.effectiveValue.map(v => f.key.name -> v))
  }

  def toStringMap: Map[String, String] = {
    Map.empty ++ fields.flatMap(f => f.effectiveValue.map(v => f.key.name -> AnyUtils.toString(v)))
  }

  def toVector: Vector[(String, Any)] = {
    Vector.empty ++ fields.map { x =>
      x.key.name -> to_natural_value(x.values)
    }
  }

  protected def to_natural_value(v: List[Any]): Any = {
    v match {
      case Nil => None
      case List(x) => x
      case xs => xs // includes List[List[_]]. Addressed as List[_].
    }
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
          val a = v.map(x => f(AnyUtils.toDouble(x)))
          Field(key, a)
        }
      }
    } catch {
      case NonFatal(e) => this
    }
  }

  def mapDecimal(f: BigDecimal => BigDecimal): Field = {
    try {
      values match {
        case Nil => this
        case v => {
          val a = v.map(x => f(AnyUtils.toBigDecimal(x)))
          Field(key, a)
        }
      }
    } catch {
      case NonFatal(e) => this
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

  def effectiveValue: Option[Any] = {
    values match {
      case Nil => None
      case x :: Nil => {
        x match {
          case y: List[_] => Some(y)
          case _ => Some(x)
        }
      }
      case xs => Some(xs)
    }
  }

  def effectiveList: List[Any] = values match {
    case Nil => Nil
    case List(Nil) => Nil
    case List(xs: Seq[_]) => xs.toList
    case xs => xs
  }

  def keyValue: Option[(Symbol, Any)] = {
    val data = values match {
      case Nil => None
      case x :: Nil => Some(x)
      case xs => Some(xs)
    }
    data.map(x => key -> x)
  }

  def keyStringValue: Option[(String, Any)] = {
    keyValue map {
      case (k, v) => k.name -> v
    }
  }

  def getOne: Option[Any] = {
    values match {
      case Nil => None
      case x :: _ => Some(x)
    }
  }

  def getString: Option[String] = {
    getOne.map(AnyUtils.toString)
  }

  def getConcreteString: Option[String] = {
    getOne flatMap {
      case s: String if Strings.blankp(s) => None
      case x => Some(AnyUtils.toString(x))
    }
  }

  def getConcreteStringList: List[String] = {
    values flatMap {
      case s: String if Strings.blankp(s) => None
      case x => Some(AnyUtils.toString(x))
    }
  }

  def updateKey(key: String): Field = {
    updateKey(Symbol(key))
  }

  def updateKey(k: Symbol): Field = {
    if (key == k) this else copy(key = k)
  }
}

object RecordSet {
  val empty = RecordSet(Vector.empty)

  def create(map: Seq[scala.collection.Map[String, Any]]): RecordSet = {
    RecordSet(map.map(Record.create))
  }
}

object Record {
  val empty = Record(Nil)
  val multiplicityRegex = """__(\d+)_""".r
  val groupRegex = """__G_""".r

  def create(map: scala.collection.Map[String, Any]): Record = {
    create(map.toList)
  }

  def create(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.create).toList)
  }

  def createS(data: Seq[(Symbol, Any)]): Record = {
    Record(data.map(Field.createS).toList)
  }

  /*
   * Uses the method in case of List as single object.
   */
  def createApp(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.createSingle).toList)
  }

  def createAppS(data: Seq[(Symbol, Any)]): Record = {
    Record(data.map(Field.createSingleS).toList)
  }

  def createAppOption(data: Seq[(String, Option[Any])]): Record = {
    val a = data.flatMap {
      case (k, Some(v)) => Some(k -> v)
      case (k, None) => None
    }
    createApp(a)
  }

  def createAppOptionS(data: Seq[(Symbol, Option[Any])]): Record = {
    val a = data.flatMap {
      case (k, Some(v)) => Some(k -> v)
      case (k, None) => None
    }
    createAppS(a)
  }

  @deprecated("Use createApp instead.", "0.2.22")
  def createSingle(data: Seq[(String, Any)]): Record = {
    Record(data.map(Field.createSingle).toList)
  }

  def data(data: (String, Any)*): Record = {
    create(data)
  }

  def dataApp(data: (String, Any)*): Record = {
    createApp(data)
  }

  def dataAppOption(data: (String, Option[Any])*): Record = {
    createAppOption(data)
  }

  def dataS(data: (Symbol, Any)*): Record = {
    createS(data)
  }

  def dataAppS(data: (Symbol, Any)*): Record = {
    createAppS(data)
  }

  def dataAppOptionS(data: (Symbol, Option[Any])*): Record = {
    createAppOptionS(data)
  }

  def normalizeMultiplicity(fs: List[Field]): List[Field] = {
    val (candidates, normal) = fs.partition(_is_multiplicity)
//    println("Record#normalizeMultiplicity %s / %s".format(normal, candidates))
    val r = normal ++ _normalize_multiplicity(candidates)
//    println("Record#normalizeMultiplicity %s = %s".format(fs, r))
    r
  }

  private def _is_multiplicity(f: Field): Boolean = {
    val r = multiplicityRegex.findFirstMatchIn(f.key.name).isDefined
//    println("Record#_is_multiplicity %s = %s".format(f, r))
    r
  }

  private def _normalize_multiplicity(fs: List[Field]): List[Field] = {
    val a = fs.map(_normalize_multiplicity)
    _chunks_to_fields(a)
  }

  private def _normalize_multiplicity(f: Field): MultiplicityChunk = {
    val m = multiplicityRegex.findFirstMatchIn(f.key.name)
    m.map(x => MultiplicityChunk(x.before.toString, x.group(1).toInt, x.after.toString, f)).get
  }

  private def _chunks_to_fields(chunks: List[MultiplicityChunk]): List[Field] = {
    val a = chunks.groupBy(_.attrname).toList
    val b = _aggregate_fields(a)
    b
  }

  private def _aggregate_fields(a: List[(String, List[MultiplicityChunk])]): List[Field] = {
    a.flatMap(_aggregate_field)
  }

  private def _aggregate_field(a: (String, List[MultiplicityChunk])): List[Field] = {
    val attrname = a._1
    val b: List[(Int, List[MultiplicityChunk])] = a._2.groupBy(_.index).toList.sortBy(_._1)
    val d = for (c: List[MultiplicityChunk] <- b.map(_._2)) yield {
      val multiplied = _normalize_multiplicity_chunks(c)
      val (a, b) = Record.toFieldsAndFiles(multiplied)
      Record(a, inputFiles = b)
    }
    List(Field.create(attrname, d))
  }

  private def _normalize_multiplicity_chunks(fs: List[MultiplicityChunk]): List[Field] = {
    val (candidates, normal) = fs.span(_is_multiplicity)
    _to_fields(normal) ++ _normalize_multiplicity_chunk_candidates(candidates)
  }

  private def _is_multiplicity(chunk: MultiplicityChunk): Boolean = {
    multiplicityRegex.findFirstMatchIn(chunk.remainder).isDefined
  }

  private def _to_fields(chunks: List[MultiplicityChunk]): List[Field] = {
    chunks.map(x => Field.create(x.remainder, x.field.values))
  }

  private def _normalize_multiplicity_chunk_candidates(chunks: List[MultiplicityChunk]): List[Field] = {
    val a = chunks.map(_normalize_multiplicity_chunk_candidate)
    _chunks_to_fields(a)
  }

  private def _normalize_multiplicity_chunk_candidate(chunk: MultiplicityChunk): MultiplicityChunk = {
    val m = multiplicityRegex.findFirstMatchIn(chunk.remainder)
    m.map(x => MultiplicityChunk(x.before.toString, x.group(1).toInt, x.after.toString, chunk.field)).get
  }

  def files2Fields(files: Seq[InputFile]): List[Field] = {
    files.map(x => Field.create(x.key, x)).toList
  }

  def toFieldsAndFiles(fs: List[Field]): (List[Field], List[InputFile]) = {
    fs.foldRight((List[Field](), List[InputFile]())) { (x, a) =>
      x.values match {
        case (f: InputFile) :: Nil => (a._1, f.withKey(x.key) :: a._2)
        case _ => (x :: a._1, a._2)
      }
    }
  }

  //
  def normalizeGroup(rs: Seq[Record], key: Symbol = 'id): Seq[Record] = {
    val a = rs.groupBy(_.asString(key))
    _aggregate_in_group(a)
  }

  private def _aggregate_in_group(rs: Map[String, Seq[Record]]): Seq[Record] = {
    val a = rs.toList.map(_aggregate_in_group)
    println("Record._aggregate_in_group map2map = " + a)
    a.map(_._2)
  }

/*
  private def _aggregate_in_group(rs: Map[String, Seq[Record]]): Map[String, Record] = {
    val a = rs.toList.map(_aggregate_in_group)
    println("Record._aggregate_in_group map2map = " + a)
    Map.empty ++ a
  }
*/

  private def _aggregate_in_group(a: (String, Seq[Record])): (String, Record) = {
    (a._1, _aggregate_in_group_rs_to_record(a._2))
  }

  private def _aggregate_in_group_rs_to_record(rs: Seq[Record]): Record = {
    val fields = rs.headOption.get.fields.filterNot(f =>
      Record.groupRegex.findFirstMatchIn(f.key.name).isDefined)
    val a: Seq[(String, Record)] = rs flatMap { r =>
      val b: Seq[GroupChunk] = r.fields flatMap { f =>
        val m = Record.groupRegex.findFirstMatchIn(f.key.name)
        m map (r => GroupChunk(r.before.toString, r.after.toString, f))
      }
      val c: Map[String, Seq[GroupChunk]] = b.groupBy(_.parent)
      _aggregate_in_group_chunk_to_record(c)
    }
    println("Record#_aggregate_in_group a = " + a)
    val d: Map[String, Seq[(String, Record)]] = a.groupBy(_._1)
    println("Record#_aggregate_in_group d = " + d)
    val e: Seq[(String, Seq[Record])] = _aggregate_in_group_fold(d)
    println("Record#_aggregate_in_group e = " + e)
    val f: Seq[Field] = e.map { x =>
      Field(Symbol(x._1), x._2.toList)
    }
    println("Record#_aggregate_in_group f = " + f)
    println("Record#_aggregate_in_group fields = " + fields)
    Record(fields ++ f)
  }

  private def _aggregate_in_group_chunk_to_record(a: Map[String, Seq[GroupChunk]]): Seq[(String, Record)] = {
    for (k <- a.keys.toList) yield {
      val rs = a.get(k) match {
        case Some(s) => Record(s.toList.map(x => Field.create(x.child, x.field.values)))
        case None => sys.error("???")
      }
      (k, rs)
    }
  }

  protected[v2] def _aggregate_in_group_fold(a: Map[String, Seq[(String, Record)]]): Seq[(String, Seq[Record])] = {
    for (k <- a.keys.toList) yield {
      val rs: Seq[Record] = a.get(k) match {
        case Some(s) => {
          val b = s.foldRight((List[Record](), Set.empty[String]))((x, a) => {
            val rec = x._2
            rec.getString("id") match {
              case Some(id) => {
                if (a._2.contains(id)) a
                else (rec :: a._1, a._2 + id)
              }
              case None => sys.error("???")
            }
          })
          b._1
        }
        case None => sys.error("???")
      }
      (k, rs)
    }
  }
/*
  private def _aggregate_in_group0(rs: Seq[Record]): Record = {
    val fields = rs.headOption.get.fields.filterNot(_.key.name.contains("__"))
    val a: Seq[Field] = rs flatMap { r =>
      val b: Seq[(String, String, Field)] = r.fields flatMap { f =>
        val m = Record.groupRegex.findFirstMatchIn(f.key.name)
        m map { r =>
          (r.before.toString, r.after.toString, f)
        }
      }
      val c = b.groupBy(_._1)
      _aggregate_in_group(c)
    }
    Record(fields ++ a)
  }

  private def _aggregate_in_group(
    a: Map[String, Seq[(String, String, Field)]]
  ): List[Field] = {
    a.toList.map(_aggregate_in_group)
  }

  private def _aggregate_in_group(
    a: (String, Seq[(String, String, Field)])
  ): Field = {
    Field(Symbol(a._1), List(_aggregate_in_group_record(a._2)))
  }

  private def _aggregate_in_group_record(
    a: Seq[(String, String, Field)]
  ): Record = {
    val b = a.groupBy(_._2)
    Record.create(
      b.map(kv => kv._1 -> kv._3.values)
    )
  }
*/
}

case class MultiplicityChunk(
  attrname: String,
  index: Int,
  remainder: String,
  field: Field
)

case class GroupChunk(
  parent: String,
  child: String,
  field: Field
)

object Field {
  def createS(data: (Symbol, Any)): Field = {
    data._2 match {
      case Some(x) => Field(data._1, List(x))
      case None => Field(data._1, Nil)
      case xs: Seq[_] => Field(data._1, xs.toList)
      case x => Field(data._1, List(x))
    }
  }

  def create(data: (String, Any)): Field = {
    data._2 match {
      case Some(x) => Field(Symbol(data._1), List(x))
      case None => Field(Symbol(data._1), Nil)
      case xs: Seq[_] => Field(Symbol(data._1), xs.toList)
      case x => Field(Symbol(data._1), List(x))
    }
  }

  def createSingleS(data: (Symbol, Any)): Field = {
    data._2 match {
      case Some(x) => Field(data._1, List(x))
      case None => Field(data._1, Nil)
      case xs: Seq[_] => {
        if (_is_seq_seq_tuple2(xs)) {
          val rs = _seq_seq_tuple2_to_records(xs)
          Field(data._1, List(rs))
        } else if (_is_seq_tuple2(xs)) {
          val r = _seq_tuple2_to_record(xs)
          Field(data._1, List(r))
        } else {
          Field(data._1, List(xs))
        }
      }
      case x => Field(data._1, List(x))
    }
  }

  def createSingle(data: (String, Any)): Field = {
    data._2 match {
      case Some(x) => Field(Symbol(data._1), List(x))
      case None => Field(Symbol(data._1), Nil)
      case xs: Seq[_] => {
        if (_is_seq_seq_tuple2(xs)) {
          val rs = _seq_seq_tuple2_to_records(xs)
          Field(Symbol(data._1), List(rs))
        } else if (_is_seq_tuple2(xs)) {
          val r = _seq_tuple2_to_record(xs)
          Field(Symbol(data._1), List(r))
        } else {
          Field(Symbol(data._1), List(xs))
        }
      }
      case x => Field(Symbol(data._1), List(x))
    }
  }

  private def _seq_seq_tuple2_to_records(xs: Seq[_]): Seq[Record] = {
    xs.map(x => _seq_tuple2_to_record(x.asInstanceOf[Seq[_]]))
  }

  private def _seq_tuple2_to_record(xs: Seq[_]): Record = {
    val fs = xs collect {
      case Tuple2(k, v) => createSingle(k.toString, v)
    }
    Record(fs.toList)
  }

  private def _is_seq_seq(xs: Seq[_]) = {
    xs.forall(_.isInstanceOf[Seq[_]])
  }

  private def _is_seq_tuple2(xs: Seq[_]) = {
    xs.forall(_.isInstanceOf[Tuple2[_, _]])
  }

  private def _is_seq_seq_tuple2(xs: Seq[_]) = {
    xs forall {
      case s: Seq[_] => _is_seq_tuple2(s)
      case _ => false
    }
  }

  def create(data: Seq[(String, Any)]): List[Field] = {
    data.map(create).toList
  }

  def createSingle(data: Seq[(String, Any)]): List[Field] = {
    data.map(createSingle).toList
  }
}
