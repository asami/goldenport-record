package org.goldenport.record

import scalaz._
import Scalaz._
import scala.collection.mutable.ArrayBuffer
import java.io.InputStream
import java.sql.ResultSet
import org.goldenport.record.sql.SqlDatatype
import org.smartdox._

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Aug. 12, 2010
 *  version Jul.  3, 2011
 *  version Dec.  4, 2011
 * @version Feb. 29, 2012
 * @author  ASAMI, Tomoharu
 */
object Schema {
  def apply(fields: RecordField*) = {
    new RecordSchema(fields: _*)
  }
}

trait UseRecord {
  implicit def symbol2Field(name: Symbol): RecordField = {
    new RecordField(name.name)
  }

  implicit def sql2Property(datatype: SqlDatatype): Property = {
    Property("sql", datatype)
  }

  implicit def tuple2Field(tuple: Tuple1[String]): RecordField = {
    Field(make_field_elements(tuple))
  }

  implicit def tuple2Field(tuple: Tuple2[String, XDatatype]): RecordField = {
    Field(make_field_elements(tuple))
  }

  implicit def tuple2Field(tuple: Tuple3[String, XDatatype, RecordSchemaElement]): RecordField = {
    Field(make_field_elements(tuple))
  }

  implicit def tuple2Field(tuple: Tuple4[String, XDatatype, RecordSchemaElement, RecordSchemaElement]): RecordField = {
    Field(make_field_elements(tuple))
  }

  implicit def tuple2Field(tuple: Tuple5[String, XDatatype, RecordSchemaElement, RecordSchemaElement, RecordSchemaElement]): RecordField = {
    Field(make_field_elements(tuple))
  }

  implicit def tuple2Field(tuple: Tuple6[String, XDatatype, Multiplicity, RecordSchemaElement, RecordSchemaElement, RecordSchemaElement]): RecordField = {
    Field(make_field_elements(tuple))
  }

  private def make_field_elements(tuple: Product): (String, XDatatype, Multiplicity, List[Constraint], List[XFacet], List[Property]) = {
    val names = new ArrayBuffer[String]
    val datatypes = new ArrayBuffer[XDatatype]
    val multiplicities = new ArrayBuffer[Multiplicity]
    val constraints = new ArrayBuffer[Constraint]
    val facets = new ArrayBuffer[XFacet]
    val properties = new ArrayBuffer[Property]

    def make_datatype = {
      if (datatypes.isEmpty) XString
      else datatypes(0)
    }

    def make_multiplicity = {
      if (multiplicities.isEmpty) MOne
      else multiplicities(0)
    }

    tuple.productIterator.foreach {
      case name: String => names += name
      case datatype: XDatatype => datatypes += datatype
      case multiplicity: Multiplicity => multiplicities += multiplicity
      case constraint: Constraint => constraints += constraint
      case facet: XFacet => facets += facet
      case property: Property => properties += property
    }
    if (names.length != 1 || datatypes.length > 1 || multiplicities.length > 1) {
      throw new IllegalArgumentException("XXX")
    }
    (names(0), make_datatype, make_multiplicity, constraints.toList, facets.toList, properties.toList)
  }

/*
  implicit def symbol2qsymbol(atom: Symbol): QSymbol = QSymbol(atom)

  implicit def string2qsymbol(atom: String): QSymbol = QSymbol(atom)

  implicit val recordResulter: PartialFunction[AnyRef, Record] = {
    case r: Record => r
    case rs: RecordSet => rs.head
  }

  // XXX
  implicit val stringResulter: PartialFunction[AnyRef, String] = {
    case s: String => s
  }
*/
}

object UseRecord extends UseRecord

object Field {
  def apply(name: String, datatype: XDatatype = XString, // XInt
            multiplicity: Multiplicity = MOne,
            constraints: List[Constraint] = Nil,
            facets: List[XFacet] = Nil,
            properties: List[Property] = Nil,
            title: Dox = EmptyDox,
            summary: Dox = EmptyDox,
            content: Dox = EmptyDox) = {
    new RecordField(name, datatype, multiplicity, constraints, facets, properties,
        Some(Description(name, title, summary, content)))
  }

  def apply(tuple: Tuple6[String, XDatatype, Multiplicity, List[Constraint], List[XFacet], List[Property]]) = {
    new RecordField(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
  }

  def unapply(field: RecordField): Option[(String, XDatatype, Multiplicity, List[Constraint], List[XFacet], List[Property])] = {
    Some((field.name, field.datatype, field.multiplicity,
          field.constraints, field.facets, field.properties))
  }
}

object IdField extends RecordField("id", XLong, MOne, List(CId), Nil) {
  def apply(name: String = "id", datatype: XDatatype = XLong) = {
    new RecordField(name, datatype, MOne, List(CId), Nil)
  }
}

object AutoIdField extends RecordField("id", XLong, MOne, List(CId, CAutoId), Nil) {
  def apply(name: String = "id", datatype: XDatatype = XLong) = {
    new RecordField(name, datatype, MOne, List(CId, CAutoId), Nil)
  }
}

object DateField extends RecordField("date", XDate, MOne, List(CAutoDateTime), Nil) {
  def apply(name: String = "date") = {
    new RecordField(name, XDate, MOne, List(CAutoDateTime), Nil)
  }
}

class RecordSchema(val fields: RecordField*) {
  def getField(name: String): Option[RecordField] = {
    fields.find(_.name == name)
  }

  def findDataType(name: String): Option[XDatatype] = {
   fields.find(_.name == name).map(_.datatype)
  }

  def idName = {
    fields.find(_.isId).get.name
  }

  def idField = {
    fields.find(_.isId).get
  }

  def validate(r: Record, ctx: RecordContext): Record = {
    val validated = for (i <- 0 until fields.length) yield {
      val f = fields(i)
      r.get(f.name) match {
        case Some(d) => {
          println("date for normalize = " + d)
          val data = f.normalize(d, ctx) match {
            case Right(dd) => f.validate(dd, ctx) match {
              case Success(_) => dd
              case Failure(e) => e.head
            }
            case Left(e) => e
          }
          println("data in validated = " + data)
          Option(f.name, data)
        }
        case None => {
          if (f.isAuto) {
            None
          } else if (f.multiplicity != MZeroOne && f.multiplicity != MZeroMore) {
            Option(f.name, new MissingFieldRecordFieldException)
          } else {
            None
          }
        }
      }
    }

    println("record before flattern = " + validated)
    val result = new Record(validated.flatten)
    result.schema = Some(this)
    result
  }
}

class RecordField(
  val name: String,
  val datatype: XDatatype = XString,
  val multiplicity: Multiplicity = MOne,
  val constraints: List[Constraint] = Nil,
  val facets: List[XFacet] = Nil,
  val properties: List[Property] = Nil,
  val description: Option[Description] = None
) {
  def isId = constraints.contains(CId)
  def isAuto = constraints.exists(_.isAuto)

  def normalize(data: AnyRef, ctx: RecordContext): Either[RecordFieldException, AnyRef] = {
    def adjusted_data = {
      if (multiplicity.isMulti) {
        data match {
          case s: String => {
            if (s.startsWith("[")) { // JSON
              if (s.endsWith("]")) {
                s.substring(1, s.length - 1).split(",").toList
              } else {
                s.substring(1).split(",").toList
              }
            } else if (s.startsWith("<")) { // XML
              data
            } else if (s.indexOf(",") != -1) { // CSV
              s.split(",").toList
            } else {
              data
            }
          }
          case _ => data
        }
      } else {
        data
      }
    }

    try {
      adjusted_data match {
        case col: Iterable[AnyRef] => {
          val r = col.map(v => datatype.normalize(v, ctx))
          r.find(_.isLeft) match {
            case Some(left) => left
            case None => Right(r.map(_.right.get))
          }
        }
        case _ => datatype.normalize(data, ctx) 
      }
    } catch {
      case e: Exception => Left(new IllegalFormatRecordFieldException(e))
    }
  }

  def validate[T <: AnyRef](data: T, ctx: RecordContext): ValidationNEL[RecordFieldException, T] = {
    def fv(v: T): ValidationNEL[RecordFieldException, T] = {
      facets.flatMap(_.validateO(v, ctx)) match {
        case Nil => data.success
        case e :: es => nel(e, es).fail
      }
    }
    def zv(v: T) = v match {
      case null => true
      case None => true
      case Nil => true
      case seq: Seq[AnyRef] => seq.isEmpty 
      case _ => false
    }
    def mv(v: T) = {
      v match {
        case col: Iterable[AnyRef] => col.foldLeft(
          data.success : ValidationNEL[RecordFieldException, T]) {
          case (a, e) => {
            val x = fv(e.asInstanceOf[T])
            x <+> a // XXX
          }
        }
        case _ => fv(data)
      }      
    }

    multiplicity match {
      case MOne => if (zv(data)) {
        new MultiplicityRecordFieldException("One").failNel
      } else {
        fv(data) 
      }
      case MZeroOne => if (zv(data)) data.success else fv(data)
      case MOneMore => if (zv(data)) {
        new MultiplicityRecordFieldException("OneMore").failNel
      } else mv(data)
      case MZeroMore => mv(data)
    }
  }
}

abstract class RecordSchemaElement

abstract class XDatatype() extends RecordSchemaElement {
  val name: String

  // XXX from SQL
  def normalize(value: AnyRef): Either[RecordFieldException, AnyRef] = Right(value)

  def normalize(value: AnyRef, ctx: RecordContext): Either[RecordFieldException, AnyRef] = {
    normalize(value)
  }

  protected final def normalize_2d(s: String) = {
    if (s == null) "00"
    else if (s.length == 1) "0" + s
    else s
  }
}

class XString extends XDatatype {
  val name = "string"
}

object XString extends XString {
  def apply() = this
}

class XBoolean extends XDatatype {
  val name = "boolean"
}

object XBoolean extends XBoolean {
  def apply() = this
}

class XDecimal extends XDatatype {
  val name = "decimal"
}

object XDecimal extends XDecimal {
  def apply() = this
}

class XFloat extends XDatatype {
  val name = "float"
}

object XFloat extends XFloat {
  def apply() = this

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Float => Right(n)
      case n: Number => Right(n.floatValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toFloat.asInstanceOf[AnyRef])
    }
  }
}

class XDouble extends XDatatype {
  val name = "double"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Double => Right(n)
      case n: Number => Right(n.doubleValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toDouble.asInstanceOf[AnyRef])
    }
  }
}

object XDouble extends XDouble {
  def apply() = this
}

class XDuration extends XDatatype {
  val name = "duration"
}

object XDuration extends XDuration {
  def apply() = this
}

class XDateTime extends XDatatype {
  val name = "dateTime"
}

object XDateTime extends XDateTime {
  def apply() = this
}

class XDate extends XDatatype {
  val name = "date"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    val ExtJS = """(\d+)/(\d+)/(\d\d\d\d)""".r
    value match {
      case ExtJS(m, d, y) => Right(
        "%s/%s/%s".format(normalize_2d(y),
                          normalize_2d(m),
                          normalize_2d(d)))
      case s: String => {
        val result = s.replace('/', '-')
        Right(result)
      }
      case d: java.util.Date => Right(d)
    }
  }
}

object XDate extends XDate {
  def apply() = this
}

class XTime extends XDatatype {
  val name = "time"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    def normalize_local_ss(h: String, m: String): String = {
      normalize_local_sss(h, m, "00")
    }

    def normalize_local_is(h: Int, m: String): String = {
      normalize_local_sss(h.toString, m, "00")
    }

    def normalize_local_sss(h: String, m: String, s: String): String = {
      normalize_local("%s:%s:%s".format(normalize_2d(h),
                                        normalize_2d(m),
                                        normalize_2d(s)))
    }

    def normalize_local(time: String): String = {
      ctx.localTime2NormalizedTime(time)
    }

    val ExtJS12AM = """12:(\d+) AM""".r
    val ExtJSAM = """(\d+):(\d+) AM""".r
    val ExtJS12PM = """(\d+):(\d\d) PM""".r
    val ExtJSPM = """12:(\d\d) PM""".r
    val WithoutTMZ = """(\d+):(\d+)(:(\d+))?""".r
    value match {
      case ExtJS12AM(m) => Right(normalize_local_ss("00", m))
      case ExtJSAM(h, m) => Right(normalize_local_ss(h, m))
      case ExtJS12PM(m) => Right(normalize_local_is(12, m))
      case ExtJSPM(h, m) => Right(normalize_local_is(h.toInt + 12, m))
      case WithoutTMZ(h, m, _, s) => Right(normalize_local_sss(h, m, s))
      case s: String => Right(s) // XXX
      case d: java.util.Date => Right(d)
    }
  }
}

object XTime extends XTime {
  def apply() = this
}

class XGYearMonth extends XDatatype {
  val name = "gYearMonth"
}

object XGYearMonth extends XGYearMonth {
  def apply() = this
}

class XGYear extends XDatatype {
  val name = "gYear"
}

object XGYear extends XGYear {
  def apply() = this
}

class XGMonthDay extends XDatatype {
  val name = "gMonthDay"
}

object XGMonthDay extends XGMonthDay {
  def apply() = this
}

class XGDay extends XDatatype {
  val name = "gDay"
}

object XGDay extends XGDay {
  def apply() = this
}

class XGMonth extends XDatatype {
  val name = "gMonth"
}

object XGMonth extends XGMonth {
  def apply() = this
}

class XHexBinary extends XDatatype {
  val name = "hexBinary"
}

object XHexBinary extends XHexBinary {
  def apply() = this
}

class XBase64Binary extends XDatatype {
  val name = "base64Binary"
}

object XBase64Binary extends XBase64Binary {
  def apply() = this
}

class XAnyURI extends XDatatype {
  val name = "anyURI"
}

object XAnyURI extends XAnyURI {
  def apply() = this
}

class XToken extends XDatatype {
  val name = "token"
}

object XToken extends XToken {
  def apply() = this
}

class XLanguage extends XDatatype {
  val name = "language"
}

object XLanguage extends XLanguage {
  def apply() = this
}

class XID extends XDatatype {
  val name = "ID"
}

// XXX
object XID extends XID {
  def apply() = this
}

// XXX
class XIDREF extends XDatatype {
  val name = "IDREF"
}

// XXX
object XIDREF extends XIDREF {
  def apply() = this
}

// XXX
class XIDREFS extends XDatatype {
  val name = "IDREFS"
}

// XXX
object XIDREFS extends XIDREFS {
  def apply() = this
}

class XInteger extends XDatatype {
  val name = "integer"
}

object XInteger extends XInteger {
  def apply() = this
}

class XNonPositiveInteger extends XDatatype {
  val name = "nonPositiveInteger"
}

object XNonPositiveInteger extends XNonPositiveInteger {
  def apply() = this
}

class XNegativeInteger extends XDatatype {
  val name = "negativeInteger"
}

object XNegativeInteger extends XNegativeInteger {
  def apply() = this
}

class XLong extends XDatatype {
  val name = "long"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Long => Right(n)
      case n: Number => Right(n.longValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toLong.asInstanceOf[AnyRef])
    }
  }
}

object XLong extends XLong {
  def apply() = this
}

class XInt extends XDatatype {
  val name = "int"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Integer => Right(n)
      case n: Number => Right(n.intValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toInt.asInstanceOf[AnyRef])
    }
  }
}

object XInt extends XInt {
  def apply() = this
}

/*
        3.2.18 QName
        3.2.19 NOTATION
*/

class XShort extends XDatatype {
  val name = "short"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Short => Right(n)
      case n: Number => Right(n.shortValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toShort.asInstanceOf[AnyRef])
    }
  }
}

object XShort extends XShort {
  def apply() = this
}

class XByte extends XDatatype {
  val name = "byte"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Byte => Right(n)
      case n: Number => Right(n.byteValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toByte.asInstanceOf[AnyRef])
    }
  }
}

object XByte extends XByte {
  def apply() = this
}

class XNonNegativeInteger extends XDatatype {
  val name = "nonNegativeInteger"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Byte => Right(n)
      case n: Number => Right(n.byteValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toByte.asInstanceOf[AnyRef])
    }
  }
}

object XNonNegativeInteger extends XNonNegativeInteger {
  def apply() = this
}

class XUnsignedLong extends XDatatype {
  val name = "unsignedLong"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    value match {
      case n: java.lang.Byte => Right(n)
      case n: Number => Right(n.byteValue.asInstanceOf[AnyRef])
      case s: String => Right(s.toByte.asInstanceOf[AnyRef])
    }
  }
}

object XUnsignedLong extends XUnsignedLong {
  def apply() = this
}

class XUnsignedInt extends XDatatype {
  val name = "unsignedInt"

  override def normalize(value: AnyRef, ctx: RecordContext) = {
    def inrange(n: Long): Boolean = {
      println("max uint = " + Integer.MAX_VALUE * 2L)
      0 <= n && n <= Integer.MAX_VALUE * 2L // XXX
    }

    println("XUnsignedInt = " + value.getClass + "/" + value)
    value match {
      case n: java.lang.Byte => if (n.byteValue >= 0) Right(n)
                                else Left(new InvalidValueRecordFieldException("unsigned int"))
      case n: java.lang.Short => if (n.shortValue >= 0) Right(n)
                                 else Left(new InvalidValueRecordFieldException("unsigned int"))
      case n: java.lang.Integer => if (n.intValue >= 0) Right(n)
                               else Left(new InvalidValueRecordFieldException("unsigned int"))
      case n: java.lang.Long => {
        if (inrange(n.longValue)) Right(n)
        else Left(new InvalidValueRecordFieldException("unsigned long"))
      }
      case n: Number => if (inrange(n.longValue)) Right(n.asInstanceOf[AnyRef])
                        else Left(new InvalidValueRecordFieldException("unsigned number"))
      case s: String => {
        val n = s.toLong
        println("XUnsignedInt = " + n)
        if (inrange(n)) Right(n.asInstanceOf[AnyRef])
        else Left(new InvalidValueRecordFieldException("unsigned number"))
      }
    }
  }
}

object XUnsignedInt extends XUnsignedInt {
  def apply() = this
}

class XUnsignedShort extends XDatatype {
  val name = "unsignedShort"
}

object XUnsignedShort extends XUnsignedShort {
  def apply() = this
}

class XUnsignedByte extends XDatatype {
  val name = "unsignedByte"
}

object XUnsignedByte extends XUnsignedByte {
  def apply() = this
}

class XPositiveInteger extends XDatatype {
  val name = "positiveInteger"
}

object XPositiveInteger extends XPositiveInteger {
  def apply() = this
}

/*
     3.3.1 normalizedString
        3.3.4 NMTOKEN
        3.3.5 NMTOKENS
        3.3.6 Name
        3.3.11 ENTITY
        3.3.12 ENTITIES
        3.3.7 NCName
*/

// entity reference datatype

class XEntityReference(val uri: String, val datatype: XDatatype = XLong) extends XDatatype {
  val name = "reference(" + uri + ")"
}

object XEntityReference {
  def apply(uri: String) = new XEntityReference(uri)

  def apply(uri: String, dt: XDatatype) = new XEntityReference(uri, dt)
}

/*
 * Multiplicity
 */
abstract class Multiplicity() extends RecordSchemaElement {
  def isMulti: Boolean
}

object MOne extends Multiplicity {
  def isMulti = false
}
object MZeroOne extends Multiplicity {
  def isMulti = false
}
object MOneMore extends Multiplicity {
  def isMulti = true
}
object MZeroMore extends Multiplicity {
  def isMulti = true
}

/**
 * XML Datatype facet.
 */
abstract class XFacet extends RecordSchemaElement {
  val name: String

  protected final def invalid_value = {
    Some(new InvalidValueRecordFieldException(name))
  }

  protected final def condition(cond: Boolean) = {
    if (cond) None 
    else invalid_value
  }

  protected final def big_decimal(value: Number) = {
    value match {
      case n: java.lang.Byte => BigDecimal(n.intValue)
      case n: java.lang.Short => BigDecimal(n.intValue)
      case n: java.lang.Integer => BigDecimal(n.intValue)
      case n: java.lang.Long => BigDecimal(n.longValue)
      case n: java.lang.Float => BigDecimal(n.floatValue)
      case n: java.lang.Double => BigDecimal(n.doubleValue)
      case n: java.math.BigInteger => BigDecimal(n.toString)
      case n: java.math.BigDecimal => BigDecimal(n)
      case n: scala.math.BigInt => BigDecimal(n)
      case n: scala.math.BigDecimal => n
    }
  }

  def validate[T](data: T, ctx: RecordContext): ValidationNEL[RecordFieldException, T] = {
    validateO(data.asInstanceOf[AnyRef], ctx) match {
      case Some(e) => e.failNel
      case None => data.success
    }
  }

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException]
}

class XLength(val value: Int) extends XFacet {
  val name = "length"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    data match {
      case s: String => condition(s.length == value) // I18N
      case n: Number => condition(n.toString.length == value)
      case _ => invalid_value
    }
  }
}

object XLength {
  def apply(value: Int) = new XLength(value)
}

class XMinLength(value: Int) extends XFacet {
  val name = "minLength"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    data match {
      case s: String => condition(s.length >= value)
      case n: Number => condition(n.toString.length >= value)
      case _ => invalid_value
    }
  }
}

object XMinLength {
  def apply(value: Int) = new XMinLength(value)
}

class XMaxLength(value: Int) extends XFacet {
  val name = "maxLength"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    data match {
      case s: String => condition(s.length <= value)
      case n: Number => condition(n.toString.length <= value)
      case _ => invalid_value
    }
  }
}

object XMaxLength {
  def apply(value: Int) = new XMaxLength(value)
}

class XPattern extends XFacet {
  val name = "pattern"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    throw new UnsupportedOperationException
  }
}

class XEnumeration(val values: List[AnyRef]) extends XFacet {
  val name = "enumeration"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    if (values.exists(_ == data)) None
    else invalid_value
  }
}

object XEnumeration {
  def apply(values: Any*) = {
    new XEnumeration(values.map(_.asInstanceOf[AnyRef]).toList)
  }
}

class XWhiteSpace extends XFacet {
  val name = "whiteSpace"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    throw new UnsupportedOperationException
  }
}

class XMaxInclusive(value: Number) extends XFacet {
  val name = "maxInclusive"
  private val _value = big_decimal(value)

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    data match {
      case n: Number => condition(big_decimal(n) <= _value)
      case _ => invalid_value
    }
  }
}

object XMaxInclusive {
  def apply(value: Number) = new XMaxInclusive(value)
}

class XMaxExclusive(value: Number) extends XFacet {
  val name = "maxExclusive"
  private val _value = big_decimal(value)

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    println("data = " + data)
    println("data = " + data.getClass + "/" + data)
    data match {
      case n: Number => {
        println("in = " + big_decimal(n))
        println("value = " + _value)
        println("condition = " + (big_decimal(n) < _value))
        condition(big_decimal(n) < _value)
      }
      case _ => invalid_value
    }
  }
}

object XMaxExclusive {
  def apply(value: Number) = new XMaxExclusive(value)
}

class XMinInclusive(value: Number) extends XFacet {
  val name = "minInclusive"
  private val _value = big_decimal(value)

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    data match {
      case n: Number => condition(big_decimal(n) >= _value)
      case _ => invalid_value
    }
  }
}

object XMinInclusive {
  def apply(value: Number) = new XMinInclusive(value)
}

class XMinExclusive(value: Number) extends XFacet {
  val name = "minExclusive"
  private val _value = big_decimal(value)

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    data match {
      case n: Number => condition(big_decimal(n) > _value)
      case _ => invalid_value
    }
  }
}

object XMinExclusive {
  def apply(value: Number) = new XMinExclusive(value)
}

class XTodalDigits extends XFacet {
  val name = "todalDigits"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    throw new UnsupportedOperationException
  }
}

class XFractionDigits extends XFacet {
  val name = "fractionDigits"

  def validateO(data: AnyRef, ctx: RecordContext): Option[RecordFieldException] = {
    throw new UnsupportedOperationException
  }
}

/**
 * Constraint represents structual constraint in the schema.
 * Value constraints are represents by XFacet.
 */
abstract class Constraint extends RecordSchemaElement {
  def isAuto: Boolean = false
}

class CId extends Constraint
object CId extends CId

class CAutoId extends Constraint {
  override def isAuto = true
}
object CAutoId extends CAutoId

class CAutoDateTime extends Constraint {
  override def isAuto = true
}
object CAutoDateTime extends CAutoDateTime

class CUnique extends Constraint
object CUnique extends CUnique

class CPassword extends Constraint
object CPassword extends CPassword

/*
class CDerivation(val expression: Expression) extends Constraint

object CDerivation {
  def apply(expr: Expression) = new CDerivation(expr)

  def apply(entity: QSymbol, valueAttr: Symbol, keyAttr: Symbol, refAttr: Symbol): CDerivation = {
    val expr = EntityReferenceAttribute(entity, valueAttr, keyAttr, refAttr)
    apply(expr)
  }
}
*/
/*
class CValues(val values: List[Any]) extends Constraint

object CValues {
  def apply(values: List[Any]) = new CValues(values)

  def apply(value: Any) = new CValues(List(value))
}

class CLongRange(val min: Option[Long], val max: Option[Long]) extends Constraint

class CDoubleRange(val min: Option[Double], val max: Option[Double]) extends Constraint
*/

sealed abstract class LinkCacheConstraint extends Constraint

object CLinkCache extends LinkCacheConstraint
object CLinkNoCache extends LinkCacheConstraint

sealed abstract class LinkHoldConstraint extends Constraint

object CLinkHold extends LinkHoldConstraint
object CLinkQuery extends LinkHoldConstraint

//

sealed abstract class InstanceConstraint extends Constraint

object CLogicalUpdateDelete extends InstanceConstraint
object CPhysicalUpdateLogicalDelete extends InstanceConstraint
object CPhysicalUpdateDelete extends InstanceConstraint

/*
 *  Property
 */
class Property(val key: String, val value: Any) extends RecordSchemaElement {
}

object Property {
  def apply(key: String, value: Any) = {
    new Property(key, value)
  }

  def apply(datatype: SqlDatatype) = {
    new Property("sql", datatype)
  }
}

/*
 * Evaluation Context
 */
abstract class RecordContext {
  def localTime2NormalizedTime(time: String): String = time
  def getSqlData(rs: ResultSet, i: Int, columnType: Int) = sys.error("not implemented yet")
}

class PlainRecordContext extends RecordContext

object DefaultRecordContext extends PlainRecordContext
