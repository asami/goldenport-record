package org.goldenport.record.v2.util

import scalaz._, Scalaz._
import org.goldenport.Strings
import org.goldenport.json.JsonUtils
import org.goldenport.util.{StringUtils, AnyUtils}
import org.goldenport.record.v2._
import org.goldenport.record.command.{NullValue => CNullValue, NotExist => CNotExist}

/*
 * @since   Aug.  5, 2014
 *  version Dec. 28, 2014
 *  version Jan.  1, 2015
 *  version Apr.  9, 2015
 *  version Sep. 28, 2015
 *  version Oct. 25, 2015
 *  version Nov. 27, 2015
 *  version Feb. 26, 2016
 *  version Mar. 11, 2016
 *  version Apr. 25, 2016
 *  version Aug. 31, 2016
 *  version Sep.  5, 2016
 *  version Oct. 18, 2016
 *  version Jul. 11, 2017
 *  version Aug. 30, 2017
 *  version Sep.  2, 2017
 * @version Mar. 26, 2020
 * @author  ASAMI, Tomoharu
 */
object RecordAux {
  private val _datatypes: Vector[(DataType, Option[Any])] = Vector(
    (XBoolean, Some(false)),
    (XByte, Some(0: Byte)),
    (XShort, Some(0: Short)),
    (XInt, Some(0)),
    (XLong, Some(0L)),
    (XFloat, Some(0.0f)),
    (XFloat1, Some(0.0f)),
    (XDouble, Some(0.0)),
    (XInteger, Some(new BigInt(java.math.BigInteger.ZERO))),
    (XDecimal, Some(new BigDecimal(java.math.BigDecimal.ZERO))),
    (XString, Some("")),
    (XToken, Some("")),
    (XDate, None),
    (XTime, None),
    (XDateTime, None),
    (XText, Some("")),
    (XLink, None),
    (XEMail, None),
    (XMoney, None),
    (XPercent, None),
    (XUnit, None),
    (XUuid, None),
    (XEverforthid, None),
    (XXml, None),
    (XHtml, None)
  )

  val datatypes = _datatypes.map(_._1)

  /*
   * XEntityReference
   * XValue
   * XEverforthObjectReference
   * XPowertype
   * XPowertypeReference
   * XStateMachine
   * XStateMachineReference
   * XExternalDataType
   */

  def getDataType(name: String): Option[DataType] = {
    datatypes.find(_.name.equalsIgnoreCase(name))
  }

  def asDataType(name: String) = {
    getDataType(name) getOrElse {
      throw new IllegalArgumentException(s"Illegal datatype $name")
    }
  }

  // TODO define in Column
  def getDefaultValue(column: Column): Option[Any] = {
    getDefaultValue(column.datatype)
  }

  // TODO define in DataType
  def getDefaultValue(datatype: DataType): Option[Any] = {
    _datatypes.find(_._1 == datatype).flatMap(_._2)
  }

  // Use one in Record
  def getValue(column: Column, rec: Record): Option[List[Any]] = {
    if (column.isSingle)
      rec.getOne(column.name).map(x => List(column.datatype.toInstance(x)))
    else
      rec.get(column.name).map(_.flatMap {
        case xs: Seq[_] => xs.map(column.datatype.toInstance(_)) // dataApp
        case xs: Array[_] => xs.map(column.datatype.toInstance(_)) // dataApp
        case x => List(column.datatype.toInstance(x))
      })
  }

  /*
   * LTSV
   * TODO Migrate to v3.Record.
   */
  def toLtsv(rec: Record): String = {
    rec.fields.map(x => x.key.name + ":" + to_value(x.values)).mkString("\t")
  }

  def toLtsvPart(rec: Record): String = {
    rec.fields.map(x => "\t" + x.key.name + ":" + to_value(x.values)).mkString
  }

  def fromLtsv(ltsv: String): Record = {
    Record.create(StringUtils.ltsv2seq(ltsv))
  }

  def fromLtsv(ltsv: Option[String]): Record = {
    ltsv.map(fromLtsv).getOrElse(Record.empty)
  }

  protected def to_value(vs: List[Any]) =
    vs.map(x => if (x == null) "null" else x.toString).mkString(",")

  /*
   * Json
   */
  def toJsonString(rec: Record): String = {
    val buf = new StringBuilder
    buildJsonString(rec, buf)
    buf.toString
  }

  // TODO string, number handling
  def toJsonString(schema: Schema, rec: Record): String = {
    ???
  }

  def buildJsonString(rec: Record, buf: StringBuilder) {
    def buildfield(kv: (String, Any)) {
      buf.append("\"")
      buf.append(kv._1)
      buf.append("\":")
      JsonUtils.data2json(buf, kv._2)
    }

    def availablep(f: (String, Any)) = {
      val v = f._2
      v != null && (v match {
        case Nil => false
        case x :: Nil => x match {
          case Nil => false
          case _ => true
        }
        case _ => true
      })
    }

    buf.append("{")
    val fs = _key_values(rec.fields).filter(availablep)
    if (fs.nonEmpty) {
      buildfield(fs.head)
      for (f <- fs.tail) {
        buf.append(",")
        buildfield(f)
      }
    }
    buf.append("}")
  }

  // Defined as Record#KeyStringValues.
  private def _key_values(fs: Seq[Field]): Seq[(String, Any)] = {
    fs.flatMap(_key_value)
  }

  private def _key_value(f: Field): Option[(String, Any)] = {
    val data = f.values match {
      case Nil => None
      case x :: Nil => x
      case xs => xs
    }
    data match {
      case None => None
      case _ => Some(f.key.name -> data)
    }
  }

  def getEffectiveStringList(
    key: Symbol
  )(rec: Record): List[String] = {
    rec.getFormStringList(key).flatMap(Strings.totokens(_))
  }

  // get means using Option
  // # origid means using Option (obsolated)
  // effective means handling effective list structure
  // form means handling empty html form is no parameter
  // eager means handling eager delimiters separated list
  // comma means handling comma delimiter separated list
  def getRigidEffectiveList(rec: Record, key: Symbol): Option[List[Any]] = {
    rec.get(key).map(effectiveList)
  }

  def getRigidEffectiveStringList(rec: Record, key: Symbol): Option[List[String]] =
    getRigidEffectiveList(rec, key).map(_.map(AnyUtils.toString))

  def getRigidEagerStringList(rec: Record, key: Symbol): Option[List[String]] =
    getRigidEffectiveStringList(rec, key).map(eagerStringList)

  def effectiveList(field: Field): List[Any] = effectiveList(field.values)

  def effectiveStringList(field: Field): List[String] =
    effectiveList(field).map(AnyUtils.toString)

  def effectiveList(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case List(Nil) => Nil
    case List(xs: List[_]) => xs // special syntax
    case xs => xs
  }

  def eagerStringList(xs: List[String]): List[String] =
    xs.flatMap(Strings.totokens).filter(Strings.notblankp)

  def eagerStringList(field: Field): List[String] =
    eagerStringList(effectiveStringList(field))

  def getEffectiveEagerFormStringList(f: Field): Option[List[String]] = {
    val delimiter = ","
    val a: List[String] = f.effectiveList.flatMap {
      case m: String => Strings.totokens(m, delimiter)
      case m => List(AnyUtils.toString(m))
    }
    a.filter(Strings.notblankp) match {
      case Nil => None
      case xs => Some(xs)
    }
  }

  def effectiveCommaFormStringList(rec: Record, key: Symbol): List[String] =
    rec.getField(key).fold(List.empty[String])(effectiveCommaFormStringList)

  def effectiveCommaFormStringList(rec: Record, key: String): List[String] =
    effectiveCommaFormStringList(rec, Symbol(key))

  def effectiveCommaFormStringList(f: Field): List[String] = {
    val delimiter = ","
    val a: List[String] = f.effectiveList.flatMap {
      case m: String => Strings.totokens(m, delimiter)
      case m => List(AnyUtils.toString(m))
    }
    a.filter(Strings.notblankp)
  }

  def getEffectiveCommaFormStringList(rec: Record, key: Symbol): Option[List[String]] =
    rec.getField(key).flatMap { f =>
      effectiveCommaFormStringList(f).filter(Strings.notblankp) match {
        case Nil => None
        case xs => Some(xs)
      }
    }

  def getEffectiveCommaFormStringList(rec: Record, key: String): Option[List[String]] =
    getEffectiveCommaFormStringList(rec, Symbol(key))

  /*
   * Form
   */
  def normalizeForm(schema: Schema)(rec: Record): Record =
    rec.copy(fields = rec.fields.filterNot(isEmptyForm(schema)))

  def isEmptyForm(schema: Schema)(field: Field): Boolean = {
    field.values match {
      case null => true
      case Nil => true
      case x :: Nil => x match {
        case NotExist => true
        case NullValue => true
        case CNotExist => true
        case CNullValue => true
        case m: Seq[_] => false
        case m: String if m.trim.isEmpty => !isStringProperty(schema, field.key)
        case _ => false
      }
      case x :: xs => false
    }
  }

  def isStringProperty(schema: Schema, column: Symbol): Boolean =
    schema.getColumn(column).fold(false)(_.datatype match {
      case XString => true
      case XToken => true
      case XText => true
      case _ => false
    })

  /*
   * Command
   *
   * Use org.goldenport.record.command.ValueCommand instead.
   */
  sealed trait FieldCommand
  case object NotExist extends FieldCommand
  case object NullValue extends FieldCommand
  type TypeC[T] = \/[FieldCommand, T]

  def NotExistC[T]: TypeC[T] = -\/(NotExist)

  implicit class TypeCValue[T](val self: TypeC[T]) extends AnyVal {
    def mapWithNull[A](f: T => A)(nullf: => A): Option[A] =
      RecordAux.this.mapWithNull(self)(f)(nullf)
  }

  def mapWithNull[T, A](t: TypeC[T])(f: T => A)(nullf: => A): Option[A] = {
    t match {
      case -\/(l) => l match {
        case NotExist => None
        case NullValue => Some(nullf)
      }
      case \/-(r) => Some(f(r))
    }
  }

  def getStringC(rec: Record, name: Symbol): TypeC[String] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: FieldCommand => -\/(c)
        case s: String => \/-(s)
        case _ => \/-(rec.asString(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getStringFormC(rec: Record, name: Symbol): TypeC[String] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: FieldCommand => -\/(c)
        case "" => -\/(NotExist)
        case s: String => \/-(s)
        case _ => \/-(rec.asString(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getBooleanC(rec: Record, name: Symbol): TypeC[Boolean] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: FieldCommand => -\/(c)
        case s: Boolean => \/-(s)
        case _ => \/-(rec.asBoolean(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getIntC(rec: Record, name: Symbol): TypeC[Int] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: FieldCommand => -\/(c)
        case s: Int => \/-(s)
        case _ => \/-(rec.asInt(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getIntFormC(rec: Record, name: Symbol): TypeC[Int] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: FieldCommand => -\/(c)
        case s: Int => \/-(s)
        case "" => -\/(NotExist)
        case _ => \/-(rec.asInt(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getBooleanC(rec: Record, name: String): TypeC[Boolean] = {
    getBooleanC(rec, Symbol(name))
  }

  def getIntC(rec: Record, name: String): TypeC[Int] = {
    getIntC(rec, Symbol(name))
  }

  def getStringC(rec: Record, name: String): TypeC[String] = {
    getStringC(rec, Symbol(name))
  }
}
