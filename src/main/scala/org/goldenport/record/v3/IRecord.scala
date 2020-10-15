package org.goldenport.record.v3

import java.util.Date
import java.net.{URL, URI}
import java.sql.Timestamp
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.util.ListUtils
import org.goldenport.util.StringUtils
import org.goldenport.record.v2.{Schema, Column, DataType, Multiplicity}
import org.goldenport.record.v2.{MZeroOne, MZeroMore}
import org.goldenport.record.util.AnyUtils

/*
 * @since   Aug. 23, 2018
 *  version Sep. 20, 2018
 *  version Oct. 30, 2018
 *  version Nov.  7, 2018
 *  version Jan.  6, 2019
 *  version Feb. 28, 2019
 *  version Apr. 29, 2019
 *  version Jul. 29, 2019
 *  version Aug. 22, 2019
 *  version Nov. 29, 2019
 *  version Jan.  9, 2020
 * @version Oct. 15, 2020
 * @author  ASAMI, Tomoharu
 */
trait IRecord extends org.goldenport.record.IRecord
    with org.w3c.dom.Element with DomPart {
  def parent: Option[IRecord] = None
  def toRecord: Record
  def toMap: Map[String, Any] = toRecord.toMap // XXX
  def getSchema: Option[Schema]
  def fields: Seq[Field]
  def isEmpty: Boolean
  def isDefined(key: Symbol): Boolean
  def isDefined(key: String): Boolean
  def length: Int = fields.length
  def get(key: Symbol): Option[Any]
  def get(key: String): Option[Any]
  def getField(key: Symbol): Option[Field] = fields.find(_.key == key)
  def getField(key: String): Option[Field] = fields.find(_.name == key)
  def getList(key: Symbol): Option[List[Any]]
  def getList(key: String): Option[List[Any]]
  def takeList(key: Symbol): List[Any] = getList(key) getOrElse Nil
  def takeList(key: String): List[Any] = getList(key) getOrElse Nil
  def getString(key: Symbol): Option[String] = get(key).map(AnyUtils.toString)
  def getString(key: String): Option[String] = get(key).map(AnyUtils.toString)
  def getStringList(key: Symbol): Option[List[String]] = getList(key).map(_.map(AnyUtils.toString))
  def getStringList(key: String): Option[List[String]] = getList(key).map(_.map(AnyUtils.toString))
  def takeStringList(key: Symbol): List[String] = takeList(key).map(AnyUtils.toString)
  def takeStringList(key: String): List[String] = takeList(key).map(AnyUtils.toString)
  def getEagerTokenList(key: Symbol): Option[List[String]] = getList(key).map(_eager_tokens_list)
  def getEagerTokenList(key: String): Option[List[String]] = getList(key).map(_eager_tokens_list)
  def takeEagerTokenList(key: Symbol): List[String] = takeList(key).flatMap(_eager_tokens)
  def takeEagerTokenList(key: String): List[String] = takeList(key).flatMap(_eager_tokens)
  def getBoolean(key: Symbol): Option[Boolean] = get(key).map(AnyUtils.toBoolean)
  def getBoolean(key: String): Option[Boolean] = get(key).map(AnyUtils.toBoolean)
  def getByte(key: Symbol): Option[Byte] = get(key).map(AnyUtils.toByte)
  def getByte(key: String): Option[Byte] = get(key).map(AnyUtils.toByte)
  def getShort(key: Symbol): Option[Short] = get(key).map(AnyUtils.toShort)
  def getShort(key: String): Option[Short] = get(key).map(AnyUtils.toShort)
  def getInt(key: Symbol): Option[Int] = get(key).map(AnyUtils.toInt)
  def getInt(key: String): Option[Int] = get(key).map(AnyUtils.toInt)
  def getLong(key: Symbol): Option[Long] = get(key).map(AnyUtils.toLong)
  def getLong(key: String): Option[Long] = get(key).map(AnyUtils.toLong)
  def getFloat(key: Symbol): Option[Float] = get(key).map(AnyUtils.toFloat)
  def getFloat(key: String): Option[Float] = get(key).map(AnyUtils.toFloat)
  def getDouble(key: Symbol): Option[Double] = get(key).map(AnyUtils.toDouble)
  def getDouble(key: String): Option[Double] = get(key).map(AnyUtils.toDouble)
  def getTimestamp(key: Symbol): Option[Timestamp] = get(key).map(AnyUtils.toTimestamp)
  def getTimestamp(key: String): Option[Timestamp] = get(key).map(AnyUtils.toTimestamp)
  def getDate(key: Symbol): Option[Date] = get(key).map(AnyUtils.toDate)
  def getDate(key: String): Option[Date] = get(key).map(AnyUtils.toDate)
  def getUrl(key: Symbol): Option[URL] = get(key).map(AnyUtils.toUrl)
  def getUrl(key: String): Option[URL] = get(key).map(AnyUtils.toUrl)
  def getUri(key: Symbol): Option[URI] = get(key).map(AnyUtils.toUri)
  def getUri(key: String): Option[URI] = get(key).map(AnyUtils.toUri)
  def getIRecord(key: Symbol): Option[IRecord] = getRecord(key)
  def getIRecord(key: String): Option[IRecord] = getRecord(key)
  def getRecord(key: Symbol): Option[Record]
  def getRecord(key: String): Option[Record]
  def takeRecordList(key: Symbol): List[Record]
  def takeRecordList(key: String): List[Record]
  def +(rhs: IRecord): IRecord = update(rhs)
  def update(p: IRecord): IRecord
  def complement(p: IRecord): IRecord

  def asNameStringVector: Vector[(String, String)] = RAISE.notImplementedYetDefect
  def asSymbolAnyVector: Vector[(Symbol, Any)] = RAISE.notImplementedYetDefect
  def asJson: JsObject = RAISE.notImplementedYetDefect

  // compatibility
  // def getOne(key: String): Option[Any] = RAISE.unsupportedOperationFault
  // def getOne(key: Symbol): Option[Any] = RAISE.unsupportedOperationFault
  // def getFormOne(key: String): Option[Any] = RAISE.unsupportedOperationFault
  // def getFormOne(key: Symbol): Option[Any] = RAISE.unsupportedOperationFault
  // def getConcreteString(key: String): Option[String] = RAISE.unsupportedOperationFault
  // def getConcreteString(key: Symbol): Option[String] = RAISE.unsupportedOperationFault
  // def getFormString(key: String): Option[String] = RAISE.unsupportedOperationFault
  // def getFormString(key: Symbol): Option[String] = RAISE.unsupportedOperationFault
  // def getFormTimestamp(key: String): Option[Timestamp] = RAISE.unsupportedOperationFault
  // def getFormTimestamp(key: Symbol): Option[Timestamp] = RAISE.unsupportedOperationFault
  // def getFormDate(key: String): Option[Date] = RAISE.unsupportedOperationFault
  // def getFormDate(key: Symbol): Option[Date] = RAISE.unsupportedOperationFault
  // def eagerStringList(key: String): List[String] = RAISE.unsupportedOperationFault
  // def effectiveList(key: String): List[Any] = RAISE.unsupportedOperationFault
  // def toStringVector: Vector[(String, String)] = RAISE.unsupportedOperationFault

  private def _eager_tokens_list(ps: List[Any]): List[String] =
    StringUtils.eagerCommaForm(ps.map(AnyUtils.toString))

  private def _eager_tokens(p: Any): List[String] =
    StringUtils.eagerCommaForm(AnyUtils.toString(p))
}

object IRecord {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import org.goldenport.json.JsonUtils.Implicits._
  import org.goldenport.record.v2.util.RecordUtils

  final val DEFAULT_TAG_NAME = "record"

  implicit object IRecordFormat extends Format[IRecord] {
    def reads(json: JsValue): JsResult[IRecord] = json match {
      case m: JsObject => JsSuccess(Record.create(m))
      case _ => JsError(s"Invalid Record($json)")
    }
    def writes(o: IRecord): JsValue = o.asJson
  }

  // def +(lhs: IRecord, rhs: IRecord): IRecord = lhs + rhs

  case class Slot(column: Column, lefts: List[String], rights: List[String]) {
    def isMatch(p: Slot): Boolean = isMatch(p.column.name)
    def isMatch(p: String): Boolean = column.name == p
  }

  def makeSchema(p: IRecord, ps: IRecord*): Schema = makeSchema(p +: ps)

  def makeSchema(p: RecordSequence): Schema = makeSchema(p.irecords)

  // See org.goldenport.record.v2.util.RecordUtils.buildSchema
  def makeSchema(ps: Seq[IRecord]): Schema = {
    case class Z(columns: List[Slot] = Nil) {
      // TODO MOne, MOneMore
      def r = Schema(columns.map(_.column))
      def +(rhs: IRecord): Z = {
        case class ZZ(r: List[Slot] = Nil) {
          def +(f: Field): ZZ = {
            val slot = _to_slot(rhs, f)
            val (ls, x, rs) = _split(r, slot)
            ZZ(ls ++ List(_merge(x, slot)) ++ rs)
          }
        }
        Z(rhs.fields./:(ZZ(columns))(_+_).r)
      }
    }
    ps./:(Z())(_+_).r
  }

  private def _to_slot(rec: IRecord, f: Field): Slot =
    ListUtils.split3((_: Field) == f)(rec.fields) match {
      case (ls, Nil, rs) => Slot(_to_column(f), ls.map(_.key.name), rs.map(_.key.name))
      case (ls, cs, rs) => Slot(_to_column(cs.head), ls.map(_.key.name), rs.map(_.key.name))
    }

  private def _to_column(f: Field): Column = {
    val datatype = DataType.guessSeq(f.value.asVector)
    val multiplicity = f.value match {
      case EmptyValue => MZeroOne
      case m: SingleValue => MZeroOne
      case m: MultipleValue => MZeroMore
    }
    Column(f.key.name, datatype, multiplicity)
  }

  private def _split(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    ListUtils.split3((_: Slot).isMatch(s))(ss) match {
      case (ls, Nil, rs) => _split_guess(ss, s)
      case (ls, cs, rs) => (ls, cs.head, rs)
    }

  private def _split_guess(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    s.lefts.reverse match {
      case Nil => _split_guess2(ss, s)
      case x :: xs => _split_guess_left(ss, s, x, xs)
    }

  private def _split_guess2(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    s.rights match {
      case Nil => (Nil, s, Nil)
      case x :: xs => _split_guess_right(ss, s, x, xs)
    }

  private def _is_slot(name: String)(slot: Slot): Boolean = slot.isMatch(name)

  private def _split_guess_left(ss: List[Slot], s: Slot, name: String, candidates: List[String]): (List[Slot], Slot, List[Slot]) =
    ListUtils.split3(_is_slot(name))(ss) match {
      case (Nil, Nil, _) => (Nil, s, Nil)
      case (ls, Nil, _) => candidates match {
        case Nil => _split_guess2(ss, s)
        case x :: xs => _split_guess_left(ss, s, x, xs)
      }
      case (ls, cs, rs) => (ls ++ cs, s, rs)
    }
    // ss.toList.span(_.isMatch(name)) match {
    //   case (Nil, Nil) => (Nil, s, Nil)
    //   case (Nil, rs) => (List(rs.head), s, rs.tail)
    //   case (ls, Nil) => candidates match {
    //     case Nil => _split_guess2(ss, s)
    //     case x :: xs => _split_guess_left(ss, s, x, xs)
    //   }
    //   case (ls, rs) => (ls :+ rs.head, s, rs.tail)
    // }

  private def _split_guess_right(ss: List[Slot], s: Slot, name: String, candidates: List[String]): (List[Slot], Slot, List[Slot]) =
    ListUtils.split3(_is_slot(name))(ss) match {
      case (Nil, Nil, _) => (Nil, s, Nil)
      case (ls, Nil, _) => candidates match {
        case Nil => (ls, s, Nil)
        case x :: xs => _split_guess_right(ss, s, x, xs)
      }
      case (ls, cs, rs) => (ls, s, cs ++ rs)
    }
    // ss.toList.span(_.isMatch(name)) match {
    //   case (Nil, Nil) => (Nil, s, Nil)
    //   case (Nil, rs) => (Nil, s, rs)
    //   case (ls, Nil) => candidates match {
    //     case Nil => (ls, s, Nil)
    //     case x :: xs => _split_guess_right(ss, s, x, xs)
    //   }
    //   case (ls, rs) => (ls, s, rs)
    // }

  private def _merge(z: Slot, n: Slot): Slot = {
    case class Z(xs: Vector[String]) {
      def r = xs.toList
      def +(rhs: String) = if (r.contains(rhs)) this else Z(xs :+ rhs)
    }
    val l = n.lefts./:(Z(z.lefts.toVector))(_+_).r
    val r = n.rights./:(Z(z.rights.toVector))(_+_).r
    Slot(n.column, l, r)
  }
}
