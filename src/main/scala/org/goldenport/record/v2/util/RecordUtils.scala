package org.goldenport.record.v2.util

import scalaz._, Scalaz._
import scalax.io.Codec
import scala.util.Try
import scala.util.control.NonFatal
import scala.collection.JavaConverters._
import scala.collection.mutable
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.bag.BufferFileBag
import org.goldenport.json.JsonUtils
import org.goldenport.util.{AnyUtils, AnyRefUtils, StringUtils, SeqUtils}
import org.goldenport.record.v2._
import org.goldenport.record.v2.bag.{RecordBag, CsvBag}

/*
 * @since   Jun.  9, 2014
 *  version Dec. 10, 2014
 *  version Aug. 27, 2015
 *  version Sep. 26, 2015
 *  version Oct.  8, 2015
 *  version Dec.  4, 2015
 *  version Jun. 24, 2016
 *  version Apr.  2, 2017
 *  version Jul. 11, 2017
 *  version Aug. 30, 2017
 *  version Sep. 15, 2017
 * @version Oct. 22, 2017
 * @author  ASAMI, Tomoharu
 */
object RecordUtils {
  def build(map: Map[_, _]): Record = build(map.toVector)
  def build(p: Seq[(_, _)]): Record = {
    val r = p map {
      case (k, v) =>
        val k9 = k match {
          case m: Symbol => m
          case m: String => Symbol(m)
          case m => Symbol(m.toString)
        }
        val a9 = v match {
          case m: String => getRecordFromJsonString(m) getOrElse m
          case m: Map[_, _] => _get_map(m) getOrElse m
          case m: mutable.Map[_, _] => _get_map(m) getOrElse m
          case m: java.util.Map[_, _] => _get_java_map(m) getOrElse m
          case m => m
        }
        k9 -> a9
    }
    Record.createAppS(r)
  }

  private def _get_map(p: Map[_, _]): Option[Record] = Try {
    build(p)
  }.toOption

  private def _get_map(p: mutable.Map[_, _]): Option[Record] = Try {
    build(p.toVector)
  }.toOption

  private def _get_java_map(p: java.util.Map[_, _]): Option[Record] = Try {
    _get_map(p.asScala)
  }.toOption.flatten

  // unify com.everforth.everforth.util.RecordUtils
  def toStringValues(rec: Record): Vector[String] = {
    rec.fields.toVector.map(_.key).map(x => rec.getString(x) getOrElse "")
  }

  def reconstituteAnyRef(dt: DataType, x: Any): AnyRef = {
    dt match {
      case XBoolean => AnyRefUtils.toBoolean(x)
      case XByte => AnyRefUtils.toByte(x)
      case XShort => AnyRefUtils.toShort(x)
      case XInt => AnyRefUtils.toInt(x)
      case XLong => AnyRefUtils.toLong(x)
      case XFloat => AnyRefUtils.toFloat(x)
      case XDouble => AnyRefUtils.toDouble(x)
      case XInteger => AnyRefUtils.toBigInt(x)
      case XDecimal => AnyRefUtils.toBigDecimal(x)
      case XString => AnyRefUtils.toString(x)
      case XToken => AnyRefUtils.toString(x)
      case XText => AnyRefUtils.toString(x)
      case XDate => AnyRefUtils.toDate(x)
      case XTime => ???
      case XDateTime => AnyRefUtils.toTimestamp(x)
      case XBase64 => throw new UnsupportedOperationException(s"Not implemented yet: XBase64")
      case XBinary => throw new UnsupportedOperationException("Not implemented yet: XBinary")
      case XEntityId => AnyRefUtils.toString(x)
      case XEverforthid => AnyRefUtils.toString(x)
      case XEMail => AnyRefUtils.toString(x)
      case t: XEntityReference => AnyRefUtils.toString(x)
      case t: XEverforthObjectReference => AnyRefUtils.toString(x)
      case XFloat1 => AnyRefUtils.toFloat(x)
      case XHtml => AnyRefUtils.toString(x)
      case XLink => AnyRefUtils.toString(x)
      case XImageLink => AnyRefUtils.toString(x)
      case XMoney => AnyRefUtils.toBigDecimal(x)
      case XPercent => AnyRefUtils.toFloat(x)
      case t: XPowertype => ???
      case t: XPowertypeReference => ???
      case t: XStateMachine => AnyRefUtils.toInt(x)
      case t: XStateMachineReference => ???
      case XUnit => AnyRefUtils.toString(x)
      case XUuid => AnyRefUtils.toString(x)
      case XXml => AnyRefUtils.toString(x)
      case t: XValue => ???
      case t: XExternalDataType => AnyRefUtils.toString(x)
    }
  }

  // See JobPart#toCsvBag
  def toCsvBag(
    file: InputFile,
    schema: Option[Schema] = None,
    headerpolicy: RecordBag.HeaderPolicy = RecordBag.noHeader,
    codec: Codec = Codec.UTF8
  ): CsvBag = {
    val bag = toBufferFileBag(file)
    CsvBag.create(bag, codec, schema, headerpolicy)
  }

  def toBufferFileBag(
    file: InputFile
  ): BufferFileBag = {
    val bag = new BufferFileBag()
    val f = file.createWorkFile()
    val in = f.openStream()
    try {
      bag.write(in)
    } finally {
      in.close()
      f.dispose()
    }
    bag
  }

  // def toBufferFileBag0(
  //   file: InputFile
  // ): BufferFileBag = {
  //   val bag = new BufferFileBag()
  //   val f = file.createWorkFile()
  //   try {
  //     for {
  //       in <- resource.managed(f.openStream())
  //     } yield {
  //       bag.write(in)
  //     }
  //   } finally {
  //     f.dispose()
  //   }
  //   bag
  // }

  /*
   * Json
   */
  // Use RecordAux
  def toJsonString(rec: Record): String = {
    val buf = new StringBuilder
    buildJsonString(rec, buf)
    buf.toString
  }

  def buildJsonString(rec: Record, buf: StringBuilder) {
    def buildfield(kv: (String, Any)) {
      buf.append("\"")
      buf.append(JsonUtils.escape(kv._1))
      buf.append("\":")
      JsonUtils.data2json(buf, kv._2)
    }

    buf.append("{")
    val fs = _key_values(rec.fields)
    if (fs.nonEmpty) {
      buildfield(fs.head)
      for (f <- fs.tail) {
        buf.append(",")
        buildfield(f)
      }
    }
    buf.append("}")
  }

  def getRecordFromJsonString(s: String): Option[Record] =
    if (s.startsWith("{")) try {
      Some(fromJsonString(s))
    } catch {
      case NonFatal(e) => None
    } else
        None

  def fromJsonString(s: String): Record = {
    Json.parse(s) match {
      case j: JsObject => js2record(j)
      case x => throw new IllegalArgumentException(s"Not js object: $s")
    }
  }

  def fromJsonString(s: Option[String]): Record = {
    s.map(fromJsonString) getOrElse Record.empty
  }

  def js2recordset(j: JsValue): RecordSet = {
    RecordSet(js2records(j))
  }

  def js2records(j: JsValue): Seq[Record] = {
    j match {
      case null => Nil
      case JsNull => Nil
      case u: JsUndefined => Nil
      case o: JsObject => List(js2record(o))
      case a: JsArray => js2records(a)
      case _ => throw new IllegalArgumentException("Not object or array = " + j)
    }
  }

  def js2record(j: JsValue): Record = j match {
    case null => Record.empty
    case JsNull => Record.empty
    case m: JsUndefined => Record.empty
    case m: JsObject => js2record(m)
    case m: JsArray => throw new IllegalArgumentException(s"Array: $j")
    case _ => throw new IllegalArgumentException(s"Not object: $j")
  }

  def js2record(o: JsObject): Record = {
    val xs = o.fields.map {
      case (k, v) => k -> js2value(v)
    }
    Record.create(xs)
  }

  def js2records(a: JsArray): Seq[Record] = {
    for (x <- a.as[List[JsValue]]) yield {
      x match {
        case o: JsObject => js2record(o)
        case _ => throw new IllegalArgumentException("Not object in array = " + a)
      }
    }
  }

  def js2value(j: JsValue): Any = {
    j match {
      case null => None
      case JsNull => None
      case u: JsUndefined => None
      case o: JsObject => js2record(o)
      case a: JsArray => array2seq(a)
      case s: JsString => s.value
      case b: JsBoolean => b.value
      case n: JsNumber => n.value
//      case _ => sys.error("Unknown json format = " + j)
    }
  }

  def array2seq(a: JsArray): Seq[Any] = {
    for (x <- a.as[List[JsValue]]) yield {
      js2value(x)
    }
  }

  def js2strings(j: JsValue): Vector[String] = {
    val a = js2value(j)
    a match {
      case None => Vector.empty
      case x: Seq[_] => _to_strings(x)
      case x: String => Vector(x)
      case x: Boolean => Vector(x.toString)
      case x: BigDecimal => Vector(x.toString)
      case _ => throw new IllegalArgumentException("Not sequence: $a")
    }
  }

  private def _to_strings(xs: Seq[_]): Vector[String] = {
    xs.toVector map {
      case x: String => x
      case x: Boolean => x.toString
      case x: BigDecimal => x.toString
      case _ => throw new IllegalArgumentException("Not string: $a")
    }
  }

  def record2Json(rec: Record): JsObject = JsObject(
    rec.fields flatMap { x =>
      _values_to_json(x.values).map(y => x.key.name -> y)
    }
  )

  private def _values_to_json(vs: List[Any]): Option[JsValue] = vs match {
    case Nil => None
    case x :: Nil => Some(_value_to_json(x))
    case x :: xs => Some(JsArray(_value_to_json(x) :: xs.map(_value_to_json)))
  }

  private def _value_to_json(p: Any): JsValue = p match {
    case m: String => JsString(m)
    case m: Boolean => JsBoolean(m)
    case m: Short => JsNumber(BigDecimal(m))
    case m: Int => JsNumber(BigDecimal(m))
    case m: Long => JsNumber(BigDecimal(m))
    case m: Float => JsNumber(BigDecimal(m))
    case m: Double => JsNumber(BigDecimal(m))
    case m: Array[_] => JsArray(m.map(_value_to_json))
    case m: Seq[_] => JsArray(m.map(_value_to_json))
    case m: Record => record2Json(m)
    case m: JsValue => m
    case m => JsString(AnyUtils.toString(m))
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

  protected def to_value(vs: List[Any]) = {
    vs.map(_.toString).mkString(",")
  }

  def projectionForDb(schema: Schema)(rec: Record): Record = {
    Record(makeColumnFieldPairsForDb(schema, rec).map(_._2))
  }

  def makeColumnFieldPairsForDb(schema: Schema, rec: Record): List[(Column, Field)] = {
    val xs = schema.columns.toList flatMap { c =>
      val a = rec.fields.find { x =>
        val name = Strings.totokens(x.key.name, ".").last
        name.equalsIgnoreCase(c.name)
      }
      a.map(x => c -> x.copy(key = Symbol(c.name)))
    }
    complementDefaultValue(schema, xs)
  }

  def makeColumnFieldPairs(schema: Schema, rec: Record): List[(Column, Field)] = {
    val xs = schema.columns.toList flatMap { c =>
      val a = rec.fields.find(_.key.name == c.name)
      a.map(x => c -> x)
    }
    complementDefaultValue(schema, xs)
  }

  def complementDefaultValue(schema: Schema, data: List[(Column, Field)]): List[(Column, Field)] = {
    def complementrequiredfield(c: Column, f: Field): Field = {
      if (c.multiplicity == MOne)
        f.copy(values = RecordAux.getDefaultValue(c).toList)
      else
        f
    }

    schema.columns.toList map { c =>
      val a = data.find(_._1 == c) match {
        case Some((_, f)) =>
          if (f.getOne.isEmpty)
            complementrequiredfield(c, f)
          else
            f
        case None =>
          complementrequiredfield(c, Field(Symbol(c.name), Nil))
      }
      c -> a
    }
  }

  def makeOptionNonEmptyListString(rec: Record, key: Symbol): Option[NonEmptyList[String]] = {
    // rec.getConcreteStringList(key) flatMap StringUtils.makeOptionNonEmptyListString
    rec.getConcreteString(key) flatMap StringUtils.makeOptionNonEmptyListString
  }

  def sum(schema: Schema, axis: Symbol, targets: Seq[Symbol])(
    xs: Seq[Record]
  ): Map[Any, Record] = {
    xs.groupBy(_.getOne(axis) getOrElse "_missing_key").map {
      case (k, v) => k -> sum(schema, targets)(v).updateS(axis -> k)
    }
  }

  def sum(schema: Schema, targets: Seq[Symbol])(xs: Seq[Record]): Record = {
    targets.foldLeft(Record.empty) { (z, x) =>
      schema.columns.find(_.name == x.name) match {
        case Some(c) => z + sum(c)(xs)
        case None => throw new IllegalArgumentException(s"Missing column '$x.name'")
      }
    }
  }

  def sum(column: Column)(xs: Seq[Record]): Record = {
    def sumint(key: Symbol, lhs: Record, rhs: Record) = {
      (lhs.getFormInt(key) getOrElse 0) + (rhs.getFormInt(key) getOrElse 0)
    }
    def sumlong(key: Symbol, lhs: Record, rhs: Record) = {
      (lhs.getFormLong(key) getOrElse 0L) + (rhs.getFormLong(key) getOrElse 0L)
    }

    val f: (Symbol, Record, Record) => Any = {
      column.datatype match {
        case XInt => sumint
        case XLong => sumlong
        case _ => throw new IllegalArgumentException(s"Unsupported datatype '$column.datatype")
      }
    }

    def sum2(key: Symbol, base: Record, x: Record): Record = {
      base.updateS(key -> f(key, base, x))
    }

    val key = Symbol(column.name)
    xs.foldLeft(Record.empty)((z, x) => sum2(key, z, x))
  }

  def makeRecord[T](schema: Schema, fields: Seq[T]): Record = {
    Record.create(TupleUtils.makeTupleVector(schema, fields))
  }

  def makeRecordFlatten[T](schema: Schema, fields: Seq[Option[T]]): Record = {
    Record.create(TupleUtils.makeTupleVectorFlatten(schema, fields))
  }

  case class Slot(column: Column, lefts: List[String], rights: List[String]) {
    def isMatch(p: Slot): Boolean = isMatch(p.column.name)
    def isMatch(p: String): Boolean = column.name == p
  }

  def buildSchema(p: Record, ps: Record*): Schema = buildSchema(p +: ps)

  def buildSchema(ps: Seq[Record]): Schema = {
    // case class Slot(column: Column, lefts: List[String], rights: List[String]) {
    //   def isMatch(p: Slot): Boolean = isMatch(p.column.name)
    //   def isMatch(p: String): Boolean = column.name == p
    // }
    // def tocolumn(f: Field): Column = Column(f.key.name, XString)
    // def toslot(rec: Record, f: Field): Slot = rec.fields.span(_ != f) match {
    //   case (Nil, Nil) => Slot(tocolumn(f), Nil, Nil)
    //   case (Nil, rs) => Slot(tocolumn(f), Nil, rs.tail.map(_.key.name))
    //   case (ls, Nil) => Slot(tocolumn(f), ls.map(_.key.name).reverse, Nil)
    //   case (ls, rs) => Slot(tocolumn(f), ls.map(_.key.name).reverse, rs.tail.map(_.key.name))
    // }
    // def split(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    //   ss.toList.span(_.isMatch(s)) match {
    //     case (Nil, Nil) => (Nil, s, Nil)
    //     case (Nil, rs) => (Nil, rs.head, rs.tail.toList)
    //     case (ls, Nil) => splitguess(ss, s)
    //     case (ls, rs) => (ls.toList, rs.head, rs.tail.toList)
    //   }
    // def splitguess(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    //   s.lefts match {
    //     case Nil => splitguess2(ss, s)
    //     case x :: xs => splitguessleft(ss, s, x, xs)
    //   }
    // def splitguessleft(ss: List[Slot], s: Slot, name: String, candidates: List[String]): (List[Slot], Slot, List[Slot]) =
    //   ss.toList.span(_.isMatch(name)) match {
    //     case (Nil, Nil) => (Nil, s, Nil)
    //     case (Nil, rs) => (List(rs.head), s, rs.tail)
    //     case (ls, Nil) => candidates match {
    //       case Nil => splitguess2(ss, s)
    //       case x :: xs => splitguessleft(ss, s, x, xs)
    //     }
    //     case (ls, rs) => (ls :+ rs.head, s, rs.tail)
    //   }
    // def splitguess2(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    //   s.rights match {
    //     case Nil => (Nil, s, Nil)
    //     case x :: xs => splitguessright(ss, s, x, xs)
    //   }
    // def splitguessright(ss: List[Slot], s: Slot, name: String, candidates: List[String]): (List[Slot], Slot, List[Slot]) =
    //   ss.toList.span(_.isMatch(name)) match {
    //     case (Nil, Nil) => (Nil, s, Nil)
    //     case (Nil, rs) => (Nil, s, rs)
    //     case (ls, Nil) => candidates match {
    //       case Nil => (ls, s, Nil)
    //       case x :: xs => splitguessright(ss, s, x, xs)
    //     }
    //     case (ls, rs) => (ls, s, rs)
    //   }
    // def merge(z: Slot, n: Slot): Slot = {
    //   case class Z(xs: Vector[String]) {
    //     def r = xs.toList
    //     def +(rhs: String) = if (r.contains(rhs)) this else Z(xs :+ rhs)
    //   }
    //   val l = n.lefts./:(Z(z.lefts.toVector))(_+_).r
    //   val r = n.rights./:(Z(z.rights.toVector))(_+_).r
    //   Slot(n.column, l, r)
    // }
    case class Z(columns: List[Slot] = Nil) {
      def r = Schema(columns.map(_.column))
      def +(rhs: Record): Z = {
        case class ZZ(r: List[Slot] = Nil) {
          def +(f: Field): ZZ = {
//            println("f:" + f)
            val slot = _to_slot(rhs, f)
//            println("slot:" + slot)
//            println("r:" + r)
            val (ls, x, rs) = _split(r, slot)
//            println("ZZ+")
            // println(ls)
            // println(x)
            // println(rs)
            ZZ(ls ++ List(_merge(x, slot)) ++ rs)
          }
        }
//        println("rec:" + rhs.fields)
        Z(rhs.fields./:(ZZ(columns))(_+_).r)
      }
    }
    ps./:(Z())(_+_).r
  }

  private def _to_slot(rec: Record, f: Field): Slot =
    SeqUtils.split3L((_: Field) == f)(rec.fields) match {
      case (ls, Nil, rs) => Slot(_to_column(f), ls.map(_.key.name), rs.map(_.key.name))
      case (ls, cs, rs) => Slot(_to_column(cs.head), ls.map(_.key.name), rs.map(_.key.name))
    }

  private def _to_column(f: Field): Column = {
    val datatype = DataType.guessSeq(f.values)
    val multiplicity = Multiplicity.guess(f.values)
    Column(f.key.name, datatype, multiplicity)
  }

  private def _split(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    SeqUtils.split3L((_: Slot).isMatch(s))(ss) match {
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
    SeqUtils.split3L(_is_slot(name))(ss) match {
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
    SeqUtils.split3L(_is_slot(name))(ss) match {
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

  // LTSV
  def toLtsv(rec: Record): String =
    rec.fields.map(x => x.key.name + ":" + _to_ltsv_values(x.values)).mkString("\t")

  def toLtsvPart(rec: Record): String =
    rec.fields.map(x => "\t" + x.key.name + ":" + _to_ltsv_values(x.values)).mkString

  private def _to_ltsv_values(xs: List[Any]): String = xs.map(_to_ltsv_value).mkString(",")

  private def _to_ltsv_value(x: Any): String = {
    val a = x match {
      case rec: Record => toJsonString(rec)
      case _ => AnyUtils.toString(x)
    }
    _escape_ltsv(a)
  }

  private def _escape_ltsv(s: String): String = JsonUtils.escape(s)

  object Implicits {
    implicit object RecordMonoid extends Monoid[Record] {
      def append(lhs: Record, rhs: => Record) = lhs + rhs // TODO monoidAppend
      def zero = Record.empty
    }
  }
}
