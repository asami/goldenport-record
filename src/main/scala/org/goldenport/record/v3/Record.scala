package org.goldenport.record.v3

import scalaz._, Scalaz._
import java.sql.Timestamp
import org.joda.time.DateTime
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.record.util.StringUtils
import org.goldenport.record.v2.{Record => Record2, Schema}
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.collection.{NonEmptyVector, VectorMap}
import org.goldenport.values.PathName

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
 *  version Sep. 17, 2018
 *  version Oct. 30, 2018
 *  version Nov.  7, 2018
 * @version Dec. 29, 2018
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
  def toRecord2: Record2 = {
    Record2.createApp(nameValues)
  }

  def isEmpty: Boolean = fields.isEmpty
  def isDefined(key: Symbol): Boolean = fields.exists(_.key == key)
  def isDefined(key: String): Boolean = isDefined(Symbol(key))

  def nonDefined(key: Symbol): Boolean = !isDefined(key)
  def nonDefined(key: String): Boolean = !isDefined(key)

  lazy val keyNames: List[String] = fields.map(_.name).toList

  def get(key: String): Option[Any] = getField(key).flatMap(_.value.getValue)

  def get(key: Symbol): Option[Any] = getField(key).flatMap(_.value.getValue)

  def getList(key: String): Option[List[Any]] = getField(key).flatMap(_.value.getList)

  def getList(key: Symbol): Option[List[Any]] = getField(key).flatMap(_.value.getList)

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

  override def getString(key: Symbol): Option[String] = {
    getField(key).map(_.asString)
  }

  override def getString(key: String): Option[String] = {
    getString(Symbol(key))
  }

  def asString(key: Symbol): String = {
    getString(key) getOrElse {
      throw new IllegalArgumentException(s"Missing string '$key.name'")
    }
  }

  override def getInt(key: Symbol): Option[Int] = getField(key).map(_.asInt)
  override def getInt(key: String): Option[Int] = getField(key).map(_.asInt)

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

  // def getTimestamp(key: Symbol): Option[Timestamp] = {
  //   getField(key).map(_.asTimestamp)
  // }

  // def getDateTime(key: Symbol): Option[DateTime] = {
  //   getField(key).map(_.asDateTime)
  // }

  // def asDateTime(key: Symbol): DateTime = {
  //   getDateTime(key) getOrElse {
  //     throw new IllegalArgumentException(s"Missing datetime '$key.name'")
  //   }
  // }

  def getRecord(key: Symbol): Option[Record] = getField(key).map(_.asRecord)
  def getRecord(key: String): Option[Record] = getField(key).map(_.asRecord)

  def takeRecordList(key: Symbol): List[Record] = getField(key).map(_.asRecordList).getOrElse(Nil)
  def takeRecordList(key: String): List[Record] = getField(key).map(_.asRecordList).getOrElse(Nil)

  def keyValues: Seq[(Symbol, Any)] = fields.flatMap(_.keyValue)
  def nameValues: Seq[(String, Any)] = fields.flatMap(_.nameValue)
  override def asNameStringVector: Vector[(String, String)] = fields.flatMap(_.nameString).toVector

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
      case Nil => prefix :+ Field(key, value)
      case x :: xs => prefix ++ (Field(key, value) :: xs)
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

  def apply(ps: Iterator[Field]): Record = Record(ps.toVector)

  def data(data: (String, Any)*): Record = createDataSeq(data)

  def dataOption(data: (String, Option[Any])*): Record = {
    val xs = data.collect {
      case (k, Some(v)) => k -> v
    }
    create(xs)
  }

  def create(map: scala.collection.Map[String, Any]): Record = create(map.toVector)

  def create(data: Seq[(String, Any)]): Record = createDataSeq(data)

  def create(p: IRecord): Record = createSymbolAnySeq(p.asSymbolAnyVector)

  def create(p: Record2): Record = createDataSeq(p.toVector)

  def createDataSeq(data: Seq[(String, Any)]): Record =
    Record(data.map(Field.createData))

  def createSymbolAnySeq(ps: Seq[(Symbol, Any)]): Record =
    Record(ps.map(Field.create))

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

  /*
   * Normalize multiplicity, nesting
   *
   * - record nesting
   *
   * a/x=AX
   * a/y=AY
   *
   * - property sequence
   *
   * a__1/x=AX
   * a__1/y=AY
   * a__2/x=AX
   * a__2/y=AY
   * 
   * - record sequence
   * 
   * __1/x=AX
   * __1/y=AY
   * __2/x=AX
   * __2/y=AY
   * 
   */
  def build(p: Record): Either[NonEmptyVector[Record], Record] =
    build(p.fields)

  sealed trait Slot {
  }
  case class LeafSlot(value: FieldValue) extends Slot {
  }
  case class ChildSlot(path: PathName, value: FieldValue) extends Slot {
    lazy val name = path.firstComponent
    lazy val getChild = path.getChild
  }

  type IndexName = String
  type PropertyName = String

  def build(ps: Seq[Field]): Either[NonEmptyVector[Record], Record] = {
    case class Z(
      xs: VectorMap[PropertyName, Vector[Slot]] = VectorMap.empty
    ) {
      def r: Either[NonEmptyVector[Record], Record] = {
        // println(s"Z#r: $this")
        xs.toVector./:(ZZ())(_+_).r
      }

      def +(rhs: Field) = {
        val a: VectorMap[String, Vector[Slot]] = {
          val pn = PathName(rhs.name)
          pn.tailOption.map { x =>
            VectorMap(pn.head -> Vector(ChildSlot(x, rhs.value)))
          }.getOrElse(
            VectorMap(pn.head -> Vector(LeafSlot(rhs.value)))
          )
        }
        // println(s"normalize: $a")
        copy(xs = xs |+| a)
      }
    }

    ps./:(Z())(_+_).r
  }

  private case class ZZ(
    xs: Vector[(PropertyName, FieldValue)] = Vector.empty
  ) {
    def r: Either[NonEmptyVector[Record], Record] = _zzzz(xs)

    def +(rhs: (PropertyName, Vector[Slot])) = {
      val (propname, slots) = rhs
      val v = _calc_slot(slots)
      copy(xs = xs :+ (propname, v))
    }
  }

  private def _zzzz(ps: Vector[(PropertyName, FieldValue)]): Either[NonEmptyVector[Record], Record] = {
    val zzzz = ps./:(ZZZZ())(_+_)
    zzzz.getWholeVector.map(Left.apply).getOrElse(Right(zzzz.record))
  }

  // private case class ZZ(
  //   wholeRecords: Vector[Record] = Vector.empty,
  //   fields: Vector[Field] = Vector.empty
  // ) {
  //   def r: Either[NonEmptyVector[Record], Record] = {
  //     println(s"ZZ: $this")
  //     wholeRecords.headOption.map(x =>
  //       Left(NonEmptyVector(x, wholeRecords.tail))
  //     ).getOrElse(
  //       Right(Record(fields))
  //     )
  //   }

  //   def +(rhs: (String, Vector[Slot])) = {
  //     val (propname, slots) = rhs
  //     val field = _zzz(propname, slots)

  //   // def +(rhs: (String, Vector[Slot])) = {
  //   //   val (propname, slots) = rhs
  //   //   val xs: Vector[(String, Field)] = slots./:(ZZZ(propname))(_+_).r
  //   //   println(s"after ZZZ: $slots => $xs")
  //   //   val zzzz = xs./:(ZZZZ())(_+_)
  //   //   println(s"after ZZZZ: $zzzz")
  //   //   zzzz.getWholeVector.
  //   //     map(x => copy(wholeRecords = wholeRecords ++ x.vector)).
  //   //     getOrElse(copy(fields = fields ++ zzzz.fields))
  //   // }
  // }

  private def _calc_slot(slots: Vector[Slot]): FieldValue = {
    case class Z(
      value: FieldValue = EmptyValue,
      leaf: VectorMap[PropertyName, Vector[FieldValue]] = VectorMap.empty,
      container: VectorMap[PropertyName, VectorMap[PathName, Vector[FieldValue]]] = VectorMap.empty
    ) {
      def r: FieldValue =
        if (container.nonEmpty)
          _calc_record_container_slot(container)
        else if (leaf.nonEmpty)
          _calc_record_slot(leaf)
        else
          value

      def +(rhs: Slot) = rhs match {
        case LeafSlot(v) => copy(value = value + v)
        case ChildSlot(n, v) =>
          n.tailOption.
            map { tail =>
              // RAISE.unsupportedOperationFault(s"_calc_slot: $n($tail), $v")
              // _calc_slot: xyz__1/A(A), SingleValue(abc1xyz1A)
              copy(container = container |+| VectorMap(n.head -> VectorMap(tail -> Vector(v))))
            }.getOrElse(
              copy(leaf = leaf |+| VectorMap(n.head -> Vector(v)))
            )
      }
    }
    slots./:(Z())(_+_).r
  }

  private def _calc_record_container_slot(ps: VectorMap[PropertyName, VectorMap[PathName, Vector[FieldValue]]]): FieldValue = {
    // RAISE.unsupportedOperationFault(s"_calc_record_container_slot: $ps")
    // _calc_record_container_slot: Map(xyz__1 -> Map(A -> Vector(SingleValue(abc1xyz1A))), xyz__2 -> Map(A -> Vector(SingleValue(abc1xyz2A))), mno__1 -> Map(B -> Vector(SingleValue(abc1mno1B))), mno__2 -> Map(B -> Vector(SingleValue(abc1mno2B))))
    val r: Vector[(PropertyName, FieldValue)] = ps.vector.map {
      case (k, v) =>
        val xs = v.vector.map {
          case (kk, vv) => Field(kk.v, vv.concatenate)
        }
        val a = build(xs) match {
          case Left(l) => MultipleValue(l.vector)
          case Right(r) => SingleValue(r)
        }
        k -> a
    }
    _zzzz(r) match {
      case Left(l) => MultipleValue(l.vector)
      case Right(r) => SingleValue(r)
    }
  }

  private def _calc_record_slot(ps: VectorMap[PropertyName, Vector[FieldValue]]): FieldValue = {
    val fields = ps.vector.map {
      case (k, vs) => Field(k, _calc_record_value(vs))
    }
    SingleValue(Record(fields))
  }

  private def _calc_record_value(vs: Vector[FieldValue]): FieldValue = {
    case class Z(r: FieldValue = EmptyValue) {
      def +(rhs: FieldValue) = copy(r = r + rhs)
    }
    vs./:(Z())(_+_).r
  }

  // private case class ZZZ(
  //   propName: String,
  //   xs: Vector[Vector[Slot]] = VectorMap.empty
  // ) {
  //   def r: Vector[(PropertyName, Field)] = xs.vector.map {
  //     case (k, v) => propName -> Field(Symbol(propName), _to_value(v))
  //   }

  //   private def _to_value(ps: Vector[Slot]): FieldValue = {
  //     case class Z(
  //       value: FieldValue = EmptyValue,
  //       fields: VectorMap[PropertyName, FieldValue] = VectorMap.empty
  //     ) {
  //       def r: FieldValue = if (fields.isEmpty)
  //         value
  //       else
  //         SingleValue(Record(fields.vector.map(Field.apply)))

  //       def +(rhs: Slot) = rhs match {
  //         case m: LeafSlot => copy(value = value + m.field.value)
  //         case m: ChildSlot => copy(fields = fields update _to_field(m))
  //       }

  //       private def _to_field(p: ChildSlot): (PropertyName, FieldValue) = {
  //         p.path.getChild.
  //           map { child => 
  //             RAISE.unsupportedOperationFault(s"ZZZ: $p")
  //           }.getOrElse(
  //             (p.path.head, p.field.value)
  //           )
  //       }
  //     }
  //     ps./:(Z())(_+_).r
  //   }

  //   def +(rhs: Slot) = copy(xs = xs |+| VectorMap(rhs.name -> Vector(rhs)))
  // }

  sealed trait MultiplicitySlot
  case class PlainSlot(field: Field) extends MultiplicitySlot
  case class WholeArraySlot(name: String, field: Field) extends MultiplicitySlot
  case class PropertyArraySlot(name: String, field: Field) extends MultiplicitySlot

  sealed trait ZZZZSlot
  case class ValueSlot(value: FieldValue) extends ZZZZSlot
  case class SequenceSlot(index: String, value: FieldValue) extends ZZZZSlot

  private val _regex = """([^_]+)?__([^_]+)""".r

  private case class ZZZZ(
    whole: VectorMap[IndexName, Vector[ZZZZSlot]] = VectorMap.empty,
    property: VectorMap[PropertyName, Vector[ZZZZSlot]] = VectorMap.empty
  ) {
    def getWholeVector: Option[NonEmptyVector[Record]] =
      if (whole.isEmpty)
        None
      else
        Some(NonEmptyVector(
          _to_record(whole.head),
          whole.tail.map(_to_record).toVector
        ))

    private def _to_record(ps: (IndexName, Vector[ZZZZSlot])): Record = {
      val xs = ps._2
      xs.head match {
        case ValueSlot(v) => v match {
          case SingleValue(vv) => vv.asInstanceOf[Record]
          case m: MultipleValue => RAISE.unsupportedOperationFault(s"_to_record: $m")
          case EmptyValue => RAISE.unsupportedOperationFault(s"_to_record: EmptyValue")
        }
        case m: SequenceSlot => RAISE.unsupportedOperationFault(s"_to_record: $m")
      }
    }

    def record: Record = Record(fields)

    def fields: Vector[Field] =
      property.toVector.map {
        case (k, v) => Field(k, _to_value(v))
      }

    private def _to_value(ps: Vector[ZZZZSlot]): FieldValue = {
      case class Z(
        value: FieldValue = EmptyValue,
        fields: VectorMap[PropertyName, FieldValue] = VectorMap.empty
      ) {
        def r: FieldValue =
          if (fields.isEmpty)
            value
          else
            SingleValue(Record(fields.toVector.map {
              case (k, v) => Field(k, v)
            })) // TODO MultiValue

        def +(rhs: ZZZZSlot) = rhs match {
          case m: ValueSlot => copy(value = value + m.value)
          case m: SequenceSlot => copy(value = value + m.value.toMulti)
            // RAISE.unsupportedOperationFault(s"getWholeVector#_to_value: $m")
        }
      }
      ps./:(Z())(_+_).r
    }

    def +(rhs: (String, FieldValue)) = {
      val (name, v) = rhs
      if (name.contains("_")) {
        _regex.findFirstMatchIn(name).map { x =>
          val index = x.group(2)
          Option(x.group(1)).map { y =>
            // RAISE.unsupportedOperationFault(s"ZZZ: $index, $y")
            _sequence(y, index, v)
          }.getOrElse {
            _whole(index, v)
          }
        }.getOrElse(_plain(name, v))
      } else {
        _plain(name, v)
      }
    }

    private def _plain(name: String, v: FieldValue) =
      copy(property = property |+| VectorMap(name -> Vector(ValueSlot(v))))

    private def _whole(index: String, v: FieldValue) =
      copy(whole = whole |+| VectorMap(index -> Vector(ValueSlot(v))))

    private def _sequence(name: String, index: String, v: FieldValue) =
      copy(property = property |+| VectorMap(name -> Vector(SequenceSlot(index, v))))
  }

  // private def _get_node_remainder(p: String): Option[(String, String)] =
  //   p.indexOf("/") match {
  //     case -1 => None
  //     case 0 => _get_node_remainder(p.substring(1))
  //     case n => Some((p.substring(0, n), p.substring(n + 1)))
  //   }

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
