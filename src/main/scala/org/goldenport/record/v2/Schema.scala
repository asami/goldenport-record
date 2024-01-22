package org.goldenport.record.v2

import scalaz._, Scalaz._
import java.util.Locale
import java.sql.Timestamp
import org.goldenport.Strings
import org.goldenport.extension.{IDocument, Description}
import com.asamioffice.goldenport.text.UString
import org.goldenport.exception.RAISE
import org.goldenport.i18n.{I18NString, I18NMessage}
import org.goldenport.collection.NonEmptyVector
import org.goldenport.extension.Showable
import org.goldenport.util.AnyUtils
import org.goldenport.record.command.ValueCommand
import org.goldenport.record.v2.projector.{Projector, ProjectorContext}
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.record.v3.IRecord

/*
 * Add
 * Column/curd
 */
/*
 * @since   Nov. 23, 2012
 *  version Dec. 28, 2012
 *  version Jan. 30, 2013
 *  version Mar. 12, 2013
 *  version Apr. 26, 2013
 *  version Jun. 24, 2013
 *  version Oct. 23, 2013
 *  version Feb.  6, 2014
 *  version Jun.  9, 2014
 *  version Jul. 25, 2014
 *  version Aug.  6, 2014
 *  version Sep. 25, 2015
 *  version Oct. 15, 2015
 *  version May. 26, 2016
 *  version Sep.  8, 2016
 *  version Jan. 21, 2017
 *  version May. 25, 2017
 *  version Aug.  1, 2017
 *  version Sep. 21, 2017
 *  version Oct. 25, 2017
 *  version Nov. 23, 2017
 *  version Dec. 13, 2017
 *  version Jan. 22, 2018
 *  version May. 16, 2018
 *  version Jul. 28, 2018
 *  version Aug. 29, 2018
 *  version Sep.  5, 2018
 * @version Oct. 24, 2018
 *  version Jan.  1, 2019
 *  version Feb. 12, 2019
 *  version Apr. 29, 2019
 *  version May.  1, 2019
 *  version Aug. 20, 2019
 *  version Oct.  1, 2019
 *  version Dec. 30, 2019
 *  version Jan. 12, 2020
 *  version Apr. 17, 2020
 *  version Jun.  8, 2020
 *  version Mar. 17, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
case class Schema(
  columns: Seq[Column],
  columnGroups: Seq[ColumnGroup] = Nil,
  grouping: Grouping = NullGrouping,
  validators: Seq[Validator] = Nil,
  sql: SqlSchema = NullSqlSchema,
  isAutoLabel: Boolean = true,
//  contexts: Seq[Context] = Nil,
//  view: GuiView = ExtjsGridView(),
//  charts: Seq[Chart] = Nil,
  formatter: Option[FormatterGroup] = None,
  pageSize: Option[Int] = 100.some,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  isCsvTitle: Option[Boolean] = None,
//  comment: String = "",
//  history: String = ""
  desc: Description = Description.empty,
  xml: XmlSchema = XmlSchema.default
) extends org.goldenport.table.ISchema with Showable {
  import scalaz.syntax.foldable._
  implicit object ValidationResultMonoid extends Monoid[ValidationResult] {
    def append(f1: ValidationResult, f2: => ValidationResult) = f1 + f2
    def zero: ValidationResult = Valid
  } // TODO uses Validation.ValidationResultMonoid

  override def toString() = if (Schema.isDebug)
    s"""Schema(${columns.map(_.showlong).mkString(";")})"""
  else
    s"""Schema(${columns.map(_.show).mkString(";")})"""

  def display = toString // TODO
  def print = toString // TODO

  def show = {
    val s = Schema(
      Vector(
        Column("name"),
        Column("datatype"),
        Column("multiplicity")
      )
    )
    val xs: Seq[IRecord] = columns.map(c =>
      org.goldenport.record.v3.Record.data(
        "name" -> c.name,
        "datatype" -> c.datatype.name,
        "multiplicity" -> c.multiplicity.mark
      )
    )
    val t = org.goldenport.record.v3.Table.create(s, xs)
    val v = org.goldenport.record.v3.TableVisualizer.thick
    v.plainText(t)
  }

  final def keys: List[String] = columns.map(_.name).toList

  final def getColumn(key: String) = {
    columns.find(_.name == key)
  }

  final def getColumn(key: Symbol) = {
    columns.find(_.name == key.name)
  }

  final def getIdColumn: Option[Column] = {
    columns.find(_.kind == IdKind) orElse
    columns.find(_.sql.isId)
  }

  final def idColumn: Column = {
    getIdColumn match {
      case Some(c) => c
      case None => throw new IllegalStateException("no id column")
    }
  }

  final def realAttributeColumns: Seq[Column] = {
    def f(c: Column) = !c.sql.isDerived
    columns.filter(f)
  }

  final def enableColumns(ps: Seq[String]): Schema =
    removeColumns(x => !ps.contains(x.name))

  final def enableColumn(p: String, ps: String*): Schema = enableColumns(p +: ps)

  final def removeColumns(f: Column => Boolean): Schema =
    copy(columns = columns.filterNot(f))

  final def removeColumns(ps: Seq[String]): Schema =
    removeColumns(x => ps.contains(x.name))

  final def removeColumn(p: String, ps: String*): Schema = removeColumns(p +: ps)

  final def addColumns(cs: Seq[Column]): Schema = {
    copy(columns = this.columns ++ cs)
  }

  final def replaceColumn(name: String, column: Column): Schema = {
    case class Z(r: Vector[Column] = Vector.empty) {
      def +(rhs: Column) = {
        if (rhs.name == name)
          Z(r :+ column)
        else
          Z(r :+ rhs)
      }
    }
    copy(columns = columns./:(Z())(_+_).r)
  }

  protected def map_Record(record: Record): Record = {
    record
  }

  protected def map_Records(records: RecordSet): RecordSet = {
    records
  }

  val gridColumns = columns.filter(_.visibility.grid).filter(_.isSingle)

  final def select(names: Seq[String]): Schema = {
    val xs = names./:(Vector.empty[Column])((z, x) => columns.find(_.name == x).
      map(a => z :+ a).
      getOrElse(RAISE.noSuchElementFault(x)))
    copy(columns = xs)
  }

  def adjustInsert(r: Record): Record = {
//    log_trace("Schema#adjustInsert before = " + r)
    val a = r.fields.flatMap(filter_insert)
    val b = columns.flatMap(_complement_insert(_, a, r))
//    log_trace("Schema#adjustInsert after = " + (a ++ b))
    r.copy(fields = a ++ b)
  }

  private def _complement_insert(c: Column, fs: Seq[Field], r: Record): Option[Field] = {
    _complement_principal(c, fs, r)
  }

  private def _complement_principal(c: Column, fs: Seq[Field], r: Record): Option[Field] = {
    if (is_create_update_principal(c) && !fs.exists(_.key.name == c.name)) {
      r.principal.map(x => Field(Symbol(c.name), List(x.asString)))
    } else None
  }

  def adjustUpdate(r: Record): Record = {
    val a = r.fields.flatMap(filter_update)
    val b = columns.flatMap(_complement_update(_, a, r))
    Record(a ++ b)
  }

  private def _complement_update(c: Column, fs: Seq[Field], r: Record): Option[Field] = {
    _complement_principal(c, fs, r)
  }

  // TODO handle date, time and datetime here, instead of driver.
  protected final def filter_insert(f: Field): Option[Field] = {
    val name = f.key.name
    // val dummy = if (name == "company_id")
    //   true
    // else
    //   false
    columns.find(_.name == name) match {
      case Some(c) if is_create_update_principal_value(c, f) => {
//        log_trace("Schema#filter_insert drop(%s) = %s".format(c, f))
        // f.copy(value = List(principal_id)).some
        None
      }
      case Some(c) => c.isSingle option f
      case None => {
//        log_trace("Schema#filter_insert drop = " + f)
        None
      }
    }
  }

  protected final def filter_update(f: Field): Option[Field] = {
    columns.find(_.name == f.key.name) match {
      case Some(c) if is_update_principal_value(c, f) => None // f.copy(value = List(principal_id)).some
      case Some(c) => c.isSingle option f
      case None => None
    }
  }

  protected final def is_create_update_principal_value(
    c: Column, f: Field
  ) = {
    f.isEmpty && is_create_update_principal(c)
  }

  protected final def is_create_update_principal(c: Column) = {
    (c.name.endsWith("_by") && // TODO adds specific flag
    (c.sql.isAutoCreate || c.sql.isAutoUpdate))
  }

  protected final def is_update_principal_value(
    c: Column, f: Field
  ) = {
    f.isEmpty && is_update_principal(c)
  }

  protected final def is_update_principal(c: Column) = {
    (c.name.endsWith("_by") && // TODO adds specific flag
     c.sql.isAutoUpdate)
  }

  def complement(p: Record): Record = {
    case class Z(r: Record) {
      def +(rhs: Column) = {
        if (_requiredp(rhs))
          rhs.form.value.fold(this) { x => 
            Z(r = r ::+ (rhs.name -> x))
          }
        else
          this
      }
    }
    columns./:(Z(p))(_+_).r
  }

  /*
   * Validation
   */
  def validate(rs: RecordSet): ValidationResult = {
    rs.records.map(validate).toVector.suml
  }

  // def validate0(r: Record): ValidationResult = {
  //   import scalaz.syntax.foldable._
  //   val a = _validate_redumental_fields(r)
  //   if (a.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + a)
  //   val b = _validate_missing_fields(r)
  //   if (b.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + b)
  //   val c = _validate_datatype(r)
  //   if (c.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + c)
  //   val d = _validate_validators(r)
  //   if (d.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + d)
  //   List(a, b, c, d).toVector.suml
  // }

  def validate(p: IRecord): ValidationResult = validate(p.toRecord.toRecord2)

  def validate(r: Record): ValidationResult = {
    List(_validate_redumental_fields(r),
         _validate_missing_fields(r),
         _validate_datatype(r),
         _validate_validators(r)
    ).suml
  }

  def validate(r: Record, policy: Projector.Policy): ValidationResult = {
    List(
      policy.severe.redundancyField ?? _validate_redumental_fields(r),
      policy.severe.missingField ?? _validate_missing_fields(r),
      policy.severe.datatype ?? _validate_datatype(r),
      policy.severe.datatype ?? _validate_validators(r)
    ).suml
  }

  private def _validate_redumental_fields(r: Record): ValidationResult = {
    r.fields.map(_.key.name).diff(columns.map(_.name)) match {
      case Nil => Valid
      case xs => {
        val a: Seq[ValidationResult] = xs.map(x => RedundancyFieldWarning(x))
        a.toVector.suml
      }
    }
  }

  private def _validate_missing_fields(r: Record): ValidationResult = {
    val a = columns.filter(_requiredp).map(_.name)
    a.diff(r.fields.map(_.key.name)) match {
      case Nil => Valid
      case xs => {
        val a: Seq[ValidationResult] = xs.map(x => MissingFieldFailure(x))
        a.toVector.suml
      }
    }
  }

  private def _requiredp(c: Column): Boolean = {
    val sql = c.sql
    (c.multiplicity == MOne || c.multiplicity == MOneMore) &&
    (!sql.isDerived) && (!sql.isAutoId) && (!sql.isReadOnly) &&
    (!sql.isAutoCreate) && (!sql.isAutoUpdate)
  }

  private def _validate_datatype(r: Record): ValidationResult = {
    r.fields.flatMap(
      f => getColumn(f.key).map(c => _validate_datatype_column(c, r, f))
    ).toVector.suml
  }

  private def _validate_datatype_column(c: Column, rec: Record, f: Field): ValidationResult = {
    val a: Seq[ValidationResult] = f.values.map(
      _ match {
        case m: ValueCommand => Valid
        case xs: Seq[_] => xs.map(x => _validate_datatype_column_value(c, rec, x)).toVector.suml
        case x => _validate_datatype_column_value(c, rec, x)
      }
    )
    a.toVector.suml
  }

  private def _validate_datatype_column_value(c: Column, rec: Record, p: Any): ValidationResult = {
    val a = c.datatype.validate(p).enkey(c.name)
    val b = c.constraints.flatMap(_.validate(c.datatype, AnyUtils.toString(p), rec))
    (a +: b).suml
  }

  private def _validate_validators(r: Record): ValidationResult =
    validators.toVector.map(_.validate(r)).suml

  /*
   * SQL
   */
  def sqlCommands: SqlActionCommands = {
//    val methods = columns.flatMap(x => x.sql.methods.map(_.create(x)))
    SqlActionCommands(sql.actions.map(_.create(this)))
  }

  /*
   * SQL Utilities
   */
  def sqlLiteral(columnname: String, rec: Record): String = {
    rec.getString(columnname) match {
      case Some(s) => {
        if (isSqlStringLiteral(columnname)) "'" + s + "'"
        else s
      }
      case None => null
    }
  }

  def getSqlLiteral(columnname: String, rec: Record): Option[String] = {
    rec.getString(columnname).map(s =>
      if (isSqlStringLiteral(columnname)) "'" + s + "'"
      else s
    )
  }

  def isSqlStringLiteral(columnname: String): Boolean = {
    getColumn(Symbol(columnname)) match {
      case Some(c) if c.datatype.isSqlString => true
      case _ => false
    }
  }

  /*
   * Format for display
   */
  def format(columnname: String, rec: Record): String = {
    val v = rec.get(columnname)
    columns.find(_.name == columnname) map { c =>
      formatter.flatMap(_.format(c, v)) getOrElse c.format(v)
    } getOrElse {
      throw new IllegalArgumentException(s"Illegal column name = $columnname")
    }
  }

  def header: Seq[String] = {
    columns.map { x =>
      x.label getOrElse {
        if (isAutoLabel) UString.capitalize(x.name) else x.name
      }
    }
  }

  /*
   * Import/Export and Convert
   */
  def importIn(rec: Record): Record = columns.foldLeft(rec)((z, x) => x importIn z)
  def importIn(ctx: ProjectorContext, rec: Record): Record = columns.foldLeft(rec)((z, x) => x.importIn(ctx, z))
  def exportOut(rec: Record): Record = columns.foldLeft(rec)((z, x) => x exportOut z)

  def convertIn(rec: Record): Record = columns.foldLeft(rec)((z, x) => x convertIn z)
  def convertOut(rec: Record): Record = columns.foldLeft(rec)((z, x) => x convertOut z)

  def importConvertIn(rec: Record): Record = convertIn(importIn(rec))
  def exportConvertOut(rec: Record): Record = convertOut(exportOut(rec))

  /*
   * Marshall
   */
  def marshall: String = Schema.json.marshall(this)
  def marshallRecord: Record = RecordUtils.fromJsonString(marshall)

  def toMarshalizable: Schema = copy(columns =
    columns.map { c =>
      c.datatype match {
        case m: XEntityReference => c.withDatatype(XEntityId)
        case m: XEverforthObjectReference => c.withDatatype(XEntityId)
        case _ => c
      }
    }
  )
}

object NullSchema extends Schema(Nil)

object Schema {
  val empty = NullSchema
  val isDebug: Boolean = false // turn on if debug.

  def createByColumnNamesWithComma(p: String): Schema =
    Schema(Strings.totokens(p, ",").map(x => Column(x)))

  def comp(lhs: Schema, rhs: Schema): List[String] = {
    if (lhs.columns.length != rhs.columns.length)
      RAISE.noReachDefect
    lhs.columns.zip(rhs.columns).toList.flatMap(x => Column.comp(x._1, x._2))
  }

  object json {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._
    import Column.Form
    import org.goldenport.json.JsonUtils.Implicits._
    import org.goldenport.record.v2.Constraint.json.Implicits._
    import org.goldenport.record.v2.Validator.json.Implicits._

    implicit val DataTypeFormat = new Format[DataType] {
      def reads(json: JsValue): JsResult[DataType] =
        json match {
          case JsString(s) => JsSuccess(DataType.to(s))
          case m => JsError(s"Unknown element in columns: $m")
        }
      def writes(o: DataType): JsValue = JsString(o.name)
    }
    implicit val MultiplicityFormat = new Format[Multiplicity] {
      def reads(json: JsValue): JsResult[Multiplicity] =
        json match {
          case JsString(s) => JsSuccess(Multiplicity.to(s))
          case m => JsError(s"Unknown element in columns: $m")
        }
      def writes(o: Multiplicity): JsValue = JsString(o.mark)
    }
    implicit val FormFormat = Json.format[Form]
    implicit val ColumnFormat = new Format[Column] {
      def reads(json: JsValue): JsResult[Column] = {
        val name = (json \ "name").as[String]
        val datatype = (json \ "datatype").asOpt[DataType] getOrElse XString
        val multiplicity = (json \ "multiplicity").asOpt[Multiplicity] getOrElse MOne
        val label = (json \ "label").asOpt[String]
        val i18nLabel = (json \ "i18nLabel").asOpt[I18NString]
        val constraints = (json \ "constrains").asOpt[List[Constraint]] getOrElse Nil
        val form = (json \ "form").asOpt[Form] getOrElse Form.empty
        JsSuccess(
          Column(name, datatype, multiplicity,
            label = label, i18nLabel = i18nLabel,
            constraints = constraints,
            form = form
          )
        )
      }
      def writes(o: Column): JsValue = {
        val constraints: Option[JsValue] = if (o.constraints.isEmpty) None else Some(Json.toJson(o.constraints))
        JsObject(
          List(
            "name" -> JsString(o.name),
            "datatype" -> JsString(o.datatype.name),
            "multiplicity" -> JsString(o.multiplicity.mark)
          ) ++ List(
            // constraints,
            o.label.map(x => "label" -> JsString(x)),
            o.i18nLabel.map(x => "i18nLabel" -> Json.toJson(x)),
            if (o.form.isEmpty) None else Some("form" -> Json.toJson(o.form)),
            o.importer.map(x => "importer" -> x.toJson)
          ).flatten
        )
      }
    }

    implicit val SchemaFormat = new Format[Schema] {
      def reads(json: JsValue): JsResult[Schema] = {
        val columns: Seq[Column] = (json \ "columns") match {
          case JsDefined(js) => js match {
            case JsArray(xs) => xs.map(_.as[Column])
            case m => RAISE.noReachDefect(s"Unknown element in columns: $m") // JsError(s"Unknown element in columns: $m")
          }
          case m: JsUndefined => RAISE.noReachDefect // JsError(m.validationError)
        }
        val validators: Seq[Validator] = (json \ "validators") match {
          case JsDefined(js) => js match {
            case JsArray(xs) => xs.map(_.as[Validator])
            case m => RAISE.noReachDefect(s"Unknown element in columns: $m") // JsError(s"Unknown element in columns: $m")
          }
          case m: JsUndefined => RAISE.noReachDefect(s"Unknown element in columns: $m") // JsError(s"Unknown element in columns: $m")
        }
        JsSuccess(Schema(columns, validators = validators))
      }
      def writes(o: Schema): JsValue = JsObject(List(
        "columns" -> JsArray(o.columns.map(Json.toJson(_)))
      ))
    }

    def marshall(schema: Schema): String = Json.toJson(schema).toString
    def unmarshall(p: String): Schema = Json.parse(p).as[Schema]
    def unmarshall(p: JsLookupResult): Schema = p match {
      case JsDefined(js) => unmarshall(js)
      case _: JsUndefined => throw new IllegalArgumentException(p.toString)
    }
    def unmarshall(p: JsValue): Schema = SchemaFormat.reads(p) match {
      case JsSuccess(s, _) => s
      case m: JsError => throw new IllegalArgumentException(m.toString)
    }
  }

  object csv {
    def unmarshall(p: String): Schema = RAISE.notImplementedYetDefect
  }

  object record {
    def unmarshall(ps: Seq[Record]): Schema =
      Schema(ps.map(Column.record.unmarshall))
  }
}

trait ColumnSlot {
//  def toGridColumn: String
  def merge(c: Column): Option[ColumnSlot] = None
}

case class ColumnGroupSlot(config: ColumnGroup, columns: Seq[Column]) extends ColumnSlot {
  def label = config.label

//  def toGridColumn: String = {
//    val cs = columns.map(_.toGridColumn).mkString("[", ",", "]")
//    """{"xtype": "gridcolumn", "text": "%s", "columns": %s}""".format(label, cs)
//  }

  override def merge(c: Column): Option[ColumnSlot] = {
    config.columns.contains(c.name) option {
      ColumnGroupSlot(config, columns :+ c)
    }
  }
}

case class ColumnGroup(label: String, columns: Seq[String]) {
}

/*
 * Multiplicity
 */
sealed trait Multiplicity {
  def mark: String
  def label: String
}

object Multiplicity {
  def guessSeq(ps: Seq[List[Any]]): Multiplicity = {
    case class Z(empty: Int = 0, one: Int = 0, more: Int = 0) {
      def r = if (more > 0) {
        if (empty > 0)
          MOneMore
        else
          MZeroMore
      } else if (one > 0) {
        if (empty > 0)
          MZeroOne
        else
          MOne
      } else {
        MZeroOne
      }
      def +(rhs: List[Any]) =
        rhs match {
          case Nil => copy(empty = empty + 1)
          case x :: Nil => if (isMultiple(rhs))
            copy(more = more + 1)
          else
            copy(one = one + 1)
          case _ => copy(more = more + 1)
        }
    }
    ps./:(Z())(_+_).r
  }

  def guess(p: List[Any]): Multiplicity =
    if (isMultiple(p))
      MZeroMore
    else
      MZeroOne

  def isMultiple(p: List[Any]): Boolean = p match {
    case Nil => false
    case x :: Nil => x match {
      case _: Seq[_] => true
      case _: Array[_] => true
      case _: Record => true
      case _ => false
    }
    case _ => true
  }

  def get(s: String): Option[Multiplicity] = Option(s) collect {
    case MOne.`mark` => MOne
    case MZeroOne.`mark` => MZeroOne
    case MOneMore.`mark` => MOneMore
    case MZeroMore.`mark` => MZeroMore
  }

  def to(s: String): Multiplicity = get(s).getOrElse(
    RAISE.syntaxErrorFault(s"Invalid multiplicity: $s")
  )
}

case object MOne extends Multiplicity {
  val mark = "1"
  val label = "1"
}
case object MZeroOne extends Multiplicity {
  val mark = "?"
  val label = "0,1"
}
case object MOneMore extends Multiplicity {
  val mark = "+"
  val label = "1..*"
}
case object MZeroMore extends Multiplicity {
  val mark = "*"
  val label = "0..*"
}

case class MRange(from: Int, to: Int) extends Multiplicity {
  val mark = ".."
  val label = s"${from}..${to}"
}

case class MRanges(ranges: List[NonEmptyList[MRange]]) extends Multiplicity {
  val mark = "..."
  val label = s"""${ranges.mkString("[", ",", "]")}"""
}

/*
 * Order
 */
sealed trait SqlOrder {
  val sql: String

  def compare(lhs: String, rhs: String): Boolean
  def compare(lhs: Int, rhs: Int): Boolean
  def compare(lhs: Long, rhs: Long): Boolean
  def compare(lhs: Float, rhs: Float): Boolean
  def compare(lhs: Double, rhs: Double): Boolean
  def compare(lhs: BigInt, rhs: BigInt): Boolean
  def compare(lhs: BigDecimal, rhs: BigDecimal): Boolean
  def compare(lhs: Timestamp, rhs: Timestamp): Boolean
}

case object SqlOrderAsc extends SqlOrder {
  val sql = "ASC"

  def compare(lhs: String, rhs: String): Boolean = {
    lhs.compareTo(rhs) < 0
  }

  def compare(lhs: Int, rhs: Int): Boolean = lhs < rhs

  def compare(lhs: Long, rhs: Long): Boolean = {
    lhs.compareTo(rhs) < 0
  }

  def compare(lhs: Float, rhs: Float): Boolean = lhs < rhs

  def compare(lhs: Double, rhs: Double): Boolean = lhs < rhs

  def compare(lhs: BigInt, rhs: BigInt): Boolean = lhs < rhs

  def compare(lhs: BigDecimal, rhs: BigDecimal): Boolean = lhs < rhs

  def compare(lhs: Timestamp, rhs: Timestamp): Boolean = {
    lhs.compareTo(rhs) < 0
  }
}
case object SqlOrderDesc extends SqlOrder {
  val sql = "DESC"

  def compare(lhs: String, rhs: String): Boolean = {
    lhs.compareTo(rhs) > 0
  }

  def compare(lhs: Int, rhs: Int): Boolean = lhs > rhs

  def compare(lhs: Long, rhs: Long): Boolean = lhs > rhs

  def compare(lhs: Float, rhs: Float): Boolean = lhs > rhs

  def compare(lhs: Double, rhs: Double): Boolean = lhs > rhs

  def compare(lhs: BigInt, rhs: BigInt): Boolean = lhs > rhs

  def compare(lhs: BigDecimal, rhs: BigDecimal): Boolean = lhs > rhs

  def compare(lhs: Timestamp, rhs: Timestamp): Boolean = {
    lhs.compareTo(rhs) > 0
  }
}

object SqlOrder {
  def get(order: String): Option[SqlOrder] = {
    if (order.equalsIgnoreCase("asc")) Some(SqlOrderAsc)
    else if (order.equalsIgnoreCase("desc")) Some(SqlOrderDesc)
    else None
  }

  def apply(order: Option[String], otherwise: SqlOrder = SqlOrderAsc): SqlOrder = {
    order match {
      case Some(s) => {
        if (s.equalsIgnoreCase("asc")) SqlOrderAsc
        else if (s.equalsIgnoreCase("desc")) SqlOrderDesc
        else throw new IllegalArgumentException("order = " + order)
      }
      case None => otherwise
    }
  }
}

/*
 * Operation
 */
sealed trait Operation {
  def extjsColumnExtension: Seq[(String, Any)]
}
case object Locked extends Operation {
  def extjsColumnExtension: Seq[(String, Any)] = {
    List("locked" -> true)
  }
}
case object Sortable extends Operation {
  def extjsColumnExtension: Seq[(String, Any)] = {
    List("sortable" -> true)
  }
}
case object Filterable extends Operation {
  def extjsColumnExtension: Seq[(String, Any)] = {
    List("filterable" -> false)
  }
}

/*
 * Grouping
 */
case class Grouping(key: String = "group_field") {
  def convertField(field: Field) = {
    map_Field(field)
  }

  def convertRecord(record: Record) = {
    map_Record(record)
  }

  def convertRecords(records: RecordSet) = {
    map_Records(records)
  }

  protected def map_Field(implicit field: Field) = {
    field
  }

  protected def map_Record(implicit record: Record) = {
    record
  }

  protected def map_Records(implicit records: RecordSet) = {
    records
  }

  def adjustColumns(columns: Seq[Column]) = {
//    if (columns.exists(_.name == key)) columns
//    else columns :+ Column(key, "Group")
    columns
  }

/*
  def gridFeatures: Seq[JsObject] = {
    List(JsObject(List(
      "ftype" -> JsString("grouping"))))
  }

  protected final def add_group_field(value: String)(implicit row: Record): Record = {
    row ::+ (key, value)
  }
*/
}

object NullGrouping extends Grouping() {
  override def adjustColumns(columns: Seq[Column]) = columns
//  override def gridFeatures = Nil
}

sealed trait ValidationResult {
  def +(a: ValidationResult): ValidationResult
  def enkey(key: String): ValidationResult = this
  def enlabel(label: String): ValidationResult = this
  def i18nMessage: I18NString
  def message(locale: Locale): String = i18nMessage(locale)
  def isSuccess: Boolean
  def isWarning: Boolean
  def isError: Boolean
}

case object Valid extends ValidationResult {
  def +(a: ValidationResult) = {
    a match {
      case Valid => this
      case w: Warning => w
      case i: Invalid => i
    }
  }
  def i18nMessage = I18NString("Valid")

  def isSuccess: Boolean = true
  def isWarning: Boolean = false
  def isError: Boolean = false
}

case class VDescription(
  name: Option[String],
  issue: I18NString,
  value: Option[Seq[Any]]
) {
  // legacy
  lazy val message = {
    name match {
      case Some(s) =>
        value.toList match {
          case Nil => "%s: %s".format(s, issue.ja)
          case v => "%s = %s: %s".format(s, _formated_value, issue.ja)
        }
      case None =>
        value.toList match {
          case Nil => "%s".format(issue.ja)
          case v => "%s: %s".format(_formated_value, issue.ja)
        }
    }
  }

  lazy val i18nMessage: I18NString = name.fold(issue)(n => issue.update(x => s"$n: $x"))

  lazy val i18nMessageWithValue: I18NString = name.fold {
    value.toList match {
      case Nil => issue
      case xs => issue.update(x => s"$x (${_formated_value})")
    }
  } { n =>
      value.toList match {
      case Nil => issue.update(x => s"$n: $x")
      case xs => issue.update(x => s"$n: $x (${_formated_value})")
    }
  }

  private def _formated_value: String = {
    val a = value.toList match {
      case Nil => "NULL"
      case x :: Nil => AnyUtils.toString(x)
      case xs => xs.map(AnyUtils.toString).mkString(",")
    }
    Strings.cutstring(a)
  }
}

object VDescription {
  def apply(name: String, issue: String): VDescription = {
    VDescription(Some(name), I18NString(issue), None)
  }

  def apply(name: String, issue: String, value: Option[Seq[Any]]): VDescription = {
    VDescription(Some(name), I18NString(issue), value)
  }

  def apply(name: String, issue: String, value: Seq[Any]): VDescription = {
    VDescription(Some(name), I18NString(issue), if (value.isEmpty) None else Some(value))
  }

  def apply(name: Option[String], issue: String): VDescription = {
    VDescription(name, I18NString(issue), None)
  }

  def apply(name: String, issue: I18NString): VDescription =
    VDescription(Some(name), issue, None)

  def apply(name: String, issue: I18NString, value: Seq[Any]): VDescription =
    VDescription(Some(name), issue, if (value.isEmpty) None else Some(value))

  def apply(name: String, issue: I18NString, value: Option[Seq[Any]]): VDescription =
    VDescription(Some(name), issue, value)

  def apply(name: Option[String], issue: I18NString, value: Seq[Any]): VDescription =
    VDescription(name, issue, if (value.isEmpty) None else Some(value))

  def message(issue: String, value: String): VDescription =
    VDescription(None, I18NString(issue), Some(Vector(value)))

  def message(issue: I18NString, value: String): VDescription =
    VDescription(None, issue, Some(Vector(value)))
}

trait Warning extends ValidationResult {
  def +(a: ValidationResult) = {
    a match {
      case Valid => this
      case w: Warning => CompoundWarning(this +: w.asWarnings)
      case i: Invalid => CompoundFailure(i.asFailures, this.some)
    }
  }
  def +(a: Warning): Warning = {
    CompoundWarning(this +: a.asWarnings)
  }
  override def enkey(key: String): Warning = this
  override def enlabel(key: String): Warning = this

  def asWarnings: NonEmptyVector[Warning] = NonEmptyVector(this)
  def descriptions: Vector[VDescription]
  def messages: Vector[String] = descriptions.map(_.message)
  def i18nMessages: Vector[I18NString] = descriptions.map(_.i18nMessage)
  def i18nMessage: I18NString = I18NString.concat(i18nMessages)
  def displayWarnings: NonEmptyVector[Warning] = NonEmptyVector(this)

  def isSuccess: Boolean = false
  def isWarning: Boolean = true
  def isError: Boolean = false
}

case class CompoundWarning(warnings: NonEmptyVector[Warning]) extends Warning {
  override def enkey(key: String) = {
    this.copy(warnings.map(_.enkey(key)))
  }
  override def enlabel(label: String) = {
    this.copy(warnings.map(_.enlabel(label)))
  }
  override def asWarnings = warnings
  def descriptions = warnings.vector.flatMap(_.descriptions)

  override def displayWarnings: NonEmptyVector[Warning] = warnings
}

case class ValueDomainWarning(
  message: String, value: String,
  key: Option[String] = None, label: Option[String] = None
) extends Warning {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    Vector(VDescription(label orElse key, message.format(value)))
  }
}

case class DuplicateWarning(
  message: String, value: String,
  key: Option[String] = None, label: Option[String] = None
) extends Warning {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    Vector(VDescription(label orElse key, message.format(value)))
  }
}  

case class IllegalFieldWarning(key: String,
                               value: Seq[Any],
                               label: Option[String] = None,
                               message: Option[String] = None) extends Warning {
  def descriptions = Vector(VDescription(label | key, message | "値が異常です。", value))
}

case class RedundancyFieldWarning(
  key: String,
  value: Option[Seq[Any]] = None,
  label: Option[String] = None,
  message: Option[String] = None) extends Warning {
  def descriptions = Vector(VDescription(label | key, message | "余分なフィールドです。", value))
}

trait Invalid extends ValidationResult {
  def +(a: ValidationResult) = {
    a match {
      case Valid => this
      case w: Warning => CompoundFailure(
        NonEmptyVector(this),
        (getWarning.map(_ + w) | w).some)
      case i: Invalid => CompoundFailure(this +: i.asFailures)
    }
  }
  override def enkey(key: String): Invalid = this
  override def enlabel(key: String): Invalid = this

  def asFailures: NonEmptyVector[Invalid] = NonEmptyVector(this)
  def descriptions: Vector[VDescription]
  def getWarning(): Option[Warning] = None
  def messages: Vector[String] = descriptions.map(_.message)
  def i18nMessages: Vector[I18NString] = descriptions.map(_.i18nMessage)
  def i18nMessage: I18NString = I18NString.concat(i18nMessages)
  def displayWarnings: Option[NonEmptyVector[Warning]] = None
  def displayErrors: NonEmptyVector[Invalid] = NonEmptyVector(this)

  def isSuccess: Boolean = false
  def isWarning: Boolean = false
  def isError: Boolean = true
}

case class CompoundFailure(failures: NonEmptyVector[Invalid], warning: Option[Warning] = None) extends Invalid {
  override def enkey(key: String) = {
    this.copy(failures.map(_.enkey(key)))
  }
  override def enlabel(label: String) = {
    this.copy(failures.map(_.enlabel(label)))
  }
  override def asFailures = failures
  def descriptions = failures.vector.flatMap(_.descriptions)
  override def getWarning() = warning
  override def displayWarnings: Option[NonEmptyVector[Warning]] =
    warning.map(_.displayWarnings)
  override def displayErrors: NonEmptyVector[Invalid] = failures
}

case class IllegalFieldFailure(key: String,
                               value: Seq[Any],
                               label: Option[String] = None,
                               message: Option[String] = None) extends Invalid {
  def descriptions = Vector(VDescription(label | key, message + "値が異常です。", value))
}

case class MultiplicityFailure(
  multiplicity: Multiplicity,
  message: I18NString,
  key: Option[String],
  value: Option[Seq[Any]],
  label: Option[String]
) extends Invalid {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    val name = label orElse key
    // val a = multiplicity match {
    //   case MOne => VDescription(name, "値が設定されていません。")
    //   case MZeroOne => VDescription(name, "値が設定されていません。") // XXX
    //   case MOneMore => VDescription(name, "値が一つも設定されていません。")
    //   case MZeroMore => VDescription(name, "値が設定されていません。") // XXX
    //   case m: MRange => VDescription(name, "値が設定されていません。")
    //   case m: MRanges => VDescription(name, "値が設定されていません。")
    // }
    val m = message
    val a = VDescription(name, m, value)
    Vector(a)
  }
}
object MultiplicityFailure {
  def apply(multiplicity: Multiplicity, message: I18NString): MultiplicityFailure =
    MultiplicityFailure(multiplicity, message, None, None, None)

  def noData(multiplicity: Multiplicity): MultiplicityFailure = {
    MultiplicityFailure(multiplicity,
      I18NString(
        "No data available.",
        "値が設定されていません。"
      )
    )
  }
  def emptyData(multiplicity: Multiplicity): MultiplicityFailure = {
    MultiplicityFailure(multiplicity,
      I18NString(
        "Empty data.",
        "データが空です。"
      )
    )
  }
  def tooManyData(multiplicity: Multiplicity, value: Seq[String]): MultiplicityFailure =
    MultiplicityFailure(multiplicity, _too_many_data(multiplicity), None, Some(value), None)

  def tooManyData(multiplicity: Multiplicity, name: String, value: Seq[String]): MultiplicityFailure =
    MultiplicityFailure(multiplicity, _too_many_data(multiplicity), Some(name), Some(value), None)

  private def _too_many_data(multiplicity: Multiplicity): I18NString =
    I18NString(
      "Too many data.",
      "データ数が多すぎます。"
    )
}

case class DataTypeFailure(datatype: DataType, value: Seq[String], key: Option[String] = None, label: Option[String] = None) extends Invalid {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  protected final def value_label = {
    value match {
      case Nil => "NULL"
      case x :: Nil => x
      case xs => xs
    }
  }
  private final def _message = value_label + "は" + datatype.label + "ではありません。"
  private final def _i18n_message = I18NMessage(
    "{0} is not {1}.",
    "{0} は {1} ではありません。",
    Vector(value_label, datatype.label)
  ).toI18NString
  def descriptions = {
    Vector(VDescription(label orElse key, _i18n_message, value))
  }
}

object DataTypeFailure {
  def create(datatype: DataType, value: Any): DataTypeFailure = {
    value match {
      case xs: Seq[_] => DataTypeFailure(datatype, _adjust(xs))
      case _ => DataTypeFailure(datatype, List(Strings.cutstring(value.toString)))
    }
  }

  private def _adjust(xs: Seq[_]): Seq[String] = {
    def f(x: Any) = Strings.cutstring(x.toString, 100)
    if (xs.length > 5)
      xs.take(5).map(f).toVector :+ "..."
    else
      xs.map(f).toVector
  }
}

case class ValueDomainFailure(
  descriptions: Vector[VDescription],
  value: String,
  key: Option[String] = None,
  label: Option[String] = None
) extends Invalid {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  // def descriptions = {
  //   Vector(VDescription(label orElse key, message.format(value)))
  // }
}
object ValueDomainFailure {
  def apply(message: String, value: String): ValueDomainFailure = 
    ValueDomainFailure(Vector(VDescription.message(message, value)), value)

  def apply(message: I18NString, value: String): ValueDomainFailure = 
    ValueDomainFailure(Vector(VDescription.message(message, value)), value)
}

case class MissingFieldFailure(
  key: String,
  value: Seq[Any] = Nil,
  label: Option[String] = None,
  message: Option[String] = None) extends Invalid {
  def descriptions = Vector(VDescription(label | key, message | "フィールドがありません。", value))
}

/*
 * Context
 */
/*
trait Context {
  def indicatorMessage(schema: Schema): String = ""

  def convertField(schema: Schema, field: Field): Field = {
    mapField(schema, field)
  }

  def convertRecord(schema: Schema, record: Record): Record = {
    mapRecord(schema, record)
  }

  def convertRecords(schema: Schema, records: RecordSet): RecordSet = {
    mapRecords(schema, records)
  }

  protected def mapField(schema: Schema, field: Field): Field = {
    field
  }

  protected def mapRecord(schema: Schema, record: Record): Record = {
    record
  }

  protected def mapRecords(schema: Schema, records: RecordSet): RecordSet = {
    records
  }
}

case object YenMoneyContext extends Context {
  override def indicatorMessage(schema: Schema) = "　単位：円"
}

case object ThousandYenMoneyContext extends Context {
  override def indicatorMessage(schema: Schema) = "　単位：千円"

  override def convertField(schema: Schema, field: Field): Field = {
    field.mapColumnDecimal(schema, _.datatype == XMoney, _ / 1000)
  }
}
*/
