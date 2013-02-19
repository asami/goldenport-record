package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._

/*
 * Add
 * Column/curd
 */
/*
 * @snice   Nov. 23, 2012
 *  version Dec. 28, 2012
 *  version Jan. 30, 2013
 * @version Feb. 20, 2013
 * @author  ASAMI, Tomoharu
 */
case class Schema(
  columns: Seq[Column],
  columnGroups: Seq[ColumnGroup] = Nil,
  grouping: Grouping = NullGrouping,
  validations: Seq[Validator] = Nil,
  contexts: Seq[Context] = Nil,
//  view: GuiView = ExtjsGridView(),
//  sql: SqlSchema = NullSqlSchema,
//  charts: Seq[Chart] = Nil,
  pageSize: Option[Int] = 100.some //,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  isCsvTitle: Option[Boolean] = None,
//  comment: String = "",
//  history: String = ""
) {
  def convertField(field: Field): Field = {
    mapField(field)
  }

  def convertRecord(record: Record): Record = {
    mapRecord(record)
  }

  def convertRecords(records: RecordSet): RecordSet = {
    mapRecords(records)
  }

  protected def mapField(field: Field): Field = {
    mapDataType(field)
  }

  protected def mapDataType(field: Field): Field = {
    getColumn(field.key) match {
      case Some(c) => field.values match {
        case Nil => field
        case v => Field(field.key, v.map(c.datatype.mapData))
      }
      case None => field
    }
  }

  final def getColumn(key: Symbol) = {
    columns.find(_.name == key)
  }

  final def getIdColumn: Option[Column] = {
    columns.find(_.sql.isId)
  }

  final def idColumn: Column = {
    getIdColumn match {
      case Some(c) => c
      case None => throw new IllegalStateException("no id column")
    }
  }

  protected def mapRecord(record: Record): Record = {
    record
  }

  protected def mapRecords(records: RecordSet): RecordSet = {
    records
  }

  val gridColumns = columns.filter(_.visibility.grid).filter(_.isSingle)
}

object NullSchema extends Schema(Nil)

trait ColumnSlot {
  def toGridColumn: String
  def merge(c: Column): Option[ColumnSlot] = None
}

case class ColumnGroupSlot(config: ColumnGroup, columns: Seq[Column]) extends ColumnSlot {
  def label = config.label

  def toGridColumn: String = {
    val cs = columns.map(_.toGridColumn).mkString("[", ",", "]")
    """{"xtype": "gridcolumn", "text": "%s", "columns": %s}""".format(label, cs)
  }

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
sealed trait Multiplicity
case object MOne extends Multiplicity
case object MZeroOne extends Multiplicity
case object MOneMore extends Multiplicity
case object MZeroMore extends Multiplicity

/*
 * Order
 */
sealed trait OrderBy {
  val sql: String
}
case object OrderByAsc extends OrderBy {
  val sql = "ASC"
}
case object OrderByDesc extends OrderBy {
  val sql = "DESC"
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
    mapField(field)
  }

  def convertRecord(record: Record) = {
    mapRecord(record)
  }

  def convertRecords(records: RecordSet) = {
    mapRecords(records)
  }

  protected def mapField(implicit field: Field) = {
    field
  }

  protected def mapRecord(implicit record: Record) = {
    record
  }

  protected def mapRecords(implicit records: RecordSet) = {
    records
  }

  def adjustColumns(columns: Seq[Column]) = {
    if (columns.exists(_.name == key)) columns
    else columns :+ Column(key, "Group")
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
  override def gridFeatures = Nil
}

sealed trait ValidationResult {
  def +(a: ValidationResult): ValidationResult
  def enkey(key: String): ValidationResult = this
  def enlabel(label: String): ValidationResult = this
}

case object Valid extends ValidationResult {
  def +(a: ValidationResult) = {
    a match {
      case Valid => this
      case w: Warning => w
      case i: Invalid => i
    }
  }
}

case class Description(name: String, issue: String, value: Seq[String] = Nil, label: Option[String] = None) {
  def message = {
    value match {
      case Nil => "%s: %s".format(name, issue)
      case v => "%s = %s: %s".format(name, v, issue)
    }
  }
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

  def asWarnings: Vector[Warning] = Vector(this)
  def descriptions: Vector[Description]
}

case class CompoundWarning(warnings: Vector[Warning]) extends Warning {
  override def enkey(key: String) = {
    this.copy(warnings.map(_.enkey(key)))
  }
  override def enlabel(label: String) = {
    this.copy(warnings.map(_.enlabel(label)))
  }
  override def asWarnings = warnings
  def descriptions = warnings.flatMap(_.descriptions)
}

case class ValueDomainWarning(
  message: String, value: String,
  key: Option[String] = None, label: Option[String] = None
) extends Warning {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    Vector(Description((label orElse key) | "", message.format(value)))
  }
}

case class DuplicateWarning(
  message: String, value: String,
  key: Option[String] = None, label: Option[String] = None
) extends Warning {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    Vector(Description((label orElse key) | "", message.format(value)))
  }
}  

case class IllegalFieldWarning(key: String,
                               value: Seq[String],
                               label: Option[String] = None,
                               message: Option[String] = None) extends Warning {
  def descriptions = Vector(Description(label | key, message | "値が異常です。", value))
}

trait Invalid extends ValidationResult {
  def +(a: ValidationResult) = {
    a match {
      case Valid => this
      case w: Warning => CompoundFailure(
        Vector(this),
        (getWarning.map(_ + w) | w).some)
      case i: Invalid => CompoundFailure(this +: i.asFailures)
    }
  }
  override def enkey(key: String): Invalid = this
  override def enlabel(key: String): Invalid = this

  def asFailures: Vector[Invalid] = Vector(this)
  def descriptions: Vector[Description]
  def getWarning(): Option[Warning] = None
}

case class CompoundFailure(failures: Vector[Invalid], warning: Option[Warning] = None) extends Invalid {
  override def enkey(key: String) = {
    this.copy(failures.map(_.enkey(key)))
  }
  override def enlabel(label: String) = {
    this.copy(failures.map(_.enlabel(label)))
  }
  override def asFailures = failures
  def descriptions = failures.flatMap(_.descriptions)
  override def getWarning() = warning
}

case class IllegalFieldFailure(key: String,
                               value: Seq[String],
                               label: Option[String] = None,
                               message: Option[String] = None) extends Invalid {
  def descriptions = Vector(Description(label | key, message + "値が異常です。", value))
}

case class MultiplicityFailure(multiplicity: Multiplicity, msg: String, key: Option[String] = None, label: Option[String] = None) extends Invalid {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    val name = (label orElse key) | ""
    val a = multiplicity match {
      case MOne => Description(name, "値が設定されていません。")
      case MZeroOne => Description(name, "値が設定されていません。") // XXX
      case MOneMore => Description(name, "値が一つも設定されていません。")
      case MZeroMore => Description(name, "値が設定されていません。") // XXX
    }
    Vector(a)
  }
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
  protected final def message = value_label + "は" + datatype.label + "ではありません。"
  def descriptions = {
    Vector(Description((label orElse key) | "", message, value))
  }
}

case class ValueDomainFailure(
  message: String,
  value: String,
  key: Option[String] = None, label: Option[String] = None
) extends Invalid {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    Vector(Description((label orElse key) | "", message.format(value)))
  }
}

trait Validator {
  def validateField(field: Field): ValidationResult
  def validateRecord(record: Record): ValidationResult
  def validateRecords(records: Seq[Record]): Seq[Record]
}

trait Validations {
  implicit def ValidationResultZero: Zero[ValidationResult] = zero(Valid)
  implicit def ValidationResultSemigroup: Semigroup[ValidationResult] = semigroup((a, b) => a + b)
}

case object Validator extends Validations {
}

trait FieldValidator extends Validator {
  def validateRecord(record: Record): ValidationResult = Valid
  def validateRecords(records: Seq[Record]): Seq[Record] = records
}

trait RecordValidator extends Validator {
  def validateField(field: Field): ValidationResult = Valid
  def validateRecords(records: Seq[Record]): Seq[Record] = records
}

trait RecordsValidator extends Validator {
  def validateField(field: Field): ValidationResult = Valid
  def validateRecord(record: Record): ValidationResult = Valid
}

class FieldsMatchValidator(keys: Seq[String],
                           predicate: Seq[String] => Boolean,
                           failure: Boolean = true,
                           message: Option[String] = None) extends FieldValidator {
  def validateField(field: Field) = {
    val k = field.key
    val v = field.value
    if (keys.contains(k)) {
      if (predicate(v)) Valid
      else {
        if (failure) IllegalFieldFailure(k, v, none, message)
        else IllegalFieldWarning(k, v, none, message)
      }
    } else Valid
  }
}

case object FieldMatchValidator {
  def apply(key: String, value: String) = {
    new FieldsMatchValidator(List(key), (x: Seq[String]) => {
      x match {
        case Nil => false
        case x :: Nil if x == value => true
        case _ => false
      }
    })
  }

  def apply(key: String, values: Seq[String]) = {
    new FieldsMatchValidator(List(key), (x: Seq[String]) => {
      x match {
        case Nil => false
        case x :: Nil if values.contains(x) => true
        case _ => false
      }
    })
  }
}

case object FieldContainsValidator {
  def apply(key: String, values: Seq[String], failure: Boolean = true) = {
    new FieldsMatchValidator(List(key), (x: Seq[String]) => {
      values.contains(x.contains _)
//      x.flatMap(a => values.find(a.contains)) ? false | true
    }, failure, (values.mkString("(", ",", ")") + "を含んでいます。").some)
  }
}  

/*
case class DuplicateIdValidator(key: String, label: String = "") extends RecordsValidator {
  val effectivelabel = if (label != "") label else key

  def validateRecords(records: Seq[Record]): Seq[Record] = {
    val (_, dups) = records.filter(!_.isReferenceData).foldRight((Set[String](), List[String]())) {
      (x, a) => val (keys, dups) = a
        x.getOne(key) match {
          case Some(v) => {
            if (keys.contains(v)) (keys, v :: dups)
            else (keys + v, dups)
          }
          case None => a
        }
    }
    for (a <- records) yield {
      a.getOne(key) match {
        case Some(v) if dups.contains(v) && !a.isReferenceData => a.enwarning(
          DuplicateWarning("%sが重複しています。".format(effectivelabel), v))
        case _ => a
      }
    }
  }
}

case class DuplicateCompositeIdValidator(ids: Seq[String], labels: Seq[String] = Nil) extends RecordsValidator {
  val effectivelabels = ids.zipAll(labels, "", "").foldRight(nil[String])((x, a) => {
    (if (x._2 != "") x._2 else x._1) :: a
  })

  def validateRecords(records: Seq[Record]): Seq[Record] = {
    val (_, dups) = records.filter(!_.isReferenceData).foldRight((Set[Seq[Option[String]]](), Seq[Seq[Option[String]]]())) {
      (x, a) => {
        val (keys, dups) = a
        val vs = ids.map(x.getOne)
        if (keys.contains(vs)) (keys, vs +: dups)
        else (keys + vs, dups)
      }
    }
    for (b <- records) yield {
      val vs = ids.map(b.get)
      if (dups.contains(vs) && !b.isReferenceData) {
        b.enwarning(
          DuplicateWarning("%sの組が重複しています。".format(effectivelabels.mkString("(", ",", ")")), vs.toString))
      } else b
    }
  }
}
*/

/*
 * Context
 */
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
