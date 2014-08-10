package org.goldenport.record.v2

import java.sql.Timestamp
import scalaz._, Scalaz._
import org.smartdox.Description

/*
 * Add
 * Column/curd
 */
/*
 * @snice   Nov. 23, 2012
 *  version Dec. 28, 2012
 *  version Jan. 30, 2013
 *  version Mar. 12, 2013
 *  version Apr. 26, 2013
 *  version Jun. 24, 2013
 *  version Oct. 23, 2013
 *  version Feb.  6, 2014
 *  version Jun.  9, 2014
 *  version Jul. 25, 2014
 * @version Aug.  6, 2014
 * @author  ASAMI, Tomoharu
 */
case class Schema(
  columns: Seq[Column],
  columnGroups: Seq[ColumnGroup] = Nil,
  grouping: Grouping = NullGrouping,
  validators: Seq[Validator] = Nil,
  sql: SqlSchema = NullSqlSchema,
//  contexts: Seq[Context] = Nil,
//  view: GuiView = ExtjsGridView(),
//  charts: Seq[Chart] = Nil,
  formatter: Option[FormatterGroup] = None,
  pageSize: Option[Int] = 100.some,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  isCsvTitle: Option[Boolean] = None,
//  comment: String = "",
//  history: String = ""
  desc: Description = Description() // TODO .empty
) {
  import scalaz.syntax.foldable._
  implicit object ValidationResultMonoid extends Monoid[ValidationResult] {
    def append(f1: ValidationResult, f2: => ValidationResult) = f1 + f2
    def zero: ValidationResult = Valid
  } // TODO uses Validation.ValidationResultMonoid

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

  final def addColumns(cs: Seq[Column]): Schema = {
    copy(columns = this.columns ++ cs)
  }

  protected def map_Record(record: Record): Record = {
    record
  }

  protected def map_Records(records: RecordSet): RecordSet = {
    records
  }

  val gridColumns = columns.filter(_.visibility.grid).filter(_.isSingle)

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
    columns.find(_.name == f.key.name) match {
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

  /*
   * Validation
   */
  def validate(rs: RecordSet): ValidationResult = {
    rs.records.map(validate).toVector.suml
  }

  def validate0(r: Record): ValidationResult = {
    import scalaz.syntax.foldable._
    val a = _validate_redumental_fields(r)
    if (a.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + a)
    val b = _validate_missing_fields(r)
    if (b.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + b)
    val c = _validate_datatype(r)
    if (c.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + c)
    val d = _validate_validators(r)     
    if (d.isInstanceOf[Invalid]) throw sys.error("Schema#validate = " + d)
    List(a, b, c, d).toVector.suml
  }

  def validate(r: Record): ValidationResult = {
    List(_validate_redumental_fields(r),
         _validate_missing_fields(r),
         _validate_datatype(r),
         _validate_validators(r)
       ).toVector.suml
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
      f => getColumn(f.key).map(c => _validate_datatype_column(c, f))
    ).toVector.suml
  }

  private def _validate_datatype_column(c: Column, f: Field): ValidationResult = {
    val a: Seq[ValidationResult] = f.values.map(
      _ match {
        case xs: Seq[_] => sys.error("???")
        case x => c.datatype.validate(x).enkey(f.key.name)
      }
    )
    a.toVector.suml
  }

  private def _validate_validators(r: Record): ValidationResult = {
    Valid // TODO
  }

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
   * Formatter
   */
  def format(columnname: String, rec: Record): String = {
    val v = rec.get(columnname)
    columns.find(_.name == columnname) map { c =>
      formatter.flatMap(_.format(c, v)) getOrElse c.format(v)
    } getOrElse {
      throw new IllegalArgumentException(s"Illegal column name = $columnname")
    }
  }
}

object NullSchema extends Schema(Nil)

object Schema {
  val empty = NullSchema
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

/*
 * Order
 */
sealed trait SqlOrder {
  val sql: String

  def compare(lhs: String, rhs: String): Boolean
  def compare(lhs: Long, rhs: Long): Boolean
  def compare(lhs: Timestamp, rhs: Timestamp): Boolean
}

case object SqlOrderAsc extends SqlOrder {
  val sql = "ASC"

  def compare(lhs: String, rhs: String): Boolean = {
    lhs.compareTo(rhs) < 0
  }

  def compare(lhs: Long, rhs: Long): Boolean = {
    lhs.compareTo(rhs) < 0
  }

  def compare(lhs: Timestamp, rhs: Timestamp): Boolean = {
    lhs.compareTo(rhs) < 0
  }
}
case object SqlOrderDesc extends SqlOrder {
  val sql = "DESC"

  def compare(lhs: String, rhs: String): Boolean = {
    lhs.compareTo(rhs) > 0
  }

  def compare(lhs: Long, rhs: Long): Boolean = {
    lhs.compareTo(rhs) < 0
  }

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

case class VDescription(name: String, issue: String, value: Seq[Any] = Nil, label: Option[String] = None) {
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
  def descriptions: Vector[VDescription]
  def messages: Vector[String] = descriptions.map(_.message)
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
    Vector(VDescription((label orElse key) | "", message.format(value)))
  }
}

case class DuplicateWarning(
  message: String, value: String,
  key: Option[String] = None, label: Option[String] = None
) extends Warning {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    Vector(VDescription((label orElse key) | "", message.format(value)))
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
  value: Seq[Any] = Nil,
  label: Option[String] = None,
  message: Option[String] = None) extends Warning {
  def descriptions = Vector(VDescription(label | key, message | "余分なフィールドです。", value))
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
  def descriptions: Vector[VDescription]
  def getWarning(): Option[Warning] = None
  def messages: Vector[String] = descriptions.map(_.message)
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
                               value: Seq[Any],
                               label: Option[String] = None,
                               message: Option[String] = None) extends Invalid {
  def descriptions = Vector(VDescription(label | key, message + "値が異常です。", value))
}

case class MultiplicityFailure(multiplicity: Multiplicity, msg: String, key: Option[String] = None, label: Option[String] = None) extends Invalid {
  override def enkey(key: String) = this.copy(key = key.some)
  override def enlabel(label: String) = this.copy(label = label.some)
  def descriptions = {
    val name = (label orElse key) | ""
    val a = multiplicity match {
      case MOne => VDescription(name, "値が設定されていません。")
      case MZeroOne => VDescription(name, "値が設定されていません。") // XXX
      case MOneMore => VDescription(name, "値が一つも設定されていません。")
      case MZeroMore => VDescription(name, "値が設定されていません。") // XXX
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
    Vector(VDescription((label orElse key) | "", message, value))
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
    Vector(VDescription((label orElse key) | "", message.format(value)))
  }
}

case class MissingFieldFailure(
  key: String,
  value: Seq[Any] = Nil,
  label: Option[String] = None,
  message: Option[String] = None) extends Invalid {
  def descriptions = Vector(VDescription(label | key, message | "フィールドがありません。", value))
}

trait Validator {
  def validateField(field: Field): ValidationResult
  def validateRecord(record: Record): ValidationResult
  def validateRecords(records: Seq[Record]): Seq[Record]
}

trait Validations {
//  implicit def ValidationResultZero: Zero[ValidationResult] = zero(Valid)
//  implicit def ValidationResultSemigroup: Semigroup[ValidationResult] = semigroup((a, b) => a + b)
  implicit object ValidationResultMonoid extends Monoid[ValidationResult] {
    def append(f1: ValidationResult, f2: => ValidationResult) = f1 + f2
    def zero: ValidationResult = Valid
  }
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
                           predicate: Seq[Any] => Boolean,
                           failure: Boolean = true,
                           message: Option[String] = None) extends FieldValidator {
  def validateField(field: Field) = {
    val k = field.key
    val v = field.values
    if (keys.contains(k)) {
      if (predicate(v)) Valid
      else {
        if (failure) IllegalFieldFailure(k.name, v, none, message)
        else IllegalFieldWarning(k.name, v, none, message)
      }
    } else Valid
  }
}

case object FieldMatchValidator {
  def apply(key: String, value: String) = {
    new FieldsMatchValidator(List(key), (x: Seq[Any]) => {
      x match {
        case Nil => false
        case x :: Nil if x == value => true
        case _ => false
      }
    })
  }

  def apply(key: String, values: Seq[String]) = {
    new FieldsMatchValidator(List(key), (x: Seq[Any]) => {
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
    new FieldsMatchValidator(List(key), (x: Seq[Any]) => {
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
