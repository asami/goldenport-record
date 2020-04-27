package org.goldenport.record.v3.sql

import scalaz._, Scalaz._
import java.util.{Date, TimeZone}
import java.sql.Timestamp
import org.joda.time._
import org.goldenport.RAISE
import org.goldenport.record.sql.SqlDatatype
import org.goldenport.record.store
import org.goldenport.record.v2.{Schema, Column, SqlColumn, DataType}
import org.goldenport.record.v3._
import org.goldenport.record.command.ValueCommand
import org.goldenport.record.util.DateUtils

/*
 * @since   Apr.  6, 2019
 *  version May.  9, 2019
 *  version Jul. 15, 2019
 *  version Oct.  9, 2019
 *  version Nov. 27, 2019
 * @version Mar. 29, 2020
 * @author  ASAMI, Tomoharu
 */
class SqlBuilder(
  tableName: String,
  idColumn: Option[String],
  schema: Option[Schema],
  sqlContext: SqlContext // Database variation point.
) {
  import SqlBuilder._

  protected def table_name = tableName
  protected val table_name_literal = sqlContext.syntax.tableNameLiteral(tableName)
  protected def id_column = idColumn orElse schema.flatMap(_.columns.find(_.sql.isId).map(_.name)) getOrElse("id")

  protected def column_name_literal(p: String) = sqlContext.syntax.columnNameLiteral(p)

  def createTable(): String = s"""CREATE TABLE $table_name_literal ${createSchema}"""

  def createSchema(): String = schema.map { s =>
    val columns = s.columns.filterNot(_.isDerived).map(_column)
    val constraints = s.columns.filterNot(_.isDerived).flatMap(_constraint)
    (columns ++ constraints).mkString("(", ", ", ")")
  }.getOrElse(RAISE.illegalStateFault("No schema"))

  private def _column(c: Column): String = {
    val sqlcolumn = c.sql
    Vector(
      Some(s"${column_name_literal(c.name)} ${_datatype(c, sqlcolumn)}"),
      _multiplicity(c),
      _id(c, sqlcolumn),
      _auto(c, sqlcolumn),
      _unique(c, sqlcolumn),
      _default(c, sqlcolumn)
    ).flatten.mkString(" ")
  }

  private def _datatype(c: Column, sql: SqlColumn): String =
    sql.datatype.map(_.fullName).getOrElse(_datatype(c.datatype))

  private def _datatype(p: DataType): String = _sqldatatype(p).fullName

  // TODO customizable
  private def _sqldatatype(p: DataType): SqlDatatype = {
    import org.goldenport.record.sql._
    import org.goldenport.record.v2._
    val string = VARCHAR(1024)
    val token = VARCHAR(256)
    p match {
      case XBoolean => BOOLEAN
      case XByte => TINYINT
      case XShort => TINYINT
      case XInt => INT
      case XLong => BIGINT
      case XFloat => FLOAT
      case XFloat1 => FLOAT
      case XDouble => DOUBLE
      case XInteger => INTEGER
      case XDecimal => DECIMAL
      case XString => string
      case XNonEmptyString => string
      case XNonBlankString => string
      case XToken => token
      case XNonEmptyToken => token
      case XNonBlankToken => token
      case XText => CLOB
      case XNonEmptyText => CLOB
      case XNonBlankText => CLOB
      case XDate => DATE
      case XTime => TIME
      case XDateTime => TIMESTAMP
      case XBase64 => BLOB
      case XBinary => BLOB
      case XLink => token
      case XImageLink => token
      case XEMail => token
      case XMoney => DECIMAL
      case XPercent => FLOAT
      case XUnit => token
      case XColor => token
      case XFile => token
      case XMonth => TINYINT
      case XMonthDay => token
      case XYear => TINYINT
      case XYearEffective => TINYINT
      case XYearPast => TINYINT
      case XYearMonth => token
      case XDay => TINYINT
      case XDuration => token
      case XPassword => token
      case XSearch => token
      case XTel => token
      case XWeek => token
      case XUuid => token
      case XEntityId => token
      case XEverforthid => token
      case XXml => CLOB
      case XHtml => CLOB
      case XRecordInstance => CLOB
      case m: XEntityReference => token
      case m: XRange => token
      case m: XValue => token
      case m: XEverforthObjectReference => token
      case m: XPowertype => token
      case m: XPowertypeReference => token
      case m: XStateMachine => string
      case m: XStateMachineReference => token
      case m: XExternalDataType => token // XXX
    }
  }

  private def _multiplicity(c: Column): Option[String] =
    if (c.isSingle)
      Some("NOT NULL")
    else
      None

  private def _id(c: Column, sql: SqlColumn): Option[String] =
    if (true)
      if (sql.isId) Some("PRIMARY KEY") else None
    else
      None

  private def _auto(c: Column, sql: SqlColumn): Option[String] =
    if (sql.isAutoId) Some("AUTO INCREMENT") else None

  private def _unique(c: Column, sql: SqlColumn): Option[String] =
    if (sql.isUnique) Some("UNIQUE") else None

  private def _default(c: Column, sql: SqlColumn): Option[String] =
    Option(sql.value).map(v =>
      sql.datatype.map(d =>
        if (d.isStringLiteral)
          s"DEFAULT '$v'"
        else
          s"DEFAULT $v"
      ).getOrElse(
        if (c.datatype.isSqlString)
          s"DEFAULT '$v'"
        else
          s"DEFAULT $v"
      )
    )

  private def _constraint(c: Column): Option[String] = {
    val sqlcolumn = c.sql
    val pk = if (true)
      None
    else
      if (sqlcolumn.isId) Some("PRIMARY_KEY") else None
    List(pk).flatten match {
      case Nil => None
      case xs => Some(xs.mkString(" "))
    }
  }

  def dropTable(): String = s"""DROP TABLE $table_name_literal"""

  // TODO derived attribute
  private def _select_columns = schema.
    map(_.columns.filterNot(_.sql.isDerived).
      map(x => column_name_literal(x.sqlColumnName)).mkString(", ")).
    getOrElse("*")

  def get(id: Any): String = {
    val idliteral = id match {
      case m: store.Id => m.literal
      case m => m
    }
    s"""SELECT ${_select_columns} FROM ${table_name_literal} WHERE ${id_column} = ${literal(idliteral)}"""
  }

  def query(q: String): String = {
    s"""SELECT ${_select_columns} FROM ${table_name_literal} WHERE ${q}"""
  }

  def insert(p: IRecord): String = {
    val rec = p.toRecord
    val idoption = rec.get(id_column)
    val columns = insertColumns(schema, rec)
    val values = insertValues(schema, rec)
    s"""INSERT INTO ${table_name_literal} $columns VALUES ($values)"""
  }

  def inserts(rs: Seq[IRecord]): String = RAISE.notImplementedYetDefect

  def update(id: Any, p: IRecord): String = {
    val rec = p.toRecord
    val values = updateValues(schema, rec.removeField(id_column))
    s"""UPDATE ${table_name_literal} SET $values WHERE id = ${literal(id)}"""
  }

  def delete(id: Any): String = {
    s"""DELETE FROM ${table_name_literal} WHERE id = ${literal(id)}"""
  }

  def literal(p: Any): String = p match {
    case SingleValue(v) => literal(v)
    case m: MultipleValue => RAISE.notImplementedYetDefect
    case m: ValueCommand => m.getSqlLiteralForSet getOrElse RAISE.noReachDefect
    case m: String => literalString(m)
    case m: Boolean => m.toString
    case m: Char => s"'$m'"
    case m: Byte => m.toString
    case m: Short => m.toString
    case m: Int => m.toString
    case m: Long => m.toString
    case m: Float => m.toString
    case m: Double => m.toString
    case m: BigInt => m.toString
    case m: BigDecimal => m.toString
    case m: Timestamp => literalString(m.toString) // GMT
    case m: Date => literalString(DateUtils.toIsoDateString(m)) // ISO Date/GMT
    case m: DateTime => literalString(m.toString)
    case m: LocalDate => literalString(m.toString)
    case m: LocalDateTime => RAISE.notImplementedYetDefect
    case m: store.Id => literalString(m.string)
    case m => literalString(m)
  }

  def literalString(p: Any): String = "'" + escape(p.toString) + "'"

  def insertColumns(schema: Option[Schema], rec: Record): String =
    schema.map(insertColumns(_, rec)).getOrElse(insertColumns(rec))

  def insertColumns(schema: Schema, rec: Record): String =
    schema.columns.flatMap(x =>
      _is_match(x, rec) option column_name_literal(x.sqlColumnName)
    ).mkString("(", ", ", ")")

  private def _is_match(c: Column, rec: Record) = {
    val names = rec.fields.map(_.key.name)
    names.contains(c.name) || c.label.map(names.contains).getOrElse(false) ||
    c.i18nLabel.map(_.containsKey(names)).getOrElse(false)
  }

  def insertColumns(rec: Record): String = rec.fields.map(_.key.name).mkString("(", ", ",")")

  def insertValues(schema: Option[Schema], rec: Record): String =
    schema.map(insertValues(_, rec)).getOrElse(insertValues(rec))

  def insertValues(schema: Schema, rec: Record): String = {
    case class Z(xs: Vector[String] = Vector.empty) {
      def r = xs.mkString(", ")
      def +(rhs: Column) =
        _get_field(rhs, rec).
          map(x => copy(xs = xs :+ _value(rhs.datatype, x))).
          getOrElse(this)
    }
    schema.columns./:(Z())(_+_).r
  }

  private def _get_field(c: Column, rec: Record): Option[Field] = rec.fields.find(_is_match(c, _))

  private def _is_match(c: Column, f: Field) = {
    val name = f.key.name
    c.name == name || c.label.map(_ == name).getOrElse(false) ||
    c.i18nLabel.map(_.containsKey(name)).getOrElse(false)
  }

  private def _value(datatype: DataType, p: Field): String = p.value match {
    case SingleValue(v) => literal(datatype.toInstance(v))
    case MultipleValue(v) => RAISE.notImplementedYetDefect
    case EmptyValue => "NULL"
  }

  def insertValues(rec: Record): String = rec.fields.map(_.value).map(literal).mkString(", ")

  def updateValues(schema: Option[Schema], rec: Record): String =
    schema.map(updateValues(_, rec)).getOrElse(updateValues(rec))

  def updateValues(schema: Schema, rec: Record): String = {
    case class Z(xs: Vector[String] = Vector.empty) {
      def r = xs.mkString(", ")
      def +(rhs: Column) =
        rec.getField(rhs.name).
          map(x => copy(xs = xs :+ _update_value(rhs, x))).
          getOrElse(this)
    }
    schema.columns./:(Z())(_+_).r
  }

  private def _update_value(c: Column, p: Field): String = {
    val v = p.value match {
      case SingleValue(v) => literal(c.datatype.toInstance(v))
      case MultipleValue(v) => RAISE.notImplementedYetDefect
      case EmptyValue => "NULL"
    }
    s"${column_name_literal(c.name)} = $v"
  }

  def updateValues(rec: Record): String = rec.fields.map(x =>
    s"${column_name_literal(x.key.name)} = ${literal(x.value)}"
  ).mkString(", ")
}

object SqlBuilder {
  def apply(tablename: String): SqlBuilder =
    new SqlBuilder(tablename, None, None, SqlContext.empty)

  def apply(tablename: String, idcolumn: String): SqlBuilder =
    new SqlBuilder(tablename, Some(idcolumn), None, SqlContext.empty)

  def apply(tablename: String, idcolumn: String, schema: Option[Schema]): SqlBuilder =
    new SqlBuilder(tablename, Some(idcolumn), schema, SqlContext.empty)

  def create(tablename: String, schema: Schema): SqlBuilder = {
    new SqlBuilder(tablename, None, Some(schema), SqlContext.empty)
  }

  def escape(s: String): String = 
    if (s.indexOf("'") == -1)
      s.replace("\\", "\\\\")
    else
      s.replace("'", "''").replace("\\", "\\\\")
}
