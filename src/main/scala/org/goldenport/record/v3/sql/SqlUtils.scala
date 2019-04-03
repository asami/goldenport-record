package org.goldenport.record.v3.sql

import java.sql.{ResultSet, ResultSetMetaData}
import org.goldenport.RAISE
import org.goldenport.Strings.totokens
import org.goldenport.record.v2.{Schema, Column, DataType}
import org.goldenport.record.v3.{Record, Field}

/*
 * @since   Mar. 23, 2019
 * @version Mar. 23, 2019
 * @author  ASAMI, Tomoharu
 */
object SqlUtils {
  def makeSchema(rs: ResultSet): Schema = {
    val meta = rs.getMetaData
    val n = meta.getColumnCount()
    case class Z(cs: Vector[Column] = Vector.empty) {
      def r = Schema(cs)

      def +(rhs: Int) = {        
        val name = meta.getColumnName(rhs)
        val dt = makeDataType(meta, rhs)
        totokens(name, ".") match {
          case Nil => this // no reach
          case x :: Nil => Z(cs :+ _make_column(name, dt, meta.getColumnLabel(rhs)))
          case x =>
            val a = x.last
            if (_is_exists(a))
              Z(cs :+ _make_column(name, dt, meta.getColumnLabel(rhs)))
            else
              Z(cs :+ _make_column(name, dt, meta.getColumnLabel(rhs), a))
        }
      }

      private def _is_exists(p: String) =
        cs.exists(x => x.name == p || x.aliases.contains(p))

      private def _make_column(
        name: String,
        datatype: DataType,
        aliases: String*
      ): Column = {
        val as = aliases.map(Option(_)).flatten.filterNot(_ == name).distinct.toList
        Column(name, datatype, aliases = as)
      }
    }
    (1 to n)./:(Z())(_+_).r
  }

  def makeDataType(meta: ResultSetMetaData, i: Int): DataType = {
    import org.goldenport.record.v2._
    val datatype = meta.getColumnType(i)
    val classname = meta.getColumnClassName(i)
    XString // TODO
  }

  def resultSetToRecord(rs: ResultSet): Record =
    resultSetToRecord(makeSchema(rs), rs)

  def resultSetToRecord(schema: Schema, rs: ResultSet): Record = {
    val a = schema.columns.flatMap(getField(_, rs))
    Record(a)
  }

  def getField(column: Column, rs: ResultSet): Option[Field] =
    (column.name +: column.aliases).toStream.flatMap(x =>
      rs.getObject(x) match {
        case null => None
        case m => Some(Field.create(column.name, m))
      }
    ).headOption

  def iterator(conn: java.sql.Connection, schema: Schema, sql: String): Iterator[Record] = {
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery(sql) // avoid anorm SQL parsing bug
    ResultSetRecordIterator.create(schema, rs)
  }

  def iterator(conn: java.sql.Connection, sql: String): Iterator[Record] = {
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery(sql) // avoid anorm SQL parsing bug
    ResultSetRecordIterator.create(rs)
  }

  def fold[T](conn: java.sql.Connection, schema: Schema, sql: String)(f: Iterator[Record] => T): T =
    f(iterator(conn, schema, sql))

  def fold[T](conn: java.sql.Connection, sql: String)(f: Iterator[Record] => T): T =
    f(iterator(conn, sql))
}
