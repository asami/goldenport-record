package org.goldenport.record.v2.jdbc

import java.util.Date
import java.sql._
import org.goldenport.record.v2._

/*
 * @since   Jun.  1, 2018
 * @version Jun.  1, 2018
 * @author  ASAMI, Tomoharu
 */
class SchemaResultSet(schema: Schema, rs: ResultSet) extends ScalaResultSet(rs) {
  override def fetch(): Record = {
    val r = schema.columns.map(c => c.name -> _fetch(c.name, c))
    Record.create(r)
  }

  def fetch(name: String): Option[Any] = schema.getColumn(name) match {
    case Some(c) => _fetch(name, c)
    case None => getObject(name)
  }

  private def _fetch(name: String, column: Column): Option[Any] =
    column.datatype match {
      case XBoolean => getBoolean(name)
      case XByte => getByte(name)
      case XShort => getShort(name)
      case XInt => getInt(name)
      case XLong => getLong(name)
      case XFloat => getFloat(name)
      case XFloat1 => getFloat(name)
      case XDouble => getDouble(name)
      case XInteger => getString(name)
      case XDecimal => getBigDecimal(name)
      case XString => getString(name)
      case XText => getString(name)
      case XToken => getString(name)
      case XDate => getDate(name)
      case XTime => getTime(name) // caution: format for Time '216:00:00' in column 127
      case XDateTime => getTimestamp(name)
      case XYear => getInt(name)
      case XMonth => getInt(name)
      case XDay => getInt(name)
      case _ => getObject(name)
    }
}

// TODO migrate to scala-lib
class ScalaResultSet(rs: ResultSet) {
  private def _column_names(rs: ResultSet): IndexedSeq[String] = {
    val meta = rs.getMetaData
    val count = meta.getColumnCount
    for (i <- 1 to count) yield {
      meta.getColumnName(i)
    }
  }

  def fetch(): Record = {
    val columns = _column_names(rs)
    val r = columns.map(c => c -> getObject(c))
    Record.create(r)
  }

  def getString(name: String): Option[String] = {
    val r = rs.getString(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getBoolean(name: String): Option[Boolean] = {
    val r = rs.getBoolean(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getByte(name: String): Option[Byte] = {
    val r = rs.getByte(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getShort(name: String): Option[Short] = {
    val r = rs.getShort(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getInt(name: String): Option[Int] = {
    val r = rs.getInt(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getLong(name: String): Option[Long] = {
    val r = rs.getLong(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getFloat(name: String): Option[Float] = {
    val r = rs.getFloat(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getDouble(name: String): Option[Double] = {
    val r = rs.getDouble(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getBigDecimal(name: String): Option[BigDecimal] = {
    val r = rs.getBigDecimal(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getDate(name: String): Option[Date] = {
    val r = rs.getDate(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getTime(name: String): Option[Time] = {
    val r = rs.getTime(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getTimestamp(name: String): Option[Timestamp] = {
    val r = rs.getTimestamp(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }

  def getObject(name: String): Option[Object] = {
    val r = rs.getObject(name)
    if (rs.wasNull)
      None
    else
      Option(r)
  }
}

