package org.goldenport.record.v3.sql

import scala.util.Try
import java.sql.ResultSet
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.Record

/*
 * @since   May. 25, 2019
 * @version May. 25, 2019
 * @author  ASAMI, Tomoharu
 */
class ConnectionResultSetRecordIterator(
  schema: Schema,
  resultset: ResultSet
) extends RecordIterator {
  private def _connection = resultset.getStatement.getConnection

  def close() {
    Try(resultset.close())
    Try(_connection.close())
  }

  def hasNext: Boolean = resultset.next()
  def next(): Record = SqlUtils.resultSetToRecord(schema, resultset)
}

object ConnectionResultSetRecordIterator {
  def create(schema: Schema, rs: ResultSet): ConnectionResultSetRecordIterator = new ConnectionResultSetRecordIterator(schema, rs)

  def create(rs: ResultSet): ConnectionResultSetRecordIterator =
    create(SqlUtils.makeSchema(rs), rs)
}
