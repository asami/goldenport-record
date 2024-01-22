package org.goldenport.record.v3.sql

import scala.util.Try
import java.sql.ResultSet
import org.goldenport.record.v2.{Schema => V2Schema}
import org.goldenport.record.v3.Schema
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.RecordBuilder
import org.goldenport.record.v3.SchemaMaker

/*
 * @since   May. 25, 2019
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
class ConnectionResultSetRecordIterator(
  schema: Schema,
  resultset: ResultSet,
  builder: RecordBuilder
) extends RecordIterator {
  private def _connection = resultset.getStatement.getConnection

  def close() {
    Try(resultset.close())
    Try(_connection.close())
  }

  def hasNext: Boolean = resultset.next()
  def next(): Record = builder.build(resultset) // SqlUtils.resultSetToRecord(schema, resultset)
}

object ConnectionResultSetRecordIterator {
  def create(schema: Schema, rs: ResultSet): ConnectionResultSetRecordIterator = new ConnectionResultSetRecordIterator(schema, rs, RecordBuilder(schema))

  def create(schema: V2Schema, rs: ResultSet): ConnectionResultSetRecordIterator =
    create(Schema.from(schema), rs)

  def create(rs: ResultSet): ConnectionResultSetRecordIterator =
    // create(SqlUtils.makeSchema(rs), rs)
    create(SchemaMaker.make(rs), rs)
}
