package org.goldenport.record.v3.sql

import java.sql.ResultSet
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.Record

/*
 * @since   Mar. 23, 2019
 * @version Mar. 23, 2019
 * @author  ASAMI, Tomoharu
 */
class ResultSetRecordIterator(
  schema: Schema,
  resultset: ResultSet
) extends Iterator[Record] {
  def close() {
    resultset.close()
  }

  def hasNext: Boolean = resultset.next()
  def next(): Record = SqlUtils.resultSetToRecord(schema, resultset)
}

object ResultSetRecordIterator {
  def create(schema: Schema, rs: ResultSet): ResultSetRecordIterator = new ResultSetRecordIterator(schema, rs)

  def create(rs: ResultSet): ResultSetRecordIterator =
    create(SqlUtils.makeSchema(rs), rs)
}
