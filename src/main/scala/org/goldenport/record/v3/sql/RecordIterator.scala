package org.goldenport.record.v3.sql

import org.goldenport.record.v3.Record

/*
 * @since   Apr.  5, 2019
 * @version Apr.  6, 2019
 * @author  ASAMI, Tomoharu
 */
trait RecordIterator extends Iterator[Record] {
  def close(): Unit
}
