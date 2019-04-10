package org.goldenport.record.v3

import org.goldenport.record.v3.sql.RecordIterator

/*
 * @since   Mar. 30, 2019
 * @version Apr.  6, 2019
 * @author  ASAMI, Tomoharu
 */
case class RecordSequence(vector: Vector[Record]) {
}

object RecordSequence {
  def createClose(iter: RecordIterator): RecordSequence = try {
    RecordSequence(iter.toVector)
  } finally {
    iter.close()
  }
}
