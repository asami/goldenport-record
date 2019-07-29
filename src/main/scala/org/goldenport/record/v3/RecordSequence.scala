package org.goldenport.record.v3

import org.goldenport.record.v3.sql.RecordIterator

/*
 * @since   Mar. 30, 2019
 *  version Apr.  6, 2019
 * @version Jul. 28, 2019
 * @author  ASAMI, Tomoharu
 */
case class RecordSequence(irecords: Vector[IRecord]) {
  lazy val toRecords: Vector[Record] = irecords.map(_.toRecord)
}

object RecordSequence {
  val empty = RecordSequence(Vector.empty)

  def apply(ps: Iterator[IRecord]): RecordSequence = new RecordSequence(ps.toVector)

  def createClose(iter: RecordIterator): RecordSequence = try {
    RecordSequence(iter.toVector)
  } finally {
    iter.close()
  }
}
