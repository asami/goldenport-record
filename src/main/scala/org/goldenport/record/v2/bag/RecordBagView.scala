package org.goldenport.record.v2.bag

import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import org.goldenport.bag.ChunkBag
import org.goldenport.record.v2._
import org.goldenport.record.v2.projector.Projector

/*
 * @since   Oct.  8, 2015
 *  version Nov. 11, 2015
 *  version Aug. 31, 2016
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
trait RecordBagView {
  def strategy: RecordBag.Strategy
  def headerPolicy: RecordBag.HeaderPolicy = strategy.headerPolicy
  def name: String
  def getSchema: Option[Schema]
  def chunkBag: ChunkBag
  def recordsR: Process[Task, Record]
  def dataRecordsR: Process[Task, Record] = {
    if (headerPolicy.physical)
      recordsR.drop(1)
    else
      recordsR
  }
  def headerDataRecordsR: Process[Task, Record] = ???
  def dataRecordsR(schema: Schema, severe: Boolean): Process[Task, \/[ValidationResult, Record]] = {
    import Projector.Policy
    val policy = if (severe) Policy.rigid else Policy.updateForm
    dataRecordsR.map(Projector(schema, policy).apply)
  }
  def records: IndexedSeq[Record] = recordsR.runLog.run
  def headerDataRecords: IndexedSeq[Record] = headerDataRecordsR.runLog.run
  def dataRecords: IndexedSeq[Record] = dataRecordsR.runLog.run
}

case class FlatRecordBagView(
  strategy: RecordBag.Strategy,
  bag: RecordBag
) extends RecordBagView {
  def name: String = bag.name
  def getSchema: Option[Schema] = bag.getSchema
  def chunkBag: ChunkBag = bag.chunkBag
  def recordsR: Process[Task, Record] = bag.recordsR
}
