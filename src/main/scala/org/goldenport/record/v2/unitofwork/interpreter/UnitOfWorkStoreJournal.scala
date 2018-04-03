package org.goldenport.record.v2.unitofwork.interpreter

import java.io.IOException
import scala.collection.mutable
import org.goldenport.record.v2.Record
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Apr.  3, 2018
 * @version Apr.  3, 2018
 * @author  ASAMI, Tomoharu
 */
class UnitOfWorkStoreJournal(val store: Store) {
  import UnitOfWorkStoreJournal._
  private val _objects = new mutable.LinkedHashMap[Store.Id, JournalEntry]
  private val _history = new mutable.ArrayBuffer[JournalEntry]

  def get(id: Store.Id): Option[Record] = _objects.get(id).map(_.record)

  def insert(id: Store.Id, rec: Record): Unit = {
    val j = Insert(id, rec)
    _objects.get(id) match {
      case Some(s) => throw new IOException(s"UnitOfWorkStoreJournal: Already exists: $id")
      case None => _objects.put(id, j)
    }
    _history.append(j)
  }

  def update(id: Store.Id, rec: Record): Unit = {
    val j = _objects.get(id) match {
      case Some(s) => s.update(rec)
      case None => Update(id, rec)
    }
    _objects.put(id, j)
    _history.append(j)
  }

  def commitHistory(): Vector[JournalEntry] = {
    _objects.values.toVector
  }
}

object UnitOfWorkStoreJournal {
  sealed trait JournalEntry {
    def record: Record
    def update(rec: Record): JournalEntry
    def toLogRecord: Record
  }
  case class Insert(id: Store.Id, record: Record) extends JournalEntry {
    def update(rec: Record) = copy(record = record.update(rec))
    def toLogRecord: Record = Record.dataApp(
      "kind" -> "insert",
      "id" -> id,
      "record" -> record
    )
  }
  case class Update(id: Store.Id, record: Record) extends JournalEntry {
    def update(rec: Record) = copy(record = record.update(record))
    def toLogRecord: Record = Record.dataApp(
      "kind" -> "update",
      "id" -> id,
      "record" -> record
    )
  }
}
