package org.goldenport.record.v2.unitofwork.interpreter

import java.io.IOException
import scala.collection.mutable
import org.goldenport.record.v2.Record
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Apr.  3, 2018
 *  version May. 31, 2018
 * @version Oct. 17, 2018
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

  // TODO to sort through sequence number to handle right-hand-side folding in monad evaluation.
  def update(id: Store.Id, rec: Record): Unit = {
    val j = _objects.get(id) match {
      case Some(s) => s.update(rec)
      case None => Update(id, rec)
    }
    _objects.put(id, j)
    _history.append(j)
  }

  def delete(id: Store.Id): Unit = {
    val j = Delete(id)
    _objects.put(id, j)
    _history.append(j)
  }

  def deletes(ids: Seq[Store.Id]): Unit = {
    ids.foreach(delete)
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
    def update(rec: Record) = copy(record = record.update(record)) // TODO rec, in port of sequence number sorting.
    def toLogRecord: Record = Record.dataApp(
      "kind" -> "update",
      "id" -> id,
      "record" -> record
    )
  }
  case class Delete(id: Store.Id) extends JournalEntry {
    val record = Record.empty
    def update(rec: Record) = throw new IOException(s"UnitOfWorkStoreJournal: Already deleted: $id")
    def toLogRecord: Record = Record.dataApp(
      "kind" -> "delete",
      "id" -> id
    )
  }
}
