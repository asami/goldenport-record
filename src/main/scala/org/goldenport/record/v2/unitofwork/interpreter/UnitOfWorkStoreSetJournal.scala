package org.goldenport.record.v2.unitofwork.interpreter

import scala.collection.mutable
import org.goldenport.record.v2.Record
import org.goldenport.record.v2.util.RecordAux
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Apr.  3, 2018
 *  version Apr.  3, 2018
 * @version May. 31, 2018
 * @author  ASAMI, Tomoharu
 */
class UnitOfWorkStoreSetJournal() {
  import UnitOfWorkStoreSetJournal._
  private val _journal = new mutable.LinkedHashMap[Store, UnitOfWorkStoreJournal]

  private def _store(s: Store): UnitOfWorkStoreJournal = {
    _journal.get(s) getOrElse {
      val a = new UnitOfWorkStoreJournal(s)
      _journal.put(s, a)
      a
    }
  }

  def get(store: Store, id: Store.Id): Option[Record] =
    _store(store).get(id)

  def insert(store: Store, id: Store.Id, rec: Record): Unit =
    _store(store).insert(id, rec)

  def update(store: Store, id: Store.Id, rec: Record): Unit =
    _store(store).update(id, rec)

  def delete(store: Store, id: Store.Id): Unit =
    _store(store).delete(id)

  def deletes(store: Store, ids: Seq[Store.Id]): Unit =
    _store(store).deletes(ids)

  def commitHistory(): CommitHistory = {
    val a = _journal.toVector map {
      case (s, j) => s -> j.commitHistory()
    }
    CommitHistory(a)
  }
}

object UnitOfWorkStoreSetJournal {
  import UnitOfWorkStoreJournal._

  case class CommitHistory(
    journals: Vector[(Store, Vector[JournalEntry])]
  ) {
    def toRecord = Record.dataApp(
      "stores" -> journals.map {
        case (store, js) => Record.dataApp(
          "store" -> store, //.name
          "journals" -> js.map(_.toLogRecord)
        )
      }
    )
    def toLog = RecordAux.toJsonString(toRecord)
  }
}
