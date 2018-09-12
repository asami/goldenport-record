package org.goldenport.record.v2.unitofwork.interpreter

import java.io.IOException
import scala.collection.mutable
import org.goldenport.record.v2.{Record, RecordSet}
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Sep.  9, 2018
 * @version Sep.  9, 2018
 * @author  ASAMI, Tomoharu
 */
trait JournalStoreOperationLogic extends StoreOperationLogic {
  import JournalStoreOperationLogic._
  protected val store_journal = new UnitOfWorkStoreSetJournal()

  def commit(): CommitResult = ???
  def abort(message: String): Unit = {}
  def abort(e: Throwable): Unit = {}

  protected def generate_id(store: Store): Store.Id
  protected def get_item_store(store: Store): Option[ItemStore]

  def insert(store: Store, rec: Record): InsertResult = {
    val id = rec.getString('id).map(Store.StringId).getOrElse(generate_id(store))
    val (xs, itemids) = _complement_item_ids(store, rec)
    store_journal.insert(store, id, xs)
    InsertResult(id, itemids)
  }

  private def _complement_item_ids(
    store: Store,
    rec: Record
  ): (Record, IndexedSeq[Store.Id]) = {
    get_item_store(store) match {
      case Some(itemstore) =>
        val itemcolumn = itemstore.itemColumnName
        if (rec.isDefined(itemcolumn)) {
          val xs = rec.getRecordList(itemcolumn) map { x =>
            if (x.isDefined('id))
              x
            else
              x.updateS('id -> itemstore.generateId)
          }
          val itemids = xs.map(_.asString('id))
          val a = rec.update(itemcolumn -> xs)
          val b = itemids.map(Store.StringId)
          (a, b.toVector)
        } else {
          (rec, Vector.empty)
        }
      case None => (rec, Vector.empty)
    }
  }

  // protected def get_item_classkind(store: Store): Option[(EverforthClassKind, String)] = {
  //   val repo = to_repository(store)
  //   repo match {
  //     case m: EverforthRepositoryBase =>
  //       m.wholeConfig.map(c => (c.classKind, c.itemColumn))
  //     case _ => None
  //   }
  // }

  def inserts(store: Store, rs: RecordSet): InsertsResult = {
    val a = rs.records.toVector.map(x => insert(store, x))
    InsertsResult(a)
  }

  def update(store: Store, id: Store.Id, rec: Record): UpdateResult = {
    val (xs, itemids) = _complement_item_ids(store, rec)
    store_journal.update(store, id, xs)
    UpdateResult(id, itemids)
  }

  def updates(store: Store, rs: Map[Store.Id, Record]): UpdatesResult = {
    val a = rs.toVector.map {
      case (id, rec) => update(store, id, rec)
    }
    UpdatesResult(a)
  }

  def delete(store: Store, id: Store.Id): DeleteResult = {
    store_journal.delete(store, id)
    DeleteResult(id)
  }

  def deletes(store: Store, ids: Seq[Store.Id]): DeletesResult = {
    store_journal.deletes(store, ids)
    DeletesResult(ids.toVector)
  }
}

object JournalStoreOperationLogic {
  // TODO migrate to record
  trait ItemStore {
    def itemColumnName: String
    def generateId(): String
  }
}
