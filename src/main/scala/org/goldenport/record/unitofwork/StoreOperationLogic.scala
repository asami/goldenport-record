package org.goldenport.record.unitofwork

import scalaz._, Scalaz._
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 * @version Nov. 25, 2015
 * @author  ASAMI, Tomoharu
 */
trait StoreOperationLogic {
  def get(store: Store, id: Store.Id): Option[Record]
  def gets(store: Store, ids: Seq[Store.Id]): RecordSet
  def select(store: Store, query: String): RecordSet
  def insert(store: Store, rec: Record): Store.Id
  def inserts(store: Store, rs: RecordSet): IndexedSeq[Store.Id]
  def update(store: Store, id: Store.Id, rec: Record): Unit
  def updates(store: Store, rs: Map[Store.Id, Record]): Unit
  def delete(store: Store, id: Store.Id): Unit
  def deletes(store: Store, ids: Seq[Store.Id]): Unit
}

object StoreOperationLogic {
  val printer = new StoreOperationLogic {
    def get(store: Store, id: Store.Id): Option[Record] = {
      println(s"get: $store, $id")
      None
    }

    def gets(store: Store, ids: Seq[Store.Id]): RecordSet = {
      ???
    }

    def select(store: Store, query: String): RecordSet = {
      ???
    }

    def insert(store: Store, rec: Record): Store.Id = {
      println(s"insert: $store, $rec")
      Store.StringId("id1234")
    }

    def inserts(store: Store, rs: RecordSet): IndexedSeq[Store.Id] = {
      ???

    }

    def update(store: Store, id: Store.Id, rec: Record): Unit = {
      ???

    }

    def updates(store: Store, rs: Map[Store.Id, Record]): Unit = {
      ???

    }

    def delete(store: Store, id: Store.Id): Unit = {
      ???

    }

    def deletes(store: Store, ids: Seq[Store.Id]): Unit = {
      ???

    }
  }
}
