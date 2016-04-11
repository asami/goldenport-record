package org.goldenport.record.unitofwork.interpreter

import scalaz.{Store => _, _}, Scalaz._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._

/*
 * @since   Nov. 15, 2015
 * @version Dec.  4, 2015
 * @author  ASAMI, Tomoharu
 */
trait StoreOperationLogic {
  def get(store: Store, id: Store.Id): GetResult
  def gets(store: Store, ids: Seq[Store.Id]): GetsResult
  def select(store: Store, query: Query): SelectResult
  def insert(store: Store, rec: Record): InsertResult
  def inserts(store: Store, rs: RecordSet): InsertsResult
  def update(store: Store, id: Store.Id, rec: Record): UpdateResult
  def updates(store: Store, rs: Map[Store.Id, Record]): UpdatesResult
  def delete(store: Store, id: Store.Id): Unit // DeleteResult
  def deletes(store: Store, ids: Seq[Store.Id]): Unit // DeletesResult
  def commit(): CommitResult
}

object StoreOperationLogic {
  val printer = new StoreOperationLogic {
    def get(store: Store, id: Store.Id): GetResult = {
      println(s"get: $store, $id")
      GetResult(None)
    }

    def gets(store: Store, ids: Seq[Store.Id]): GetsResult = {
      ???
    }

    def select(store: Store, query: Query): SelectResult = {
      ???
    }

    def insert(store: Store, rec: Record): InsertResult = {
      println(s"insert: $store, $rec")
      InsertResult(Store.StringId("id1234"))
    }

    def inserts(store: Store, rs: RecordSet): InsertsResult = {
      ???
    }

    def update(store: Store, id: Store.Id, rec: Record): UpdateResult = {
      ???
    }

    def updates(store: Store, rs: Map[Store.Id, Record]): UpdatesResult = {
      ???
    }

    def delete(store: Store, id: Store.Id): Unit = {
      ???
    }

    def deletes(store: Store, ids: Seq[Store.Id]): Unit = {
      ???
    }

    def commit(): CommitResult = {
      ???
    }
  }
}
