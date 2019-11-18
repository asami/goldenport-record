package org.goldenport.record.unitofwork.interpreter

import scalaz.{Store => _, _}, Scalaz._
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._

/*
 * @since   Nov. 15, 2015
 *  version Dec.  4, 2015
 *  version Apr. 28, 2016
 *  version Mar. 28, 2018
 *  version May. 31, 2018
 * @version Sep. 13, 2019
 * @author  ASAMI, Tomoharu
 */
trait StoreOperationLogic extends LogicBase {
  def get(store: Store, id: Store.Id): GetResult
  def getSync(store: Store, id: Store.Id): GetResult
  def getShare(store: Store, id: Store.Id): GetResult
  def getExclusive(store: Store, id: Store.Id): GetResult
  def gets(store: Store, ids: Seq[Store.Id]): GetsResult
  def getsSync(store: Store, ids: Seq[Store.Id]): GetsResult
  def getsShare(store: Store, ids: Seq[Store.Id]): GetsResult
  def getsExclusive(store: Store, ids: Seq[Store.Id]): GetsResult
  def select(store: Store, query: Query): SelectResult
  def selectSync(store: Store, query: Query): SelectResult
  def selectShare(store: Store, query: Query): SelectResult
  def selectExclusive(store: Store, query: Query): SelectResult
  def insert(store: Store, rec: Record): InsertResult
  def inserts(store: Store, rs: RecordSet): InsertsResult
  def update(store: Store, id: Store.Id, rec: Record): UpdateResult
  def updates(store: Store, rs: Map[Store.Id, Record]): UpdatesResult
  def delete(store: Store, id: Store.Id): DeleteResult
  def deletes(store: Store, ids: Seq[Store.Id]): DeletesResult
}

object StoreOperationLogic {
  val printer = new StoreOperationLogic {
    def get(store: Store, id: Store.Id): GetResult = {
      println(s"get: $store, $id")
      GetResult(None)
    }

    def getSync(store: Store, id: Store.Id): GetResult = {
      println(s"getSync: $store, $id")
      GetResult(None)
    }

    def getShare(store: Store, id: Store.Id): GetResult = {
      println(s"getShare: $store, $id")
      GetResult(None)
    }

    def getExclusive(store: Store, id: Store.Id): GetResult = {
      println(s"getExclusive: $store, $id")
      GetResult(None)
    }

    def gets(store: Store, ids: Seq[Store.Id]): GetsResult = {
      RAISE.notImplementedYetDefect
    }

    def getsSync(store: Store, ids: Seq[Store.Id]): GetsResult = {
      RAISE.notImplementedYetDefect
    }

    def getsShare(store: Store, ids: Seq[Store.Id]): GetsResult = {
      RAISE.notImplementedYetDefect
    }

    def getsExclusive(store: Store, ids: Seq[Store.Id]): GetsResult = {
      RAISE.notImplementedYetDefect
    }

    def select(store: Store, query: Query): SelectResult = {
      RAISE.notImplementedYetDefect
    }

    def selectSync(store: Store, query: Query): SelectResult = {
      RAISE.notImplementedYetDefect
    }

    def selectShare(store: Store, query: Query): SelectResult = {
      RAISE.notImplementedYetDefect
    }

    def selectExclusive(store: Store, query: Query): SelectResult = {
      RAISE.notImplementedYetDefect
    }

    def insert(store: Store, rec: Record): InsertResult = {
      println(s"insert: $store, $rec")
      InsertResult(Store.StringId("id1234"))
    }

    def inserts(store: Store, rs: RecordSet): InsertsResult = {
      RAISE.notImplementedYetDefect
    }

    def update(store: Store, id: Store.Id, rec: Record): UpdateResult = {
      RAISE.notImplementedYetDefect
    }

    def updates(store: Store, rs: Map[Store.Id, Record]): UpdatesResult = {
      RAISE.notImplementedYetDefect
    }

    def delete(store: Store, id: Store.Id): DeleteResult = {
      RAISE.notImplementedYetDefect
    }

    def deletes(store: Store, ids: Seq[Store.Id]): DeletesResult = {
      RAISE.notImplementedYetDefect
    }

    def commit(): CommitResult = {
      RAISE.notImplementedYetDefect
    }

    def abort(message: String): Unit = RAISE.notImplementedYetDefect
    def abort(e: Throwable): Unit = RAISE.notImplementedYetDefect
  }
}
