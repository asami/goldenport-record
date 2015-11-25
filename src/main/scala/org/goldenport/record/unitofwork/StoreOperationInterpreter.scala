package org.goldenport.record.unitofwork

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 * @version Nov. 24, 2015
 * @author  ASAMI, Tomoharu
 */
trait StoreOperationInterpreter[+F[_]] extends NaturalTransformation[StoreOperation, F] {
  def apply[T](op: StoreOperation[T]): F[T] = {
    op match {
      case Get(store, id) => get(store, id)
      case Gets(store, ids) => gets(store, ids)
      case Select(store, query) => select(store, query)
      case Insert(store, rec) => insert(store, rec)
      case Inserts(store, rs) => inserts(store, rs)
      case Update(store, id, rec) => update(store, id, rec)
      case Updates(store, rs) => updates(store, rs)
      case Delete(store, id) => delete(store, id)
      case Deletes(store, ids) => deletes(store, ids)
    }
  }

  def get[T](store: Store, id: Store.Id): F[T]
  def gets[T](store: Store, ids: Seq[Store.Id]): F[T]
  def select[T](store: Store, query: String): F[T]
  def insert[T](store: Store, rec: Record): F[T]
  def inserts[T](store: Store, rs: RecordSet): F[T]
  def update[T](store: Store, id: Store.Id, rec: Record): F[T]
  def updates[T](store: Store, rs: Map[Store.Id, Record]): F[T]
  def delete[T](store: Store, id: Store.Id): F[T]
  def deletes[T](store: Store, ids: Seq[Store.Id]): F[T]
}

class IdStoreOperationInterpreter(
  logic: StoreOperationLogic
) extends StoreOperationInterpreter[Id] {
  def get[T](store: Store, id: Store.Id): T = {
    logic.get(store, id).asInstanceOf[T]
  }

  def gets[T](store: Store, ids: Seq[Store.Id]): T = {
    logic.gets(store, ids).asInstanceOf[T]
  }

  def select[T](store: Store, query: String): T = {
    logic.select(store, query).asInstanceOf[T]
  }

  def insert[T](store: Store, rec: Record): T = {
    logic.insert(store, rec).asInstanceOf[T]
  }

  def inserts[T](store: Store, rs: RecordSet): T = {
    logic.inserts(store, rs).asInstanceOf[T]
  }

  def update[T](store: Store, id: Store.Id, rec: Record): T = {
    logic.update(store, id, rec).asInstanceOf[T]
  }

  def updates[T](store: Store, rs: Map[Store.Id, Record]): T = {
    logic.updates(store, rs).asInstanceOf[T]
  }

  def delete[T](store: Store, id: Store.Id): T = {
    logic.delete(store, id).asInstanceOf[T]
  }

  def deletes[T](store: Store, ids: Seq[Store.Id]): T = {
    logic.deletes(store, ids).asInstanceOf[T]
  }
}

trait StoreOperationInterpreterBase[F[_]] extends StoreOperationInterpreter[F] {
  def typeclass: Applicative[F]
  def logic: StoreOperationLogic

  def get[T](store: Store, id: Store.Id): F[T] = {
    typeclass.point(logic.get(store, id).asInstanceOf[T])
  }

  def gets[T](store: Store, ids: Seq[Store.Id]): F[T] = {
    typeclass.point(logic.gets(store, ids).asInstanceOf[T])
  }

  def select[T](store: Store, query: String): F[T] = {
    typeclass.point(logic.select(store, query).asInstanceOf[T])
  }

  def insert[T](store: Store, rec: Record): F[T] = {
    typeclass.point(logic.insert(store, rec).asInstanceOf[T])
  }

  def inserts[T](store: Store, rs: RecordSet): F[T] = {
    typeclass.point(logic.inserts(store, rs).asInstanceOf[T])
  }

  def update[T](store: Store, id: Store.Id, rec: Record): F[T] = {
    typeclass.point(logic.update(store, id, rec).asInstanceOf[T])
  }

  def updates[T](store: Store, rs: Map[Store.Id, Record]): F[T] = {
    typeclass.point(logic.updates(store, rs).asInstanceOf[T])
  }

  def delete[T](store: Store, id: Store.Id): F[T] = {
    typeclass.point(logic.delete(store, id).asInstanceOf[T])
  }

  def deletes[T](store: Store, ids: Seq[Store.Id]): F[T] = {
    typeclass.point(logic.deletes(store, ids).asInstanceOf[T])
  }
}

class TaskStoreOperationInterpreter(
  val logic: StoreOperationLogic
) extends StoreOperationInterpreterBase[Task] {
  def typeclass: Applicative[Task] = Task.taskInstance
}

object StoreOperationInterpreter {
  def log = new IdStoreOperationInterpreter(StoreOperationLogic.printer)
  def logTask = new TaskStoreOperationInterpreter(StoreOperationLogic.printer)
}
