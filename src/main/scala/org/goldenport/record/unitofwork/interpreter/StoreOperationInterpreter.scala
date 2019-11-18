package org.goldenport.record.unitofwork.interpreter

import scala.language.higherKinds
import scalaz.{Store => _, _}, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._

/*
 * @since   Nov. 15, 2015
 *  version Dec.  3, 2015
 *  version Apr. 28, 2016
 *  version Mar. 28, 2018
 * @version Sep. 13, 2019
 * @author  ASAMI, Tomoharu
 */
trait StoreOperationInterpreter[+F[_]] extends NaturalTransformation[StoreOperation, F] {
  def abort(message: String): Unit = {
    interpreter_Abort(message)
  }
  def abort(e: Throwable): Unit = {
    interpreter_Abort(e)
  }

  protected def interpreter_Abort(message: String): Unit
  protected def interpreter_Abort(e: Throwable): Unit

  def apply[T](op: StoreOperation[T]): F[T] = {
    op match {
      case Get(store, id) => get(store, id)
      case GetSync(store, id) => getSync(store, id)
      case GetShare(store, id) => getShare(store, id)
      case GetExclusive(store, id) => getExclusive(store, id)
      case Gets(store, ids) => gets(store, ids)
      case GetsSync(store, ids) => getsSync(store, ids)
      case GetsShare(store, ids) => getsShare(store, ids)
      case GetsExclusive(store, ids) => getsExclusive(store, ids)
      case Select(store, query) => select(store, query)
      case SelectSync(store, query) => selectSync(store, query)
      case SelectShare(store, query) => selectShare(store, query)
      case SelectExclusive(store, query) => selectExclusive(store, query)
      case Insert(store, rec) => insert(store, rec)
      case Inserts(store, rs) => inserts(store, rs)
      case Update(store, id, rec) => update(store, id, rec)
      case Updates(store, rs) => updates(store, rs)
      case Delete(store, id) => delete(store, id)
      case Deletes(store, ids) => deletes(store, ids)
      case Commit() => commit()
    }
  }

  def get[T](store: Store, id: Store.Id): F[T]
  def getSync[T](store: Store, id: Store.Id): F[T]
  def getShare[T](store: Store, id: Store.Id): F[T]
  def getExclusive[T](store: Store, id: Store.Id): F[T]
  def gets[T](store: Store, ids: Seq[Store.Id]): F[T]
  def getsSync[T](store: Store, ids: Seq[Store.Id]): F[T]
  def getsShare[T](store: Store, ids: Seq[Store.Id]): F[T]
  def getsExclusive[T](store: Store, ids: Seq[Store.Id]): F[T]
  def select[T](store: Store, query: Query): F[T]
  def selectSync[T](store: Store, query: Query): F[T]
  def selectShare[T](store: Store, query: Query): F[T]
  def selectExclusive[T](store: Store, query: Query): F[T]
  def insert[T](store: Store, rec: Record): F[T]
  def inserts[T](store: Store, rs: RecordSet): F[T]
  def update[T](store: Store, id: Store.Id, rec: Record): F[T]
  def updates[T](store: Store, rs: Map[Store.Id, Record]): F[T]
  def delete[T](store: Store, id: Store.Id): F[T]
  def deletes[T](store: Store, ids: Seq[Store.Id]): F[T]
  def commit[T](): F[T]
}

class IdStoreOperationInterpreter(
  logic: StoreOperationLogic
) extends StoreOperationInterpreter[Id] {
  protected def interpreter_Abort(message: String) = logic.abort(message)
  protected def interpreter_Abort(e: Throwable) = logic.abort(e)

  def get[T](store: Store, id: Store.Id): T = {
    logic.get(store, id).asInstanceOf[T]
  }

  def getSync[T](store: Store, id: Store.Id): T = {
    logic.getSync(store, id).asInstanceOf[T]
  }

  def getShare[T](store: Store, id: Store.Id): T = {
    logic.getShare(store, id).asInstanceOf[T]
  }

  def getExclusive[T](store: Store, id: Store.Id): T = {
    logic.getExclusive(store, id).asInstanceOf[T]
  }

  def gets[T](store: Store, ids: Seq[Store.Id]): T = {
    logic.gets(store, ids).asInstanceOf[T]
  }

  def getsSync[T](store: Store, ids: Seq[Store.Id]): T = {
    logic.getsSync(store, ids).asInstanceOf[T]
  }

  def getsShare[T](store: Store, ids: Seq[Store.Id]): T = {
    logic.getsShare(store, ids).asInstanceOf[T]
  }

  def getsExclusive[T](store: Store, ids: Seq[Store.Id]): T = {
    logic.getsExclusive(store, ids).asInstanceOf[T]
  }

  def select[T](store: Store, query: Query): T = {
    logic.select(store, query).asInstanceOf[T]
  }

  def selectSync[T](store: Store, query: Query): T = {
    logic.selectSync(store, query).asInstanceOf[T]
  }

  def selectShare[T](store: Store, query: Query): T = {
    logic.selectShare(store, query).asInstanceOf[T]
  }

  def selectExclusive[T](store: Store, query: Query): T = {
    logic.selectExclusive(store, query).asInstanceOf[T]
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

  def commit[T](): T = {
    logic.commit().asInstanceOf[T]
  }
}

trait StoreOperationInterpreterBase[F[_]] extends StoreOperationInterpreter[F] {
  def typeclass: Applicative[F]
  def logic: StoreOperationLogic

  protected def interpreter_Abort(message: String) = logic.abort(message)
  protected def interpreter_Abort(e: Throwable) = logic.abort(e)

  def get[T](store: Store, id: Store.Id): F[T] = {
    typeclass.point(logic.get(store, id).asInstanceOf[T])
  }

  def getSync[T](store: Store, id: Store.Id): F[T] = {
    typeclass.point(logic.getSync(store, id).asInstanceOf[T])
  }

  def getShare[T](store: Store, id: Store.Id): F[T] = {
    typeclass.point(logic.getShare(store, id).asInstanceOf[T])
  }

  def getExclusive[T](store: Store, id: Store.Id): F[T] = {
    typeclass.point(logic.getExclusive(store, id).asInstanceOf[T])
  }

  def gets[T](store: Store, ids: Seq[Store.Id]): F[T] = {
    typeclass.point(logic.gets(store, ids).asInstanceOf[T])
  }

  def getsSync[T](store: Store, ids: Seq[Store.Id]): F[T] = {
    typeclass.point(logic.getsSync(store, ids).asInstanceOf[T])
  }

  def getsShare[T](store: Store, ids: Seq[Store.Id]): F[T] = {
    typeclass.point(logic.getsShare(store, ids).asInstanceOf[T])
  }

  def getsExclusive[T](store: Store, ids: Seq[Store.Id]): F[T] = {
    typeclass.point(logic.getsExclusive(store, ids).asInstanceOf[T])
  }

  def select[T](store: Store, query: Query): F[T] = {
    typeclass.point(logic.select(store, query).asInstanceOf[T])
  }

  def selectSync[T](store: Store, query: Query): F[T] = {
    typeclass.point(logic.selectSync(store, query).asInstanceOf[T])
  }

  def selectShare[T](store: Store, query: Query): F[T] = {
    typeclass.point(logic.selectShare(store, query).asInstanceOf[T])
  }

  def selectExclusive[T](store: Store, query: Query): F[T] = {
    typeclass.point(logic.selectExclusive(store, query).asInstanceOf[T])
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

  def commit[T](): F[T] = {
    typeclass.point(logic.commit().asInstanceOf[T])
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
