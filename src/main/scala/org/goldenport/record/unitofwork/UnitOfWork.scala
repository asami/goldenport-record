package org.goldenport.record.unitofwork

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 * @version Dec.  4, 2015
 * @author  ASAMI, Tomoharu
 */
sealed trait UnitOfWork[+A] {
}

trait ExtensionUnitOfWork[+A] extends UnitOfWork[A]

case class Value[T](v: T) extends UnitOfWork[T]

case class InvokeService(request: UnitOfWork.ServiceRequest) extends UnitOfWork[UnitOfWork.ServiceResponse] {
}

object UnitOfWork {
  type UnitOfWorkFM[T] = Free.FreeC[UnitOfWork, T]

  def lift[T[_] <: UnitOfWork[_], A](x: T[A]) = Free.liftFC(x)
  def lift[T](x: T): UnitOfWorkFM[T] = Free.liftFC(Value(x))

  trait ServiceRequest
  trait ServiceResponse

  def invoke[T <: ServiceResponse](req: ServiceRequest): UnitOfWorkFM[T] =
    Free.liftFC(InvokeService(req)).asInstanceOf[UnitOfWorkFM[T]]

  def runFM[F[_], O](
    f: UnitOfWorkFM[O]
  )(interpreter: NaturalTransformation[UnitOfWork, F]
  )(implicit F: Monad[F]): F[O] = {
    Free.runFC(f)(interpreter)
  }

  def runId[O](
    f: UnitOfWorkFM[O]
  )(interpreter: NaturalTransformation[UnitOfWork, Id]): O = {
    runFM(f)(interpreter)
  }

  def runTask[O](
    f: UnitOfWorkFM[O]
  )(interpreter: NaturalTransformation[UnitOfWork, Task]): Task[O] = {
    runFM(f)(interpreter)
  }

  object store {
    def get(store: Store, id: Store.Id) = StoreOperation.get(store, id).asInstanceOf[UnitOfWorkFM[GetResult]]

    def gets(store: Store, ids: Seq[Store.Id]) = StoreOperation.gets(store, ids).asInstanceOf[UnitOfWorkFM[GetsResult]]

    def select(store: Store, query: Query) = StoreOperation.select(store, query).asInstanceOf[UnitOfWorkFM[SelectResult]]

    def insert(
      store: Store,
      rec: Record
    ): UnitOfWorkFM[InsertResult] = StoreOperation.insert(store, rec).asInstanceOf[UnitOfWorkFM[InsertResult]]

    def inserts(
      store: Store,
      rs: RecordSet
    ): UnitOfWorkFM[IndexedSeq[InsertResult]] = StoreOperation.inserts(store, rs).asInstanceOf[UnitOfWorkFM[IndexedSeq[InsertResult]]]

    def update(
      store: Store,
      id: Store.Id,
      rec: Record
    ): UnitOfWorkFM[UpdateResult] = StoreOperation.update(store, id, rec).asInstanceOf[UnitOfWorkFM[UpdateResult]]

    def update(
      store: Store,
      id: String,
      rec: Record
    ): UnitOfWorkFM[UpdateResult] = StoreOperation.update(store, id, rec).asInstanceOf[UnitOfWorkFM[UpdateResult]]

    def updates(
      store: Store, rs: Map[Store.Id, Record]
    ) = StoreOperation.updates(store, rs).asInstanceOf[UnitOfWorkFM[Unit]]

    def delete(
      store: Store, id: String
    ) = StoreOperation.delete(store, id).asInstanceOf[UnitOfWorkFM[Unit]]

    def deletes(
      store: Store,
      ids: Seq[Store.Id]
    ) = StoreOperation.deletes(store, ids).asInstanceOf[UnitOfWorkFM[Unit]]

    def commit() = StoreOperation.commit().asInstanceOf[UnitOfWorkFM[CommitResult]]
  }
}
