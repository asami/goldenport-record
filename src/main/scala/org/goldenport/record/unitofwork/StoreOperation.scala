package org.goldenport.record.unitofwork

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 * @version Nov. 25, 2015
 * @author  ASAMI, Tomoharu
 */
sealed trait StoreOperation[+A] extends UnitOfWork[A] {
}

case class Get(store: Store, id: Store.Id) extends StoreOperation[Option[Record]]

case class Gets(store: Store, ids: Seq[Store.Id]) extends StoreOperation[RecordSet]

case class Select(store: Store, query: String) extends StoreOperation[RecordSet]

case class Insert(store: Store, rec: Record) extends StoreOperation[Store.Id]

case class Inserts(store: Store, rs: RecordSet) extends StoreOperation[IndexedSeq[Store.Id]]

case class Update(store: Store, id: Store.Id, rec: Record) extends StoreOperation[Unit]

case class Updates(store: Store, rs: Map[Store.Id, Record]) extends StoreOperation[Unit]

case class Delete(store: Store, id: Store.Id) extends StoreOperation[Unit]

case class Deletes(store: Store, ids: Seq[Store.Id]) extends StoreOperation[Unit]

object StoreOperation {
  type StoreOperationFM[T] = Free.FreeC[StoreOperation, T]
  type StoreOperationFMRaw[T] = Free[({type λ[α] = Coyoneda[StoreOperation, α]})#λ, T]

  def get(store: Store, id: Store.Id) = Free.liftFC(Get(store, id))

  def gets(store: Store, ids: Seq[Store.Id]) = Free.liftFC(Gets(store, ids))

  def insert(
    store: Store,
    rec: Record
  ): StoreOperationFM[Store.Id] = Free.liftFC(Insert(store, rec))

  def inserts(
    store: Store,
    rs: RecordSet
  ): StoreOperationFM[IndexedSeq[Store.Id]] = Free.liftFC(Inserts(store, rs))

  def update(store: Store, id: Store.Id, rec: Record) = Free.liftFC(Update(store, id, rec))

  def update(store: Store, id: String, rec: Record) = Free.liftFC(Update(store, Store.StringId(id), rec))

  def updates(store: Store, rs: Map[Store.Id, Record]) = Free.liftFC(Updates(store, rs))

  def updatesS(store: Store, rs: Map[String, Record]) = Free.liftFC(Updates(store, rs.map(kv => Store.StringId(kv._1) -> kv._2)))

  def delete(store: Store, id: String) = Free.liftFC(Delete(store, Store.StringId(id)))

  def deletes(store: Store, ids: Seq[Store.Id]) = Free.liftFC(Deletes(store, ids))

  def runFM[F[_], O](
    f: StoreOperationFM[O]
  )(interpreter: NaturalTransformation[StoreOperation, F]
  )(implicit F: Monad[F]): F[O] = {
    Free.runFC(f)(interpreter)
  }

  def runId[O](
    f: StoreOperationFM[O]
  )(interpreter: NaturalTransformation[StoreOperation, Id]): O = {
    runFM(f)(interpreter)
  }

  def runTask[O](
    f: StoreOperationFM[O]
  )(interpreter: NaturalTransformation[StoreOperation, Task]): Task[O] = {
    runFM(f)(interpreter)
  }
}
