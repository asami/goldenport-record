package org.goldenport.record.unitofwork

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 *  version Dec.  4, 2015
 *  version Apr. 27, 2016
 *  version Mar. 28, 2018
 *  version Apr.  7, 2018
 * @version May. 31, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait StoreOperation[+A] extends ExtensionUnitOfWork[A] {
}

case class Get(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class GetShare(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class GetExclusive(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class Gets(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class GetsShare(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class GetsExclusive(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class Select(store: Store, query: Query) extends StoreOperation[SelectResult]

case class SelectShare(store: Store, query: Query) extends StoreOperation[SelectResult]

case class SelectExclusive(store: Store, query: Query) extends StoreOperation[SelectResult]

case class Insert(store: Store, rec: Record) extends StoreOperation[InsertResult]

case class Inserts(store: Store, rs: RecordSet) extends StoreOperation[IndexedSeq[InsertResult]]

case class Update(store: Store, id: Store.Id, rec: Record) extends StoreOperation[UpdateResult]

case class Updates(store: Store, rs: Map[Store.Id, Record]) extends StoreOperation[IndexedSeq[UpdateResult]]

case class Delete(store: Store, id: Store.Id) extends StoreOperation[DeleteResult]

case class Deletes(store: Store, ids: Seq[Store.Id]) extends StoreOperation[IndexedSeq[DeleteResult]]

case class Commit() extends StoreOperation[CommitResult]

case class GetResult(entity: Option[Entity])
case class GetsResult(entities: EntitySequence)
case class SelectResult(entities: EntitySequence)
case class InsertResult(
  id: Store.Id,
  items: IndexedSeq[Store.Id] = Vector.empty
)
case class InsertsResult(inserts: IndexedSeq[InsertResult])
case class UpdateResult(
  id: Store.Id,
  items: IndexedSeq[Store.Id] = Vector.empty
)
case class UpdatesResult(updates: IndexedSeq[UpdateResult])
case class DeleteResult(
  id: Store.Id,
  items: IndexedSeq[Store.Id] = Vector.empty
)
case class DeletesResult(ids: IndexedSeq[Store.Id])
sealed trait CommitResult {
  def log: String
}
case class CommitSuccess(log: String) extends CommitResult {
}
case class CommitFailure(log: String) extends CommitResult {
}

object StoreOperation {
  type StoreOperationFM[T] = Free.FreeC[StoreOperation, T]
  type StoreOperationFMRaw[T] = Free[({type λ[α] = Coyoneda[StoreOperation, α]})#λ, T]

  def get(store: Store, id: Store.Id) = Free.liftFC(Get(store, id))

  def getShare(store: Store, id: Store.Id) = Free.liftFC(GetShare(store, id))

  def getExclusive(store: Store, id: Store.Id) = Free.liftFC(GetExclusive(store, id))

  def gets(store: Store, ids: Seq[Store.Id]) = Free.liftFC(Gets(store, ids))

  def select(store: Store, query: Query) = Free.liftFC(Select(store, query))

  def selectShare(store: Store, query: Query) = Free.liftFC(SelectShare(store, query))

  def selectExclusive(store: Store, query: Query) = Free.liftFC(SelectExclusive(store, query))

  def insert(
    store: Store,
    rec: Record
  ): StoreOperationFM[InsertResult] = Free.liftFC(Insert(store, rec))

  def inserts(
    store: Store,
    rs: RecordSet
  ): StoreOperationFM[IndexedSeq[InsertResult]] = Free.liftFC(Inserts(store, rs))

  def update(store: Store, id: Store.Id, rec: Record) = Free.liftFC(Update(store, id, rec))

  def update(store: Store, id: String, rec: Record) = Free.liftFC(Update(store, Store.StringId(id), rec))

  def updates(store: Store, rs: Map[Store.Id, Record]) = Free.liftFC(Updates(store, rs))

  def updatesS(store: Store, rs: Map[String, Record]) = Free.liftFC(Updates(store, rs.map(kv => Store.StringId(kv._1) -> kv._2)))

  def delete(store: Store, id: String) = Free.liftFC(Delete(store, Store.StringId(id)))

  def delete(store: Store, id: Store.Id) = Free.liftFC(Delete(store, id))

  def deletes(store: Store, ids: Seq[Store.Id]) = Free.liftFC(Deletes(store, ids))

  def commit() = Free.liftFC(Commit())

  // compiler error
//  def abort(p: String) = Free.liftFC(CommitFailure(p))

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
