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
 *  version May. 31, 2018
 *  version Oct. 16, 2018
 *  version Sep. 13, 2019
 * @version Jun. 26, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait StoreOperation[+A] extends ExtensionUnitOfWork[A] {
}

case class Get(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class GetSync(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class GetShare(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class GetExclusive(store: Store, id: Store.Id) extends StoreOperation[GetResult]

case class Gets(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class GetsSync(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class GetsShare(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class GetsExclusive(store: Store, ids: Seq[Store.Id]) extends StoreOperation[GetsResult]

case class Select(store: Store, query: Query) extends StoreOperation[SelectResult]

case class SelectSync(store: Store, query: Query) extends StoreOperation[SelectResult]

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
object CommitResult {
  val empty = CommitSuccess("")
}
case class CommitSuccess(log: String) extends CommitResult {
}
case class CommitFailure(log: String) extends CommitResult {
}

object StoreOperation {
  type StoreOperationFM[T] = Free[StoreOperation, T]
//  type StoreOperationFMRaw[T] = Free[({type λ[α] = Coyoneda[StoreOperation, α]})#λ, T]

  def get(store: Store, id: Store.Id) = Free.liftF(Get(store, id))

  def getSync(store: Store, id: Store.Id) = Free.liftF(GetSync(store, id))

  def getShare(store: Store, id: Store.Id) = Free.liftF(GetShare(store, id))

  def getExclusive(store: Store, id: Store.Id) = Free.liftF(GetExclusive(store, id))

  def gets(store: Store, ids: Seq[Store.Id]) = Free.liftF(Gets(store, ids))

  def getsSync(store: Store, ids: Seq[Store.Id]) = Free.liftF(GetsSync(store, ids))

  def select(store: Store, query: Query) = Free.liftF(Select(store, query))

  def selectSync(store: Store, query: Query) = Free.liftF(SelectSync(store, query))

  def selectShare(store: Store, query: Query) = Free.liftF(SelectShare(store, query))

  def selectExclusive(store: Store, query: Query) = Free.liftF(SelectExclusive(store, query))

  def insert(
    store: Store,
    rec: Record
  ): StoreOperationFM[InsertResult] = Free.liftF(Insert(store, rec))

  def inserts(
    store: Store,
    rs: RecordSet
  ): StoreOperationFM[IndexedSeq[InsertResult]] = Free.liftF(Inserts(store, rs))

  def update(store: Store, id: Store.Id, rec: Record) = Free.liftF(Update(store, id, rec))

  def update(store: Store, id: String, rec: Record) = Free.liftF(Update(store, Store.StringId(id), rec))

  def updates(store: Store, rs: Map[Store.Id, Record]) = Free.liftF(Updates(store, rs))

  def updatesS(store: Store, rs: Map[String, Record]) = Free.liftF(Updates(store, rs.map(kv => Store.StringId(kv._1) -> kv._2)))

  def delete(store: Store, id: String) = Free.liftF(Delete(store, Store.StringId(id)))

  def delete(store: Store, id: Store.Id) = Free.liftF(Delete(store, id))

  def deletes(store: Store, ids: Seq[Store.Id]) = Free.liftF(Deletes(store, ids))

  def commit() = Free.liftF(Commit())

  // compiler error
//  def abort(p: String) = Free.liftF(CommitFailure(p))

  def runFM[F[_], O](
    f: StoreOperationFM[O]
  )(interpreter: NaturalTransformation[StoreOperation, F]
  )(implicit F: Monad[F]): F[O] = {
    f.foldMap(interpreter)
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
