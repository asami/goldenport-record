package org.goldenport.record.v2.unitofwork

import scala.language.higherKinds
import scala.util.control.NonFatal
import org.goldenport.record.unitofwork._
import org.goldenport.record.v2.Record

/*
 * @since   Apr.  2, 2018
 *  version May. 31, 2018
 *  version Sep. 12, 2018
 *  version Sep. 13, 2019
 *  version Mar. 19, 2021
 *  version Dec. 31, 2021
 * @version Sep. 14, 2023
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkHelper {
  import UnitOfWork._
  import Store._

  // experimental
  protected def uow_operation[T[_] <: UnitOfWork[_], A](x: T[A]) = UnitOfWork.lift(x)

  protected def uow_raise[T](e: Throwable): UnitOfWorkFM[T] = UnitOfWork.raise(e)

  protected def uow_lift[T](p: T): UnitOfWorkFM[T] = UnitOfWork.lift(p)

  protected lazy val uow_unit: UnitOfWorkFM[Unit] = UnitOfWork.lift(Unit)

  protected def uow_take[T](label: String, p: Option[T]): UnitOfWorkFM[T] =
    uow_get_or_raise(new NoSuchElementException(label))(p)

  protected def uow_get_or_raise[T](e: => Throwable)(p: Option[T]): UnitOfWorkFM[T] =
    p.map(UnitOfWork.lift(_)).getOrElse(UnitOfWork.raise(e))

  protected def uow_flush[T](condition: T => Option[Throwable])(
    result: T
  ): UnitOfWorkFM[CommitResult] =
    condition(result) match {
      case Some(x) => UnitOfWork.raise(x)
      case None => UnitOfWork.flush()
    }

  protected def uow_execute[T](p: => T): UnitOfWorkFM[T] = try {
    uow_lift(p)
  } catch {
    case NonFatal(e) => uow_raise(e)
  }

  protected def uow_run[T](p: => UnitOfWorkFM[T]): UnitOfWorkFM[T] = try {
    p
  } catch {
    case NonFatal(e) => uow_raise(e)
  }

  protected def service_invoke[T <: ServiceResponse](req: ServiceRequest): UnitOfWorkFM[T] =
    UnitOfWork.invoke(req)

  protected def commit_flow_condition[T](condition: T => Option[Throwable])(
    p: UnitOfWorkFM[T]
  ): UnitOfWorkFM[UnitOfWorkResult[T]] = {
    for {
      r <- p
      c <- store_commit(condition)(r)
      _ <- uow_flush(condition)(r)
    } yield UnitOfWorkResult(r, c)
  }

  protected def store_get(
    store: Store, id: String
  ): UnitOfWorkFM[GetResult] = UnitOfWork.store.get(
    store, StringId(id))

  protected def store_get_sync(
    store: Store, id: String
  ): UnitOfWorkFM[GetResult] = UnitOfWork.store.getSync(
    store, StringId(id))

  protected def store_get_share(
    store: Store, id: String
  ): UnitOfWorkFM[GetResult] = UnitOfWork.store.getShare(
    store, StringId(id))

  protected def store_get_exclusive(
    store: Store, id: String
  ): UnitOfWorkFM[GetResult] = UnitOfWork.store.getExclusive(
    store, StringId(id))

  protected def store_take(store: Store, id: String): UnitOfWorkFM[Entity] = for {
    x <- UnitOfWork.store.get(store, StringId(id))
    entity <- uow_take(s"id: $id", x.entity)
  } yield entity

  protected def store_take_sync(store: Store, id: String): UnitOfWorkFM[Entity] = for {
    x <- UnitOfWork.store.getSync(store, StringId(id))
    entity <- uow_take(s"id: $id", x.entity)
  } yield entity

  protected def store_take_share(store: Store, id: String): UnitOfWorkFM[Entity] = for {
    x <- UnitOfWork.store.getShare(store, StringId(id))
    entity <- uow_take(s"id: $id", x.entity)
  } yield entity

  protected def store_take_exclusive(store: Store, id: String): UnitOfWorkFM[Entity] = for {
    x <- UnitOfWork.store.getExclusive(store, StringId(id))
    entity <- uow_take(s"id: $id", x.entity)
  } yield entity

  protected def store_get(
    store: Store, ids: Seq[String]
  ): UnitOfWorkFM[GetsResult] = UnitOfWork.store.gets(
    store, ids.map(StringId))

  protected def store_get_sync(
    store: Store, ids: Seq[String]
  ): UnitOfWorkFM[GetsResult] = UnitOfWork.store.getsSync(
    store, ids.map(StringId))

  protected def store_select(
    store: Store, query: String
  ): UnitOfWorkFM[SelectResult] = UnitOfWork.store.select(
    store, Query(query))

  protected def store_select_sync(
    store: Store, query: String
  ): UnitOfWorkFM[SelectResult] = UnitOfWork.store.selectSync(
    store, Query(query))

  protected def store_select(
    store: Store, query: Query
  ): UnitOfWorkFM[SelectResult] = UnitOfWork.store.select(
    store,
    query
  )

  protected def store_select_sync(
    store: Store, query: Query
  ): UnitOfWorkFM[SelectResult] = UnitOfWork.store.selectSync(
    store,
    query
  )

  protected def store_select_share(
    store: Store, query: Query
  ): UnitOfWorkFM[SelectResult] = UnitOfWork.store.selectShare(
    store,
    query
  )

  protected def store_select_exclusive(
    store: Store, query: Query
  ): UnitOfWorkFM[SelectResult] = UnitOfWork.store.selectExclusive(
    store,
    query
  )

  protected def store_insert(
    store: Store, rec: Record
  ): UnitOfWorkFM[InsertResult] = UnitOfWork.store.insert(store, rec)

  protected def store_update(
    store: Store, id: String, rec: Record
  ): UnitOfWorkFM[UpdateResult] = UnitOfWork.store.update(store, id, rec)

  protected def store_update(
    store: Store, id: Store.Id, rec: Record
  ): UnitOfWorkFM[UpdateResult] = UnitOfWork.store.update(store, id, rec)

  protected def store_updates(
    store: Store, records: Map[Store.Id, Record]
  ): UnitOfWorkFM[IndexedSeq[UpdateResult]] = UnitOfWork.store.updates(store, records)

  protected def store_delete(
    store: Store, id: String
  ): UnitOfWorkFM[DeleteResult] = UnitOfWork.store.delete(store, Store.Id(id))

  protected def store_delete(
    store: Store, id: Store.Id
  ): UnitOfWorkFM[DeleteResult] = UnitOfWork.store.delete(store, id)

  protected def store_deletes(
    store: Store, ids: Seq[Store.Id]
  ): UnitOfWorkFM[DeletesResult] = UnitOfWork.store.deletes(store, ids)

  protected def store_commit[T](condition: T => Option[Throwable])(
    result: T
  ): UnitOfWorkFM[CommitResult] =
    condition(result) match {
      case Some(x) => UnitOfWork.lift(CommitFailure(x.toString))
      case None => UnitOfWork.store.commit()
    }
}
object UnitOfWorkHelper {
  object Implicits {
    import scalaz._, Scalaz._
    import UnitOfWork._
    implicit val uowApp = new Applicative[UnitOfWorkFM] {
      def point[A](a: => A): UnitOfWorkFM[A] = UnitOfWork.lift(a)
      def ap[A, B](fa: => UnitOfWorkFM[A])(f: => UnitOfWorkFM[A => B]): UnitOfWorkFM[B] = fa.flatMap(x => f.map(f => f(x)))
    }
  }
}
