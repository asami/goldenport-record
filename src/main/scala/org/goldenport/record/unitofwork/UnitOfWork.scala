package org.goldenport.record.unitofwork

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scala.util.Try
import scala.util.control.NonFatal
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 *  version Dec.  4, 2015
 *  version Apr. 27, 2016
 *  version Nov. 30, 2017
 *  version Dec.  1, 2017
 *  version Jan. 31, 2018
 *  version Mar. 28, 2018
 *  version Apr.  3, 2018
 *  version May. 31, 2018
 *  version Sep. 12, 2018
 *  version Oct. 16, 2018
 *  version Sep. 13, 2019
 * @version Jun. 26, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait UnitOfWork[+A] {
}

trait ExtensionUnitOfWork[+A] extends UnitOfWork[A]

case class Value[T](v: T) extends UnitOfWork[T]

case class Flush() extends UnitOfWork[CommitResult]

case class InvokeService(request: UnitOfWork.ServiceRequest) extends UnitOfWork[UnitOfWork.ServiceResponse] {
}

case class Raise(e: Throwable) extends UnitOfWork[Throwable]

case class UnitOfWorkResult[T](
  result: T,
  commit: CommitResult
) {
  def map[B](f: T => B): UnitOfWorkResult[B] = copy(result = f(result))
}
object UnitOfWorkResult {
  def apply[T](p: T): UnitOfWorkResult[T] = UnitOfWorkResult(p, CommitResult.empty)
}

object UnitOfWork {
  type UnitOfWorkFM[T] = Free[UnitOfWork, T]

  def lift[T[_] <: UnitOfWork[_], A](x: T[A]) = Free.liftF(x)
  def lift[T](x: T): UnitOfWorkFM[T] = Free.liftF(Value(x))

  def liftTry[T](p: Try[T]): UnitOfWorkFM[T] = p match {
    case scala.util.Success(s) => lift(s)
    case scala.util.Failure(e) => raise(e)
  }

  def execute[T](body: => T): UnitOfWorkFM[T] = try {
    lift(body)
  } catch {
    case NonFatal(e) => raise(e)
  }

  def raise[T](e: Throwable): UnitOfWorkFM[T] = 
    Free.liftF(Raise(e)).asInstanceOf[UnitOfWorkFM[T]]

  def flush[T](): UnitOfWorkFM[T] =
    Free.liftF(Flush()).asInstanceOf[UnitOfWorkFM[T]]

  trait ServiceRequest
  trait ServiceResponse

  case class BooleanRequest(v: Boolean) extends ServiceRequest
  case class ByteRequest(v: Byte) extends ServiceRequest
  case class ShortRequest(v: Short) extends ServiceRequest
  case class IntRequest(v: Int) extends ServiceRequest
  case class FloatRequest(v: Float) extends ServiceRequest
  case class DoubleRequest(v: Double) extends ServiceRequest
  case class BigIntRequest(v: BigInt) extends ServiceRequest
  case class BigDecimalRequest(v: BigDecimal) extends ServiceRequest
  case class StringRequest(v: String) extends ServiceRequest
  case object UnitRequest extends ServiceRequest
  case class BooleanResponse(v: Boolean) extends ServiceResponse
  case class ByteResponse(v: Byte) extends ServiceResponse
  case class ShortResponse(v: Short) extends ServiceResponse
  case class IntResponse(v: Int) extends ServiceResponse
  case class FloatResponse(v: Float) extends ServiceResponse
  case class DoubleResponse(v: Double) extends ServiceResponse
  case class BigIntResponse(v: BigInt) extends ServiceResponse
  case class BigDecimalResponse(v: BigDecimal) extends ServiceResponse
  case class StringResponse(v: String) extends ServiceResponse
  case class UnitResponse() extends ServiceResponse

  def invoke[T <: ServiceResponse](req: ServiceRequest): UnitOfWorkFM[T] =
    Free.liftF(InvokeService(req)).asInstanceOf[UnitOfWorkFM[T]]

  def runFM[F[_], O](
    f: UnitOfWorkFM[O]
  )(interpreter: NaturalTransformation[UnitOfWork, F]
  )(implicit F: Monad[F]): F[O] = {
    f.foldMap(interpreter)
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

    def getSync(store: Store, id: Store.Id) = StoreOperation.getSync(store, id).asInstanceOf[UnitOfWorkFM[GetResult]]

    def getShare(store: Store, id: Store.Id) = StoreOperation.getShare(store, id).asInstanceOf[UnitOfWorkFM[GetResult]]

    def getExclusive(store: Store, id: Store.Id) = StoreOperation.getExclusive(store, id).asInstanceOf[UnitOfWorkFM[GetResult]]

    def gets(store: Store, ids: Seq[Store.Id]) = StoreOperation.gets(store, ids).asInstanceOf[UnitOfWorkFM[GetsResult]]

    def getsSync(store: Store, ids: Seq[Store.Id]) = StoreOperation.getsSync(store, ids).asInstanceOf[UnitOfWorkFM[GetsResult]]

    def select(store: Store, query: Query) = StoreOperation.select(store, query).asInstanceOf[UnitOfWorkFM[SelectResult]]

    def selectSync(store: Store, query: Query) = StoreOperation.selectSync(store, query).asInstanceOf[UnitOfWorkFM[SelectResult]]

    def selectShare(store: Store, query: Query) = StoreOperation.selectShare(store, query).asInstanceOf[UnitOfWorkFM[SelectResult]]

    def selectExclusive(store: Store, query: Query) = StoreOperation.selectExclusive(store, query).asInstanceOf[UnitOfWorkFM[SelectResult]]

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
    ) = StoreOperation.updates(store, rs).asInstanceOf[UnitOfWorkFM[IndexedSeq[UpdateResult]]]

    def delete(
      store: Store, id: String
    ) = StoreOperation.delete(store, Store.Id(id)).asInstanceOf[UnitOfWorkFM[DeleteResult]]

    def delete(
      store: Store, id: Store.Id
    ) = StoreOperation.delete(store, id).asInstanceOf[UnitOfWorkFM[DeleteResult]]

    def deletes(
      store: Store,
      ids: Seq[Store.Id]
    ) = StoreOperation.deletes(store, ids).asInstanceOf[UnitOfWorkFM[DeletesResult]]

    def commit() = StoreOperation.commit().asInstanceOf[UnitOfWorkFM[CommitResult]]

    // compiler error
//    def abort(p: String) = StoreOperation.abort(p).asInstanceOf[UnitOfWorkFM[CommitResult]]
  }
}
