package org.goldenport.record.unitofwork.interpreter

import scala.language.higherKinds
import scalaz.{Store => _, Value => _, _}, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._

/*
 * @since   Nov. 15, 2015
 *  version Dec.  2, 2015
 * @version Apr. 28, 2016
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkInterpreter[+F[_]] extends NaturalTransformation[UnitOfWork, F] {
  private var _exception: Option[Throwable] = None

  def storeOperation: StoreOperationInterpreter[F]

  def abort(message: String): Unit = {
    interpreter_Abort(message)
    storeOperation.abort(message)
  }
  def abort(e: Throwable): Unit = {
    interpreter_Abort(e)
    storeOperation.abort(e)
  }

  protected def interpreter_Abort(message: String): Unit
  protected def interpreter_Abort(e: Throwable): Unit

  def apply[T](op: UnitOfWork[T]): F[T] =
    get_exception match {
      case Some(s) => apply_exception(s)
      case None => apply_operation(op)
    }

  protected def get_exception: Option[Throwable] = _exception

  protected def apply_exception[T](e: Throwable): F[T] = raise(e)

  protected def apply_operation[T](op: UnitOfWork[T]): F[T] =
    op match {
      case Raise(e) => raise_exception(e)
      case InvokeService(req) => invokeService(req)
      case Value(v) => value(v)
      case m: StoreOperation[_] => storeOperation(m)
      case m: ExtensionUnitOfWork[_] => extension(m)
    }

  protected def raise_exception[T](e: Throwable): F[T] = {
    _exception = Some(e)
    raise(e)
  }

  def raise[T](e: Throwable): F[T]
  def invokeService[T](req: UnitOfWork.ServiceRequest): F[T]
  def value[T](v: T): F[T]
  def extension[T](p: ExtensionUnitOfWork[_]): F[T]
}

class IdUnitOfWorkInterpreter(
  val storeOperation: IdStoreOperationInterpreter,
  val logic: UnitOfWorkLogic
) extends UnitOfWorkInterpreter[Id] {
  def invokeService[T](req: UnitOfWork.ServiceRequest): T = {
    logic.invokeService(req).asInstanceOf[T]
  }

  def raise[T](e: Throwable): T = ???
  def value[T](v: T) = v
  def extension[T](p: ExtensionUnitOfWork[_]): T = ???

  protected def interpreter_Abort(message: String): Unit = ???
  protected def interpreter_Abort(e: Throwable): Unit = ???
}

trait UnitOfWorkInterpreterBase[F[_]] extends UnitOfWorkInterpreter[F] {
  def typeclass: Applicative[F]
  def logic: UnitOfWorkLogic

  protected def interpreter_Abort(message: String) = logic.abort(message)
  protected def interpreter_Abort(e: Throwable): Unit = logic.abort(e)

  def invokeService[T](req: UnitOfWork.ServiceRequest): F[T] = {
    typeclass.point(logic.invokeService(req).asInstanceOf[T])
  }

  def value[T](v: T): F[T] = {
    typeclass.point(v)
  }

  def extension[T](p: ExtensionUnitOfWork[_]): F[T] = {
    typeclass.point(logic.extension(p).asInstanceOf[T])
  }
}

class TaskUnitOfWorkInterpreter(
  val storeOperation: TaskStoreOperationInterpreter,
  val logic: UnitOfWorkLogic
) extends UnitOfWorkInterpreterBase[Task] {
  def typeclass: Applicative[Task] = Task.taskInstance
  def raise[T](e: Throwable): Task[T] = Task.fail(e)
}

object UnitOfWorkInterpreter {
  def log = new IdUnitOfWorkInterpreter(StoreOperationInterpreter.log, UnitOfWorkLogic.printer)
  def logTask = new TaskUnitOfWorkInterpreter(StoreOperationInterpreter.logTask, UnitOfWorkLogic.printer)
}
