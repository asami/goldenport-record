package org.goldenport.record.unitofwork.interpreter

import scala.language.higherKinds
import scalaz.{Store => _, Value => _, _}, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._

/*
 * @since   Nov. 15, 2015
 * @version Dec.  2, 2015
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkInterpreter[+F[_]] extends NaturalTransformation[UnitOfWork, F] {
  def storeOperation: StoreOperationInterpreter[F]

  def apply[T](op: UnitOfWork[T]): F[T] = {
    op match {
      case InvokeService(req) => invokeService(req)
      case Value(v) => value(v)
      case m: StoreOperation[_] => storeOperation(m)
      case _: ExtensionUnitOfWork[_] => sys.error(s"Undefined ExtensionUnitOfWork: $op")
    }
  }

  def invokeService[T](req: UnitOfWork.ServiceRequest): F[T]
  def value[T](v: T): F[T]
}

class IdUnitOfWorkInterpreter(
  val storeOperation: IdStoreOperationInterpreter,
  val logic: UnitOfWorkLogic
) extends UnitOfWorkInterpreter[Id] {
  def invokeService[T](req: UnitOfWork.ServiceRequest): T = {
    logic.invokeService(req).asInstanceOf[T]
  }

  def value[T](v: T) = v
}

trait UnitOfWorkInterpreterBase[F[_]] extends UnitOfWorkInterpreter[F] {
  def typeclass: Applicative[F]
  def logic: UnitOfWorkLogic

  def invokeService[T](req: UnitOfWork.ServiceRequest): F[T] = {
    typeclass.point(logic.invokeService(req).asInstanceOf[T])
  }

  def value[T](v: T): F[T] = {
    typeclass.point(v)
  }
}

class TaskUnitOfWorkInterpreter(
  val storeOperation: TaskStoreOperationInterpreter,
  val logic: UnitOfWorkLogic
) extends UnitOfWorkInterpreterBase[Task] {
  def typeclass: Applicative[Task] = Task.taskInstance
}

object UnitOfWorkInterpreter {
  def log = new IdUnitOfWorkInterpreter(StoreOperationInterpreter.log, UnitOfWorkLogic.printer)
  def logTask = new TaskUnitOfWorkInterpreter(StoreOperationInterpreter.logTask, UnitOfWorkLogic.printer)
}
