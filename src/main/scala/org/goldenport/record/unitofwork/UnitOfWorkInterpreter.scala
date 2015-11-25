package org.goldenport.record.unitofwork

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.record.v2._

/*
 * @since   Nov. 15, 2015
 * @version Nov. 16, 2015
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkInterpreter[+F[_]] extends NaturalTransformation[UnitOfWork, F] {
  def storeOperation: StoreOperationInterpreter[F]

  def apply[T](op: UnitOfWork[T]): F[T] = {
    op match {
      case InvokeService(req) => invokeService(req)
      case m: StoreOperation[_] => storeOperation(m)
    }
  }

  def invokeService[T](req: UnitOfWork.ServiceRequest): F[T]
}

class IdUnitOfWorkInterpreter(
  val storeOperation: IdStoreOperationInterpreter,
  val logic: UnitOfWorkLogic
) extends UnitOfWorkInterpreter[Id] {
  def invokeService[T](req: UnitOfWork.ServiceRequest): T = {
    logic.invokeService(req).asInstanceOf[T]
  }
}

trait UnitOfWorkInterpreterBase[F[_]] extends UnitOfWorkInterpreter[F] {
  def typeclass: Applicative[F]
  def logic: UnitOfWorkLogic

  def invokeService[T](req: UnitOfWork.ServiceRequest): F[T] = {
    typeclass.point(logic.invokeService(req).asInstanceOf[T])
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
