package org.goldenport.record.unitofwork.interpreter

import org.goldenport.exception.RAISE
import org.goldenport.record.unitofwork._, UnitOfWork._

/*
 * @since   Sep. 15, 2018
 * @version Sep. 12, 2025
 * @author  ASAMI, Tomoharu
 */
class StandardUnitOfWorkLogic(
  val executionContext: UnitOfWorkLogic.ExecutionContext
) extends UnitOfWorkLogic {
  def commit(): CommitResult = CommitSuccess("")
  def abort(message: String): Unit = {}
  def abort(e: Throwable): Unit = {}

  def invokeService(req: ServiceRequest): ServiceResponse = RAISE.unsupportedOperationFault
}

object StandardUnitOfWorkLogic {
}
