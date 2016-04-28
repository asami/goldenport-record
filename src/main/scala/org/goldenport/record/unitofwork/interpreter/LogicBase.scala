package org.goldenport.record.unitofwork.interpreter

import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Apr. 28, 2016
 * @version Apr. 28, 2016
 * @author  ASAMI, Tomoharu
 */
trait LogicBase {
  def commit(): CommitResult
  def abort(message: String): Unit
  def abort(e: Throwable): Unit
}
