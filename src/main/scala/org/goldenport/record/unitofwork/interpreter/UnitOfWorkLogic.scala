package org.goldenport.record.unitofwork.interpreter

import scalaz.{Store => _, _}, Scalaz._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Nov. 16, 2015
 *  version Dec.  2, 2015
 * @version Apr. 28, 2016
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkLogic extends LogicBase {
  def invokeService(req: ServiceRequest): ServiceResponse
  def extension[T](p: ExtensionUnitOfWork[_]): T =
    sys.error(s"Undefined ExtensionUnitOfWork: $p")
}

object UnitOfWorkLogic {
  val printer = new UnitOfWorkLogic {
    def invokeService(req: ServiceRequest): ServiceResponse = {
      println(s"invokeService: ")
      new ServiceResponse {}
    }

    def commit(): CommitResult = ???
    def abort(message: String): Unit = ???
    def abort(e: Throwable): Unit = ???
  }
}
