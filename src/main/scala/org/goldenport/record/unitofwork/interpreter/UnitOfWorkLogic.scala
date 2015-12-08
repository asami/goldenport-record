package org.goldenport.record.unitofwork.interpreter

import scalaz.{Store => _, _}, Scalaz._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._

/*
 * @since   Nov. 16, 2015
 * @version Dec.  2, 2015
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkLogic {
  def invokeService(req: ServiceRequest): ServiceResponse
}

object UnitOfWorkLogic {
  val printer = new UnitOfWorkLogic {
    def invokeService(req: ServiceRequest): ServiceResponse = {
      println(s"invokeService: ")
      new ServiceResponse {}
    }
  }
}
