package org.goldenport.record.unitofwork

import scalaz._, Scalaz._
import org.goldenport.record.v2._
import UnitOfWork._

/*
 * @since   Nov. 16, 2015
 * @version Nov. 16, 2015
 * @author  ASAMI, Tomoharu
 */
trait UnitOfWorkLogic {
  def invokeService(req: ServiceRequest): ServiceResponse
}

object UnitOfWorkLogic {
  val printer = new UnitOfWorkLogic {
    def invokeService(req: ServiceRequest): ServiceResponse = {
      println(s"invokeService: ")
      ServiceResponse()
    }
  }
}
