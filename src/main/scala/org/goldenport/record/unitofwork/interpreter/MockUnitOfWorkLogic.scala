package org.goldenport.record.unitofwork.interpreter

import java.net.URL
import org.goldenport.exception.RAISE
import org.goldenport.record.unitofwork._, UnitOfWork._
import org.goldenport.record.http._

/*
 * @since   Sep. 18, 2018
 * @version Sep. 18, 2018
 * @author  ASAMI, Tomoharu
 */
case class MockUnitOfWorkLogic(
  urlTable: MockUnitOfWorkLogic.UrlTable
) extends UnitOfWorkLogic {
  def commit(): CommitResult = CommitSuccess("")
  def abort(message: String): Unit = {}
  def abort(e: Throwable): Unit = {}

  def invokeService(req: ServiceRequest): ServiceResponse = RAISE.unsupportedOperationFault
  override def httpService(req: Request): Response = urlTable.get(req) getOrElse RAISE.unsupportedOperationFault
}

object MockUnitOfWorkLogic {
  case class UrlTable(map: Map[URL, Response]) {
    def get(req: Request): Option[Response] = map.get(req.url)
  }

  def build(p: (String, String), ps: (String, String)*): MockUnitOfWorkLogic = {
    val xs = (p +: ps).map {
      case (k, v) => (new URL(k), Response.html(v))
    }
    MockUnitOfWorkLogic(UrlTable(xs.toMap))
  }
}
