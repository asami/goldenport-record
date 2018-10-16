package org.goldenport.record.http

import org.goldenport.values.PathName
import org.goldenport.record.v3.Record

/*
 * @since   Aug. 19, 2018
 * @version Sep. 15, 2018
 * @author  ASAMI, Tomoharu
 */
trait Service {
  protected def http_driver: Driver

  def invoke(req: Request): Response = {
    http_driver.invoke(req)
  }
}
