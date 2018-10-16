package org.goldenport.record.http

import org.goldenport.values.PathName
import org.goldenport.record.v3.Record

/*
 * @since   Aug. 19, 2018
 * @version Aug. 19, 2018
 * @author  ASAMI, Tomoharu
 */
trait Driver {
  def invoke(req: Request): Response
}
