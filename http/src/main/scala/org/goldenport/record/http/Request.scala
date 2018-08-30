package org.goldenport.record.http

import org.goldenport.values.PathName
import org.goldenport.record.v3.Record

/*
 * unify arcadia
 * 
 * @since   Dec. 19, 2017
 * @version Aug. 19, 2018
 * @author  ASAMI, Tomoharu
 */
case class Request(
  pathname: String,
  operationName: String,
  method: String, // TODO
  query: Record,
  form: Record
) {
  def isGet = method.toUpperCase == "GET"
  def isMutation = !isGet
  lazy val pathName = PathName(pathname)
}
