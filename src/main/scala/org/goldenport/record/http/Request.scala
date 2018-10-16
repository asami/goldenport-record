package org.goldenport.record.http

import java.net.URL
import org.goldenport.value._
import org.goldenport.values.PathName
import org.goldenport.record.v3.Record

/*
 * unify arcadia
 * 
 * @since   Dec. 19, 2017
 *  version Aug. 19, 2018
 * @version Sep. 17, 2018
 * @author  ASAMI, Tomoharu
 */
case class Request(
  url: URL,
  method: Request.Method,
  query: Record,
  form: Record
) {
  def isGet = method == Request.GET
  def isMutation = !isGet
  // lazy val pathName = PathName(pathname)
  def show = s"Request(${url})"
}

object Request {
  sealed trait Method extends NamedValueInstance {
  }
  case object GET extends Method {
    val name = "GET"
  }
  case object POST extends Method {
    val name = "POST"
  }
  case object PUT extends Method {
    val name = "PUT"
  }
  case object DELETE extends Method {
    val name = "DELETE"
  }

  def apply(url: URL): Request = Request(url, GET, Record.empty, Record.empty)
}
