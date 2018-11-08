package org.goldenport.record.http

import java.net.{URL, URLDecoder}
import org.goldenport.Strings
import org.goldenport.value._
import org.goldenport.values.PathName
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.util.StringUtils

/*
 * unify arcadia
 * 
 * @since   Dec. 19, 2017
 *  version Aug. 19, 2018
 *  version Sep. 17, 2018
 *  version Oct. 30, 2018
 * @version Nov.  7, 2018
 * @author  ASAMI, Tomoharu
 */
case class Request(
  url: URL,
  method: Request.Method,
  query: IRecord,
  form: IRecord,
  header: IRecord
) {
  def isGet = method == Request.GET
  def isMutation = !isGet
  def urlStringWithQuery = Request.buildUrlStringWithQuery(url, query)
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

  def apply(url: URL): Request = Request(url, GET, Record.empty, Record.empty,Record.empty)

  def create(baseurl: String, path: String): Request = {
    val s = StringUtils.concatPath(baseurl, path)
    apply(new URL(s))
  }

  def parseQuery(p: String): Record = {
    val a = if (p.startsWith("?")) p.substring(1) else p
    val bs = Strings.totokens(a, "&")
    val xs = bs.map(x => StringUtils.tokeyvalue(x, "=")).map {
      case (k, v) => k -> URLDecoder.decode(v, "UTF-8")
    }
    Record.create(xs)
  }

  def buildUrlStringWithQuery(url: URL, p: IRecord): String =
    buildUrlStringWithQuery(url.toExternalForm, p)

  def buildUrlStringWithQuery(s: String, p: IRecord): String =
    StringUtils.addUrlParams(s, p.asNameStringVector)
}
