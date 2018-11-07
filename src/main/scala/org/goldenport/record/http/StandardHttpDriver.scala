package org.goldenport.record.http

import scalaj.http._
import org.goldenport.exception.RAISE

/*
 * @since   Sep. 17, 2018
 * @version Oct. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class StandardHttpDriver(
  config: StandardHttpDriver.Config = StandardHttpDriver.Config.default
) extends Driver {
  def invoke(req: Request): Response = {
    val http = req.method match {
      case Request.GET => Http(req.url.toString).
          option(HttpOptions.followRedirects(config.followRedirects)).
          headers(req.header.nameStrings).
          params(req.query.nameStrings)
      case Request.POST => _mutation_request(req)
      case Request.PUT => _mutation_request(req)
      case Request.DELETE => _mutation_request(req)
    }
    val res = http.exec(Response.parser)
    res.body
  }

  private def _mutation_request(req: Request) =
    Http(req.urlStringWithQuery).
      method(req.method.name).
      option(HttpOptions.followRedirects(config.followRedirects)).
      headers(req.header.nameStrings).
      params(req.form.nameStrings)
}

object StandardHttpDriver {
  case class Config(
    followRedirects: Boolean = true
  )
  object Config {
    val default = Config()
  }
}
