package org.goldenport.record.http

import scalaj.http._
import org.goldenport.exception.RAISE

/*
 * @since   Sep. 17, 2018
 *  version Oct. 30, 2018
 *  version Nov.  7, 2018
 *  version Dec.  5, 2018
 * @version Feb. 21, 2024
 * @author  ASAMI, Tomoharu
 */
case class StandardHttpDriver(
  config: StandardHttpDriver.Config = StandardHttpDriver.Config.default
) extends Driver {
  def invoke(req: Request): Response = {
    val http = req.method match {
      case Request.GET => _access_request(req)
      case Request.POST => _mutation_request(req)
      case Request.PUT => _mutation_request(req)
      case Request.DELETE => _mutation_request(req)
    }
    val res = http.exec(Response.parser)
    res.body
  }

  private def _access_request(req: Request) =
    Http(req.url.toString).
      option(HttpOptions.followRedirects(config.followRedirects)).
      headers(req.header.asNameStringVector).
      params(req.query.asNameStringVector).
      timeout(connTimeoutMs = config.accessConnTimeoutMs, readTimeoutMs = config.accessReadTimeoutMs)

  private def _mutation_request(req: Request) =
    Http(req.urlStringWithQuery).
      option(HttpOptions.followRedirects(config.followRedirects)).
      headers(req.header.asNameStringVector).
      postForm(req.form.asNameStringVector).
      method(req.method.name).
      timeout(connTimeoutMs = config.mutationConnTimeoutMs, readTimeoutMs = config.mutationReadTimeoutMs)
}

object StandardHttpDriver {
  case class Config(
    followRedirects: Boolean = true,
    connTimeoutMs: Int = 1000,
    readTimeoutMs: Int = 5000
  ) {
    def accessConnTimeoutMs: Int = connTimeoutMs
    def accessReadTimeoutMs: Int = readTimeoutMs
    def mutationConnTimeoutMs: Int = connTimeoutMs
    def mutationReadTimeoutMs: Int = readTimeoutMs
  }
  object Config {
    val default = Config()
  }
}
