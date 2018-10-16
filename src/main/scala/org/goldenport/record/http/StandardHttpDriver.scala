package org.goldenport.record.http

import scalaj.http._
import org.goldenport.exception.RAISE

/*
 * @since   Sep. 17, 2018
 * @version Sep. 18, 2018
 * @author  ASAMI, Tomoharu
 */
case class StandardHttpDriver(
  config: StandardHttpDriver.Config = StandardHttpDriver.Config.default
) extends Driver {
  def invoke(req: Request): Response = req.method match {
    case Request.GET =>
      val http = Http(req.url.toString).
        option(HttpOptions.followRedirects(config.followRedirects)).
        params(req.query.nameStrings)
      val res = http.exec(Response.parser)
      res.body
    case Request.POST => RAISE.notImplementedYetDefect
    case Request.PUT => RAISE.notImplementedYetDefect
    case Request.DELETE => RAISE.notImplementedYetDefect
  }
}

object StandardHttpDriver {
  case class Config(
    followRedirects: Boolean = true
  )
  object Config {
    val default = Config()
  }
}
