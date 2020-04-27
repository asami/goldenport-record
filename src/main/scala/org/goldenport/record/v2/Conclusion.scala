package org.goldenport.record.v2

import scalaz._, Scalaz._
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Apr. 17, 2020
 * @version Apr. 18, 2020
 * @author  ASAMI, Tomoharu
 */
case class Conclusion(
  code: Int,
  detail: Option[Int],
  errors: Option[NonEmptyVector[I18NString]],
  warnings: Option[NonEmptyVector[I18NString]],
  exception: Option[Throwable],
  validations: List[Conclusion.ValidationSlot]
) {
  def warningForProperty: Map[Symbol, Warning] = validations.flatMap(x =>
    x.result match {
      case m: Warning => Some(x.key -> m)
      case _ => None
    }
  ).toMap

  def errorForProperty: Map[Symbol, Invalid] = validations.flatMap(x =>
    x.result match {
      case m: Invalid => Some(x.key -> m)
      case _ => None
    }
  ).toMap
}

object Conclusion {
  case class ValidationSlot(
    key: Symbol,
    result: ValidationResult
  )

  val success = Conclusion(200, None, None, None, None, Nil)

  def badRequest(p: String): Conclusion = badRequest(I18NString(p))
  def badRequest(p: I18NString): Conclusion = error(400, p)
  def unauthorized(p: String): Conclusion = unauthorized(I18NString(p))
  def unauthorized(p: I18NString): Conclusion = error(401, p)
  def paymentRequired(p: String): Conclusion = paymentRequired(I18NString(p))
  def paymentRequired(p: I18NString): Conclusion = error(402, p)
  def forbidden(p: String): Conclusion = forbidden(I18NString(p))
  def forbidden(p: I18NString): Conclusion = error(403, p)
  def notFound(p: String): Conclusion = notFound(I18NString(p))
  def notFound(p: I18NString): Conclusion = error(404, p)
  def methodNotAllowed(p: String): Conclusion = methodNotAllowed(I18NString(p))
  def methodNotAllowed(p: I18NString): Conclusion = error(405, p)
  def notAcceptable(p: String): Conclusion = notAcceptable(I18NString(p))
  def notAcceptable(p: I18NString): Conclusion = error(406, p)
  def proxyAuthenticationRequired(p: String): Conclusion = proxyAuthenticationRequired(I18NString(p))
  def proxyAuthenticationRequired(p: I18NString): Conclusion = error(407, p)
  def requestTimeout(p: String): Conclusion = requestTimeout(I18NString(p))
  def requestTimeout(p: I18NString): Conclusion = error(408, p)
  def conflict(p: String): Conclusion = conflict(I18NString(p))
  def conflict(p: I18NString): Conclusion = error(409, p)
  def gone(p: String): Conclusion = gone(I18NString(p))
  def gone(p: I18NString): Conclusion = error(410, p)
  def internalServerError(p: String): Conclusion = internalServerError(I18NString(p))
  def internalServerError(p: I18NString): Conclusion = error(500, p)
  def notImplemented(p: String): Conclusion = notImplemented(I18NString(p))
  def notImplemented(p: I18NString): Conclusion = error(501, p)
  def badGateway(p: String): Conclusion = badGateway(I18NString(p))
  def badGateway(p: I18NString): Conclusion = error(502, p)
  def serviceUnavailable(p: String): Conclusion = serviceUnavailable(I18NString(p))
  def serviceUnavailable(p: I18NString): Conclusion = error(503, p)
  def gatewayTimeout(p: String): Conclusion = gatewayTimeout(I18NString(p))
  def gatewayTimeout(p: I18NString): Conclusion = error(504, p)

  def error(code: Int, p: String): Conclusion = error(code, I18NString(p))
  def error(code: Int, p: I18NString): Conclusion = Conclusion(
    401,
    None,
    Some(NonEmptyVector(p)),
    None,
    None,
    Nil
  )

  def missing(p: String, ps: String*): Conclusion = Conclusion(
    400,
    None,
    None,
    None,
    None,
    (p +: ps).map(_missing).toList
  )

  private def _missing(p: String) = {
    ValidationSlot(Symbol(p), MissingFieldFailure(p))
  }
}
