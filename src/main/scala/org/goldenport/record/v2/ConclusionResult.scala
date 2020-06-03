package org.goldenport.record.v2

import org.goldenport.i18n.I18NString

/*
 * @since   Jun.  3, 2020
 * @version Jun.  3, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait ConclusionResult[T] {
  def conclusion: Conclusion
}
case class SuccessConclusionResult[T](
  result: T,
  conclusion: Conclusion = Conclusion.success
) extends ConclusionResult[T] {
}

case class ErrorConclusionResult[T](
  conclusion: Conclusion
) extends ConclusionResult[T] {
}

object ConclusionResult {
  def badRequest[T](p: String): ConclusionResult[T] = badRequest(I18NString(p))
  def badRequest[T](p: I18NString): ConclusionResult[T] = error(400, p)
  def unauthorized[T](p: String): ConclusionResult[T] = unauthorized(I18NString(p))
  def unauthorized[T](en: String, ja: String): ConclusionResult[T] = unauthorized(I18NString(en, ja))
  def unauthorized[T](p: I18NString): ConclusionResult[T] = error(401, p)
  def paymentRequired[T](p: String): ConclusionResult[T] = paymentRequired(I18NString(p))
  def paymentRequired[T](p: I18NString): ConclusionResult[T] = error(402, p)
  def forbidden[T](p: String): ConclusionResult[T] = forbidden(I18NString(p))
  def forbidden[T](p: I18NString): ConclusionResult[T] = error(403, p)
  def notFound[T](p: String): ConclusionResult[T] = notFound(I18NString(p))
  def notFound[T](p: I18NString): ConclusionResult[T] = error(404, p)
  def methodNotAllowed[T](p: String): ConclusionResult[T] = methodNotAllowed(I18NString(p))
  def methodNotAllowed[T](p: I18NString): ConclusionResult[T] = error(405, p)
  def notAcceptable[T](p: String): ConclusionResult[T] = notAcceptable(I18NString(p))
  def notAcceptable[T](p: I18NString): ConclusionResult[T] = error(406, p)
  def proxyAuthenticationRequired[T](p: String): ConclusionResult[T] = proxyAuthenticationRequired(I18NString(p))
  def proxyAuthenticationRequired[T](p: I18NString): ConclusionResult[T] = error(407, p)
  def requestTimeout[T](p: String): ConclusionResult[T] = requestTimeout(I18NString(p))
  def requestTimeout[T](p: I18NString): ConclusionResult[T] = error(408, p)
  def conflict[T](p: String): ConclusionResult[T] = conflict(I18NString(p))
  def conflict[T](p: I18NString): ConclusionResult[T] = error(409, p)
  def gone[T](p: String): ConclusionResult[T] = gone(I18NString(p))
  def gone[T](p: I18NString): ConclusionResult[T] = error(410, p)
  def internalServerError[T](p: String): ConclusionResult[T] = internalServerError(I18NString(p))
  def internalServerError[T](p: I18NString): ConclusionResult[T] = error(500, p)
  def notImplemented[T](p: String): ConclusionResult[T] = notImplemented(I18NString(p))
  def notImplemented[T](p: I18NString): ConclusionResult[T] = error(501, p)
  def badGateway[T](p: String): ConclusionResult[T] = badGateway(I18NString(p))
  def badGateway[T](p: I18NString): ConclusionResult[T] = error(502, p)
  def serviceUnavailable[T](p: String): ConclusionResult[T] = serviceUnavailable(I18NString(p))
  def serviceUnavailable[T](p: I18NString): ConclusionResult[T] = error(503, p)
  def gatewayTimeout[T](p: String): ConclusionResult[T] = gatewayTimeout(I18NString(p))
  def gatewayTimeout[T](p: I18NString): ConclusionResult[T] = error(504, p)

  def error[T](code: Int, p: String): ConclusionResult[T] = error(code, I18NString(p))
  def error[T](code: Int, p: I18NString): ConclusionResult[T] = ErrorConclusionResult(Conclusion.error(code, p))

  def missing[T](p: String, ps: String*): ConclusionResult[T] = missings(p +: ps)

  def missings[T](ps: Seq[String]): ConclusionResult[T] = ErrorConclusionResult(Conclusion.missings(ps))
}
