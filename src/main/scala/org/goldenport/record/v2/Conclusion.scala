package org.goldenport.record.v2

import scalaz._, Scalaz._
import java.util.Locale
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.collection.NonEmptyVector
import org.goldenport.context.{Conclusion => LibConclusion, ErrorMessages, WarningMessages, Messages, Faults}

/*
 * See 
 * @since   Apr. 17, 2020
 *  version May. 26, 2020
 *  version Jun.  8, 2020
 *  version May. 20, 2021
 *  version Oct. 12, 2021
 *  version Jan. 25, 2022
 * @version Feb. 18, 2022
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

  def message: String = message(Locale.ENGLISH)
  def messageError: String = messageError(Locale.ENGLISH)
  def messageWarning: String = messageWarning(Locale.ENGLISH)
  def getMessage: Option[String] = getMessage(Locale.ENGLISH)
  def getMessageError: Option[String] = getMessageError(Locale.ENGLISH)
  def getMessageWarning: Option[String] = getMessageWarning(Locale.ENGLISH)
  def message(locale: Locale): String = getMessage(locale) getOrElse ""
  def messageError(locale: Locale): String = getMessageError(locale) getOrElse ""
  def messageWarning(locale: Locale): String = getMessageWarning(locale) getOrElse ""
  def getMessage(l: Locale): Option[String] = {
    val a = _messages(l, errors) ++ _messages(l, warnings) ++ _errors(l, validations) ++ _warnings(l, validations)
    a match {
      case Nil => None
      case xs => Some(xs.mkString(";"))
    }
  }
  def getMessageError(l: Locale): Option[String] = {
    val a = _messages(l, errors) ++ _errors(l, validations) 
    a match {
      case Nil => None
      case xs => Some(xs.mkString(";"))
    }
  }
  def getMessageWarning(l: Locale): Option[String] = {
    val a = _messages(l, warnings) ++ _warnings(l, validations)
    a match {
      case Nil => None
      case xs => Some(xs.mkString(";"))
    }
  }

  private def _messages(l: Locale, ps: Option[NonEmptyVector[I18NString]]): List[String] =
    ps.map(x => _messages(l, x.list)).getOrElse(Nil)

  private def _messages(l: Locale, ps: List[I18NString]): List[String] = ps.map(_.as(l))

  private def _errors(l: Locale, ps: List[Conclusion.ValidationSlot]): List[String] =
    _messages(l, ps.filter(_.isError).map(_.i18nMessage))

  private def _warnings(l: Locale, ps: List[Conclusion.ValidationSlot]): List[String] =
    _messages(l, ps.filter(_.isWarning).map(_.i18nMessage))

  def +(rhs: Conclusion): Conclusion = Conclusion(
    code,
    detail,
    errors |+| rhs.errors,
    warnings |+| rhs.warnings,
    exception,
    _unify(validations, rhs.validations)
  )

  private def _unify(lhs: List[Conclusion.ValidationSlot], rhs: List[Conclusion.ValidationSlot]) = {
    lhs ++ rhs // TODO merge
  }

  def forConfig: Conclusion = if (code == 200) this else copy(code = 500)

  def toContextConclusion: LibConclusion = {
    import org.goldenport.context.StatusCode
    import org.goldenport.context.{ErrorMessage, WarningMessage}

    def _error_(p: I18NString) = ErrorMessage.I18NStringErrorMessage(p)
    def _warning_(p: I18NString) = WarningMessage.I18NStringWarningMessage(p)

    def es = errors.map(_.list.map(_error_)).getOrElse(Nil)
    def ws = warnings.map(_.list.map(_warning_)).getOrElse(Nil)

    def _errors_ = ErrorMessages(es)
    def _warnings_ = WarningMessages(ws)

    LibConclusion(
      StatusCode(code, None, detail),
      None,
      _errors_,
      _warnings_,
      exception
    )
  }
}

object Conclusion {
  implicit object ConclusionMonoid extends Monoid[Conclusion] {
    def append(lhs: Conclusion, rhs: => Conclusion) = lhs + rhs
    def zero = Conclusion.success
  }

  case class ValidationSlot(
    key: Symbol,
    result: ValidationResult
  ) {
    def i18nMessage = result.i18nMessage
    def isWarning = result.isWarning
    def isError = result.isError
  }

  val success = Conclusion(200, None, None, None, None, Nil)

  def badRequest(p: String): Conclusion = badRequest(I18NString(p))
  def badRequest(p: I18NString): Conclusion = error(400, p)
  def unauthorized(p: String): Conclusion = unauthorized(I18NString(p))
  def unauthorized(en: String, ja: String): Conclusion = unauthorized(I18NString(en, ja))
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
    code,
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

  def missings(ps: Seq[String]): Conclusion = Conclusion(
    400,
    None,
    None,
    None,
    None,
    ps.map(_missing).toList
  )

  private def _missing(p: String) = {
    ValidationSlot(Symbol(p), MissingFieldFailure(p))
  }

  def from(p: LibConclusion): Conclusion =
    Conclusion(
      p.code.main,
      p.code.detail.map(_.code),
      errors = _messages(p.errors, p.faults),
      warnings = _messages(p.warnings),
      p.exception,
      _validations(p.faults)
    )

  private def _messages(em: ErrorMessages, f: Faults): Option[NonEmptyVector[I18NString]] = {
    _messages(em) |+| _messages(f)
  }

  private def _messages(p: Messages): Option[NonEmptyVector[I18NString]] = p.toI18NStringONev

  private def _messages(p: Faults): Option[NonEmptyVector[I18NString]] = p.toI18NStringONev

  private def _validations(p: Faults): List[Conclusion.ValidationSlot] = Nil
}
