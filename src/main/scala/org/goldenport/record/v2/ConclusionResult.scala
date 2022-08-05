package org.goldenport.record.v2

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.util.Locale
import org.goldenport.i18n.I18NString
import org.goldenport.context.Consequence
import org.goldenport.collection.NonEmptyVector
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure, EmptyParseResult}
import org.goldenport.parser.{ParseMessage}

/*
 * @since   Jun.  3, 2020
 *  version Jun. 30, 2020
 *  version Sep.  6, 2020
 *  version Oct. 13, 2020
 *  version Jan. 29, 2021
 *  version Feb. 20, 2021
 *  version May. 20, 2021
 *  version Oct.  6, 2021
 *  version Jan. 25, 2022
 * @version Aug.  3, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait ConclusionResult[+T] {
  def conclusion: Conclusion
  def toOption: Option[T]
  def add(p: Conclusion): ConclusionResult[T]
  def map[U](f: T => U): ConclusionResult[U]
  // ConclusionResult is not Monad. Just to use 'for' comprehension in Scala syntax suger.
  def flatMap[U](f: T => ConclusionResult[U]): ConclusionResult[U]
  def forConfig: ConclusionResult[T]

  def getMessage: Option[String] = conclusion.getMessage
  def message: String = conclusion.message
  def getMessage(locale: Locale): Option[String] = conclusion.getMessage(locale)
  def message(locale: Locale): String = conclusion.message(locale)
  def toConsequence: Consequence[T]
}
case class SuccessConclusionResult[T](
  result: T,
  conclusion: Conclusion = Conclusion.success
) extends ConclusionResult[T] {
  def code = conclusion.code
  def toOption: Option[T] = Some(result)
  def add(p: Conclusion): ConclusionResult[T] = copy(conclusion = conclusion + p)
  def map[U](f: T => U): ConclusionResult[U] = copy(result = f(result))
  def flatMap[U](f: T => ConclusionResult[U]): ConclusionResult[U] =
    f(result) match {
      case m: SuccessConclusionResult[_] => m.copy(conclusion = conclusion + m.conclusion)
      case m: ErrorConclusionResult[_] => m.copy(conclusion = conclusion + m.conclusion)
    }
  def forConfig: ConclusionResult[T] = copy(conclusion = conclusion.forConfig)

  def toConsequence: Consequence[T] = Consequence.Success(result, conclusion.toContextConclusion)
}

case class ErrorConclusionResult[T](
  conclusion: Conclusion
) extends ConclusionResult[T] {
  def toOption: Option[T] = None
  def add(p: Conclusion): ConclusionResult[T] = copy(conclusion = conclusion + p)
  def map[U](f: T => U): ConclusionResult[U] = this.asInstanceOf[ErrorConclusionResult[U]]
  def flatMap[U](f: T => ConclusionResult[U]): ConclusionResult[U] = this.asInstanceOf[ConclusionResult[U]]

  def forConfig: ConclusionResult[T] = copy(conclusion = conclusion.forConfig)

  def toConsequence: Consequence[T] = Consequence.Error(conclusion.toContextConclusion)
}

object ConclusionResult {
  implicit object ConclusionResultApplicative extends Applicative[ConclusionResult] {
    def ap[A, B](fa: => ConclusionResult[A])(f: => ConclusionResult[A => B]): ConclusionResult[B] = f match {
      case SuccessConclusionResult(sf, cs) => fa.map(sf).add(cs)
      case m: ErrorConclusionResult[_] => fa match {
        case mm: SuccessConclusionResult[_] => m.add(mm.conclusion).asInstanceOf[ErrorConclusionResult[B]]
        case mm: ErrorConclusionResult[_] => mm.add(m.conclusion).asInstanceOf[ErrorConclusionResult[B]]
      }
    }
    def point[A](a: => A): ConclusionResult[A] = SuccessConclusionResult(a)
  }

  def apply[T](p: => T): ConclusionResult[T] = success(p)
  def success[T](p: T): ConclusionResult[T] = SuccessConclusionResult(p)
  //
  def successOrMissingProperty[T](name: String, p: Option[T]): ConclusionResult[T] =
    p.map(success).getOrElse(missing(name))
  // def successOrMissingPropertyOrError[T](name: String, p: Option[Left[String, T]]): ConclusionResult[T] =
  //   p.map(success).getOrElse(missingPropertyOrError(name))

  // Generic error derived from HTTP
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

  //
  def error[T](code: Int, p: String): ConclusionResult[T] = error(code, I18NString(p))
  def error[T](code: Int, p: I18NString): ConclusionResult[T] = ErrorConclusionResult(Conclusion.error(code, p))
  def error[T](e: Throwable): ConclusionResult[T] = e match {
    case m: IllegalArgumentException => badRequest(m.getMessage)
    case m: SecurityException => unauthorized(m.getMessage)
    case m: UnsupportedOperationException => notImplemented(m.getMessage)
    case m: NoSuchElementException => notFound(m.getMessage)
    case m: java.io.FileNotFoundException => notFound(m.getMessage)
    case m => internalServerError(m.getMessage)
  }

  // Specific error with detail code.
  def missing[T](p: String, ps: String*): ConclusionResult[T] = missings(p +: ps)
  def missings[T](ps: Seq[String]): ConclusionResult[T] = ErrorConclusionResult(Conclusion.missings(ps))

  //
  def execute[T](body: => T): ConclusionResult[T] = try {
    SuccessConclusionResult(body)
  } catch {
    case NonFatal(e) => error(e)
  }
  //
  def from[A](p: ParseResult[A]): ConclusionResult[A] = p match {
    case m: ParseSuccess[_] => SuccessConclusionResult(m.ast, _conclusion_success(m))
    case m: ParseFailure[_] => ErrorConclusionResult(_conclusion_error(m))
    case m: EmptyParseResult[_] => ErrorConclusionResult(_conclusion_error(m))
  }

  private def _conclusion_success(p: ParseResult[_]): Conclusion =
    Conclusion(
      200,
      None,
      errors = _messages(p.errors),
      warnings = _messages(p.warnings),
      None,
      Nil
    )

  private def _conclusion_error(p: ParseResult[_]): Conclusion = 
    Conclusion(
      400,
      None,
      errors = _messages(p.errors),
      warnings = _messages(p.warnings),
      None,
      Nil
    )

  private def _messages(ps: Vector[ParseMessage]) =
    NonEmptyVector.createOption(ps.map(_.msg))

  def from[A](p: Consequence[A]): ConclusionResult[A] = p match {
    case m: Consequence.Success[_] => SuccessConclusionResult(m.result, Conclusion.from(m.conclusion))
    case m: Consequence.Error[_] => ErrorConclusionResult(Conclusion.from(m.conclusion))
  }
}
