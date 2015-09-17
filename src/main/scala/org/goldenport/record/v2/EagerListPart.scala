package org.goldenport.record.v2

import scalaz.NonEmptyList
import org.goldenport.Strings
import org.goldenport.record.command._
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep. 17, 2015
 * @version Sep. 17, 2015
 * @author  ASAMI, Tomoharu
 */
trait EagerListPart { self: Record =>
  def getEagerStringList(key: Symbol): Option[NonEmptyList[String]] = {
    get(key).toList.flatMap(_expands).map(AnyUtils.toString).
      filter(Strings.notblankp) match {
        case Nil => None
        case x :: xs => Some(NonEmptyList.nel(x, xs))
      }
  }

  def getEagerStringList(key: String): Option[NonEmptyList[String]] = {
    getEagerStringList(Symbol(key))
  }

  private def _expands(x: Any): List[Any] = {
    x match {
      case xs: Seq[_] => xs.toList.flatMap(_expands)
      case x: String => Strings.totokens(x)
      case x => List(x)
    }
  }
}
