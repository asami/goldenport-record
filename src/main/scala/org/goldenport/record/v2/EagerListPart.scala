package org.goldenport.record.v2

import scalaz.NonEmptyList
import java.net.URL
import org.goldenport.Strings
import org.goldenport.record.command._
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep. 17, 2015
 * @version Oct. 25, 2015
 * @author  ASAMI, Tomoharu
 */
trait EagerListPart { self: Record =>
  def getEagerStringListOption(key: Symbol): Option[NonEmptyList[String]] = {
    getEagerStringListOption(key)
  }

  def getEagerStringListOption(key: String): Option[NonEmptyList[String]] = {
    getEagerStringListOption(key)
  }

  // TODO refactor signature
  def getEagerStringList(key: Symbol): Option[NonEmptyList[String]] = {
    get(key).toList.flatMap(_expands).map(AnyUtils.toString).
      filter(Strings.notblankp) match {
        case Nil => None
        case x :: xs => Some(NonEmptyList.nel(x, xs))
      }
  }

  // TODO refactor signature
  def getEagerStringList(key: String): Option[NonEmptyList[String]] = {
    getEagerStringList(Symbol(key))
  }

  def getEagerUrlListOption(key: Symbol): Option[NonEmptyList[URL]] = {
    get(key).toList.flatMap(_expands).
      filter {
        case s: String => Strings.notblankp(s)
        case _ => true
      }.
      map(AnyUtils.toUrl) match {
        case Nil => None
        case x :: xs => Some(NonEmptyList.nel(x, xs))
      }
  }

  def getEagerUrlListOption(key: String): Option[NonEmptyList[URL]] = {
    getEagerUrlListOption(Symbol(key))
  }

  def getEagerUrlList(key: Symbol): List[URL] = {
    getEagerUrlListOption(key) match {
      case Some(s) => s.list
      case None => Nil
    }
  }

  def getEagerUrlList(key: String): List[URL] = {
    getEagerUrlListOption(key) match {
      case Some(s) => s.list
      case None => Nil
    }
  }

  private def _expands(x: Any): List[Any] = {
    x match {
      case xs: Seq[_] => xs.toList.flatMap(_expands)
      case x: String => Strings.totokens(x)
      case x => List(x)
    }
  }

  // def getEffectiveList(key: Symbol): List[Any] = {
  //   getFormList(key) map {
  //     case s: String => Strings.totokens(s)
  //     case m => m
  //   }
  // }

  // def getEffectiveList(key: Symbol, delimiters: String): List[Any] = {
  //   getFormList(key).map {
  //     case s: String => Strings.totokens(s, delimiters)
  //     case m => m
  //   }
  // }

  // def getEffectiveUrlList(key: Symbol): List[URL] = {
  //   getEffectiveList(key).map(AnyUtils.toUrl)
  // }

  // def getEffectiveList(key: String): List[Any] = {
  //   getEffectiveList(Symbol(key))
  // }

  // def getEffectiveUrlList(key: String): List[URL] = {
  //   getEffectiveList(key).map(AnyUtils.toUrl)
  // }

  // def getEffectiveStringList: List[String] = {
  //   getConcreteStringList.flatMap(Strings.totokens(_))
  // }
}
