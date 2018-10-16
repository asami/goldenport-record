package org.goldenport.record.v2

import scalaz.{NonEmptyList, IList}
import java.net.URL
import org.goldenport.Strings
import org.goldenport.record.command._
import org.goldenport.record.util.AnyUtils

/*
 * Latest naming policy
 *
 * get means using Option
 * effective means handling effective list structure
 * form means handling empty html form is no parameter
 * eager means handling eager delimiters separated list
 * comma means handling comma delimiter separated list
 *
 * See RecordAux in lib.
 * 
 * @since   Sep. 17, 2015
 *  version Oct. 25, 2015
 *  version Nov. 23, 2015
 *  version Dec. 11, 2015
 *  version Feb. 26, 2016
 *  version Apr. 27, 2016
 * @version Oct. 17, 2016
 * @author  ASAMI, Tomoharu
 */
trait EagerListPart { self: Record =>
  def eagerStringList(key: String): List[String] =
    eagerStringList(Symbol(key))

  def eagerStringList(key: Symbol): List[String] =
    getEagerStringList(key) getOrElse Nil

  def getEagerStringList(key: String): Option[List[String]] =
    getEagerStringList(Symbol(key))

  def getEagerFormStringList(key: String): Option[NonEmptyList[String]] =
    getEagerFormStringList(Symbol(key))

  def getEagerStringList(key: Symbol): Option[List[String]] = {
    get(key).map(x => _expands(x).map(AnyUtils.toString))
  }

  def getEagerFormStringList(key: Symbol): Option[NonEmptyList[String]] = {
    get(key).toList.flatMap(_expands).map(AnyUtils.toString).
      filter(Strings.notblankp) match {
        case Nil => None
        case x :: xs => Some(NonEmptyList.nel(x, IList.fromList(xs)))
      }
  }

  // Long
  def eagerLongList(key: String): List[Long] =
    eagerLongList(Symbol(key))

  def eagerLongList(key: Symbol): List[Long] =
    _to_list(getEagerLongList(key))

  def getEagerLongList(key: String): Option[List[Long]] =
    getEagerLongList(Symbol(key))

  def getEagerLongList(key: Symbol): Option[List[Long]] =
    getEagerFormLongList(key).map(_.list.toList)

  def getEagerFormLongList(key: String): Option[NonEmptyList[Long]] =
    getEagerFormLongList(Symbol(key))

  def getEagerFormLongList(key: Symbol): Option[NonEmptyList[Long]] = {
    val a = get(key).toList.flatMap(_expands).
      filter {
        case s: String => Strings.notblankp(s)
        case _ => true
      }.map(AnyUtils.toLong): List[Long]
    a match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, IList.fromList(xs)))
    }
  }

  // URL
  def eagerUrlList(key: String): List[URL] = eagerUrlList(Symbol(key))

  def getEagerUrlList(key: String): Option[List[URL]] =
    getEagerUrlList(Symbol(key))

  def eagerUrlList(key: Symbol): List[URL] =
    _to_list(getEagerUrlList(key))

  private def _to_list[T](p: Option[List[T]]) = p getOrElse Nil

  def getEagerUrlList(key: Symbol): Option[List[URL]] =
    getEagerFormUrlList(key).map(_.list.toList)

  def getEagerFormUrlList(key: Symbol): Option[NonEmptyList[URL]] = {
    val a = get(key).toList.flatMap(_expands).
      filter {
        case s: String => Strings.notblankp(s)
        case _ => true
      }.map(AnyUtils.toUrl): List[URL]
    a match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, IList.fromList(xs)))
    }
  }

  // def getEagerStringListOption(key: Symbol): Option[NonEmptyList[String]] = {
  //   getEagerStringListOption(key)
  // }

  // def getEagerStringListOption(key: String): Option[NonEmptyList[String]] = {
  //   getEagerStringListOption(key)
  // }

  // TODO refactor signature
  // def getEagerStringList(key: Symbol): Option[NonEmptyList[String]] = {
  //   get(key).toList.flatMap(_expands).map(AnyUtils.toString).
  //     filter(Strings.notblankp) match {
  //       case Nil => None
  //       case x :: xs => Some(NonEmptyList.nel(x, xs))
  //     }
  // }

  // TODO refactor signature
  // def getEagerStringList(key: String): Option[NonEmptyList[String]] = {
  //   getEagerStringList(Symbol(key))
  // }

  // TODO refactor signature
  def getEagerUrlListOption(key: Symbol): Option[NonEmptyList[URL]] =
    getEagerFormUrlList(key)

  // TODO refactor signature
  // def getEagerUrlListOption(key: String): Option[NonEmptyList[URL]] = {
  //   getEagerUrlListOption(Symbol(key))
  // }

  // TODO refactor signature
  // def getEagerUrlList(key: Symbol): List[URL] = {
  //   getEagerUrlListOption(key) match {
  //     case Some(s) => s.list
  //     case None => Nil
  //   }
  // }

  // TODO refactor signature
  // def getEagerUrlList(key: String): List[URL] = {
  //   getEagerUrlListOption(key) match {
  //     case Some(s) => s.list
  //     case None => Nil
  //   }
  // }

  // TODO refactor signature
  // def getEagerLongListOption(key: Symbol): Option[NonEmptyList[Long]] = {
  //   get(key).toList.flatMap(_expands).
  //     filter {
  //       case s: String => Strings.notblankp(s)
  //       case _ => true
  //     }.
  //     map(AnyUtils.toLong) match {
  //       case Nil => None
  //       case x :: xs => Some(NonEmptyList.nel(x, xs))
  //     }
  // }

  // TODO refactor signature
  // def getEagerLongListOption(key: String): Option[NonEmptyList[Long]] = {
  //   getEagerLongListOption(Symbol(key))
  // }

  // TODO refactor signature
  // def getEagerLongList(key: Symbol): List[Long] = {
  //   getEagerLongListOption(key) match {
  //     case Some(s) => s.list
  //     case None => Nil
  //   }
  // }

  // TODO refactor signature
  // def getEagerLongList(key: String): List[Long] = {
  //   getEagerLongListOption(key) match {
  //     case Some(s) => s.list
  //     case None => Nil
  //   }
  // }

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
