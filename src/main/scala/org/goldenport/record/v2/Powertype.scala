package org.goldenport.record.v2

import scala.util.control.NonFatal

/*
 * @since   Aug. 12, 2015
 *  version Sep. 25, 2015
 * @version Oct. 25, 2015
 * @author  ASAMI, Tomoharu
 */
trait Powertype {
  def value: Int
  def name: String
  def label: String = name
  def aliases: Seq[String] = Nil
  def mark: Option[String] = None
  def isMark(c: Character): Boolean = {
    mark match {
      case Some(s) => s == c.toString
      case None => false
    }
  }
  def checkMark(s: String): Option[String] = {
    mark flatMap { x =>
      if (s.startsWith(x))
        Some(s.substring(x.length))
      else
        None
    }
  }
}

trait PowertypeClass {
  type T <: Powertype

  def elements: Seq[T]

  def default = elements.head

  def get(v: Int): Option[T] = elements.find(_.value == v)

  def get(v: String): Option[T] = {
    elements.find(_.name == v) orElse
    elements.find(_.aliases.contains(v)) orElse
    elements.find(_.label == v)
  }

  def getName(v: Int): Option[String] = get(v).map(_.name)
  def getName(v: String): Option[String] = get(v).map(_.name)
  def getLabel(v: Int): Option[String] = get(v).map(_.label)
  def getLabel(v: String): Option[String] = get(v).map(_.label)
  def getValue(v: Int): Option[Int] = get(v).map(_.value)
  def getValue(v: String): Option[Int] = get(v).map(_.value)

  def getsByMarkAndRemainder(s: String): (Seq[T], Option[String]) = {
    case class Z(
      kinds: Vector[T] = Vector.empty,
      unknown: Vector[Character] = Vector.empty
    ) {
      val delimiter = " ,;\t\n\r"
      def +(c: Character) = {
        if (delimiter.exists(_ == c))
          this
        else
          elements.find(_.isMark(c)) match {
            case Some(s) => copy(kinds = kinds :+ s)
            case None => copy(unknown = unknown :+ c)
          }
      }
      def result = {
        if (unknown.isEmpty)
          (kinds, None)
        else
          (kinds, Some(unknown.mkString))
      }
    }
    s.foldLeft(Z())(_ + _).result
  }

  def getsByMark(s: String): Seq[T] = {
    val (xs, remainder) = getsByMarkAndRemainder(s)
    if (remainder.isEmpty)
      xs
    else
      throw new IllegalArgumentException(s"Unknown marks '$remainder'")
  }

  def apply(v: Int): T = paramInt(v)
  def apply(v: String): T = paramString(v)

  def usage = elements.map(x => s"${x.name}:${x.value}").mkString("(", " ,", ")")

  def paramInt(v: Int): T = {
    get(v) getOrElse {
      throw new IllegalArgumentException(s"Invalid powertype value '$v' for $usage")
    }
  }

  def paramInt(v: Option[Int]): T = {
    v.map(paramInt) getOrElse default
  }

  def paramString(v: String): T = {
    val x = v.trim
    try {
      paramInt(x.toInt)
    } catch {
      case NonFatal(e) =>
        get(x) getOrElse {
          throw new IllegalArgumentException(s"Invalid powertype value '$v' for $usage")
        }
    }
  }

  def paramString(v: Option[String]): T = {
    v.map(paramString) getOrElse default
  }

  def getParamString(v: String): Option[T] = {
    val x = v.trim
    try {
      get(x.toInt)
    } catch {
      case NonFatal(e) => get(x)
    }
  }

  def toInstance(v: Any): Int = {
    v match {
      case x: Int => paramInt(x).value
      case x: Long => paramInt(x.toInt).value
      case x: String => paramString(x).value
      case x: Byte => paramInt(x.toInt).value
      case x: Short => paramInt(x.toInt).value
      case _ =>
        throw new IllegalArgumentException(s"Invalid powertype value '$v' for $usage")
    }
  }

  def isValid(v: Int) = get(v).isDefined
  def isValid(v: String) = getParamString(v).isDefined

  def validate(v: Any): Boolean = {
    v match {
      case x: Int => isValid(x)
      case x: Long => isValid(x.toInt)
      case x: String => isValid(x)
      case x: Byte => isValid(x.toInt)
      case x: Short => isValid(x.toInt)
      case _ => false
    }
  }
}
