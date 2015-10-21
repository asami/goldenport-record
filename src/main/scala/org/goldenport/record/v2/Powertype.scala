package org.goldenport.record.v2

import scala.util.control.NonFatal

/*
 * @since   Aug. 12, 2015
 * @version Sep. 25, 2015
 * @author  ASAMI, Tomoharu
 */
trait Powertype {
  def value: Int
  def name: String
  def label: String = name
}

trait PowertypeClass {
  type T <: Powertype

  def elements: Seq[T]

  def default = elements.head

  def get(v: Int): Option[T] = elements.find(_.value == v)

  def get(v: String): Option[T] = {
    elements.find(_.name == v) orElse
    elements.find(_.label == v)
  }

  def getName(v: Int): Option[String] = get(v).map(_.name)
  def getName(v: String): Option[String] = get(v).map(_.name)
  def getLabel(v: Int): Option[String] = get(v).map(_.label)
  def getLabel(v: String): Option[String] = get(v).map(_.label)
  def getValue(v: Int): Option[Int] = get(v).map(_.value)
  def getValue(v: String): Option[Int] = get(v).map(_.value)

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
      case x: String => paramString(x).value
      case _ =>
        throw new IllegalArgumentException(s"Invalid powertype value '$v' for $usage")
    }
  }

  def isValid(v: Int) = get(v).isDefined
  def isValid(v: String) = getParamString(v).isDefined

  def validate(v: Any): Boolean = {
    v match {
      case x: Int => isValid(x)
      case x: String => isValid(x)
      case _ => false
    }
  }
}
