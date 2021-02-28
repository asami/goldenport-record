package org.goldenport.record.v2

import scala.util.control.NonFatal
import scala.math.BigInt
import scala.math.BigDecimal
import java.util.Locale
import com.typesafe.config._
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.util.StringUtils
// import org.goldenport.util.NumberUtils
import org.goldenport.record.util.AnyUtils

/*
 * @since   Aug. 12, 2015
 *  version Sep. 25, 2015
 *  version Oct. 25, 2015
 *  version Nov. 23, 2015
 *  version Dec.  9, 2015
 *  version Nov. 12, 2017
 *  version Jan. 11, 2020
 *  version Jun. 18, 2020
 *  version Aug. 19, 2020
 *  version Oct. 28, 2020
 * @version Feb. 20, 2021
 * @author  ASAMI, Tomoharu
 */
trait Powertype {
  def value: Int
  def name: String
  def label: String = name
  def i18nLabel: Option[I18NString] = None
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

  def label(locale: Locale): String = i18nLabel.flatMap(_.get(locale)).getOrElse(label)

  private lazy val _value_string = value.toString

  def isMatch(p: Any): Boolean = p match {
    case m: String => m == name || m == _value_string
    case m: Int => m == value
    case m => m == this
  }
}

trait PowertypeClass {
  type T <: Powertype

  def name: String = StringUtils.className(this)

  def elements: Seq[T]

  def default = elements.head

  def get(v: Int): Option[T] = elements.find(_.value == v)

  def get(v: String): Option[T] = {
    elements.find(_.name == v) orElse
    elements.find(_.aliases.contains(v)) orElse
    elements.find(_.label == v) orElse {
      try {
        get(v.toInt)
      } catch {
        case e: NumberFormatException => None
      }
    }
  }

  def get(config: Config, key: String): Option[T] = config.getStringOption(key).flatMap(get)

  def getName(v: Int): Option[String] = get(v).map(_.name)
  def getName(v: String): Option[String] = get(v).map(_.name)
  def getName(config: Config, key: String): Option[String] = get(config, key).map(_.name)
  def getLabel(v: Int): Option[String] = get(v).map(_.label)
  def getLabel(v: String): Option[String] = get(v).map(_.label)
  def getLabel(config: Config, key: String): Option[String] = get(config, key).map(_.label)
  def getValue(v: Int): Option[Int] = get(v).map(_.value)
  def getValue(v: String): Option[Int] = get(v).map(_.value)
  def getValue(config: Config, key: String): Option[Int] = get(config, key).map(_.value)

  def toValue(v: Any): Int = Option(v).flatMap {
    case m: Int => getValue(m)
    case m: Number => getValue(m.intValue)
    case m: String => getValue(m)
    case m => getValue(m.toString)
  }.getOrElse {
    RAISE.invalidArgumentFault(s"Unknown powertype($name): $v")
  }

  def toValues(vs: Seq[Any]): Seq[Int] = vs.map(toValue)

  def makeName(v: Any): String = Option(v).flatMap {
    case m: Int => getName(m)
    case m: Number => getName(m.intValue)
    case m: String => getName(m)
    case m => getName(AnyUtils.toString(m))
  }.getOrElse {
    AnyUtils.toString(v)
  }

  def makeLabel(v: Any): String = Option(v).flatMap {
    case m: Int => getLabel(m)
    case m: Number => getLabel(m.intValue)
    case m: String => getLabel(m)
    case m => getLabel(AnyUtils.toString(m))
  }.getOrElse {
    AnyUtils.toString(v)
  }

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

  def paramString(v: Option[String], default: T): T = {
    v.map(paramString) getOrElse default
  }

  def getParamString(v: Option[String]): Option[T] = {
    v.flatMap(getParamString)
  }

  def getParamString(v: String): Option[T] = {
    val x = v.trim
    try {
      get(x.toInt)
    } catch {
      case NonFatal(e) => get(x)
    }
  }

  // def getValue(rec: Record, key: String): Option[Int] =
  //   rec.getOne(key).map {
  //     case m: Int => m
  //     case m: String => apply(m).value
  //     case m => NumberUtils.getInt(m).getOrElse(RAISE.invalidArgumentFault(s"Unavailable value: $m"))
  //   }

  def normalizeValue(rec: Record, key: String): Record = {
    getParamString(rec.getString(key)) match {
      case Some(s) => rec.update(key -> s.value)
      case None => rec
    }
  }

  def normalizeName(rec: Record, key: String): Record = {
    getParamString(rec.getString(key)) match {
      case Some(s) => rec.update(key -> s.name)
      case None => rec
    }
  }

  def normalizeLabel(rec: Record, key: String): Record = {
    getParamString(rec.getString(key)) match {
      case Some(s) => rec.update(key -> s.label)
      case None => rec
    }
  }

  def toInstance(v: Any): Int = {
    v match {
      case x: Int => paramInt(x).value
      case x: Long => paramInt(x.toInt).value
      case x: String => paramString(x).value
      case x: Byte => paramInt(x.toInt).value
      case x: Short => paramInt(x.toInt).value
      case x: BigInt => paramInt(x.toInt).value
      case x: BigDecimal => paramInt(x.toInt).value
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
      case x: BigInt => isValid(x.toInt)
      case x: BigDecimal => isValid(x.toInt)
      case _ => false
    }
  }
}
