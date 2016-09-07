package org.goldenport.record.command

import scalaz._, Scalaz._
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep.  8, 2016
 * @version Sep.  8, 2016
 * @author  ASAMI, Tomoharu
 */
sealed trait ValueCommand {
  def getSqlLiteralForSet: Option[String] = None
}
case object NotExist extends ValueCommand {
}
case object NullValue extends ValueCommand {
  override def getSqlLiteralForSet = Some("NULL")
}
trait ExtensionValueCommand extends ValueCommand

object ValueCommand {
  type CommandOrValue[T] = \/[ValueCommand, T]

  def NotExistCommand[T]: CommandOrValue[T] = -\/(NotExist)

  implicit class CommandOrValueWrapper[T](val self: CommandOrValue[T]) extends AnyVal {
    def mapWithNull[A](f: T => A)(nullf: => A): Option[A] =
      ValueCommand.this.mapWithNull(self)(f)(nullf)
  }

  def mapWithNull[T, A](t: CommandOrValue[T])(f: T => A)(nullf: => A): Option[A] = {
    t match {
      case -\/(l) => l match {
        case NotExist => None
        case NullValue => Some(nullf)
        case m: ExtensionValueCommand => None
      }
      case \/-(r) => Some(f(r))
    }
  }

  def getStringC(rec: Record, name: Symbol): CommandOrValue[String] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: ValueCommand => -\/(c)
        case s: String => \/-(s)
        case _ => \/-(rec.asString(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getStringFormC(rec: Record, name: Symbol): CommandOrValue[String] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: ValueCommand => -\/(c)
        case "" => -\/(NotExist)
        case s: String => \/-(s)
        case _ => \/-(rec.asString(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getBooleanC(rec: Record, name: Symbol): CommandOrValue[Boolean] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: ValueCommand => -\/(c)
        case s: Boolean => \/-(s)
        case _ => \/-(rec.asBoolean(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getIntC(rec: Record, name: Symbol): CommandOrValue[Int] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: ValueCommand => -\/(c)
        case s: Int => \/-(s)
        case _ => \/-(rec.asInt(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getIntFormC(rec: Record, name: Symbol): CommandOrValue[Int] = {
    rec.getOne(name) match {
      case Some(s) => s match {
        case c: ValueCommand => -\/(c)
        case s: Int => \/-(s)
        case "" => -\/(NotExist)
        case _ => \/-(rec.asInt(name))
      }
      case None => -\/(NotExist)
    }
  }

  def getBooleanC(rec: Record, name: String): CommandOrValue[Boolean] = {
    getBooleanC(rec, Symbol(name))
  }

  def getIntC(rec: Record, name: String): CommandOrValue[Int] = {
    getIntC(rec, Symbol(name))
  }

  def getStringC(rec: Record, name: String): CommandOrValue[String] = {
    getStringC(rec, Symbol(name))
  }
}
