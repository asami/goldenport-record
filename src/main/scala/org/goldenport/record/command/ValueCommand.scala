package org.goldenport.record.command

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep.  8, 2016
 *  version Feb. 24, 2019
 *  version Oct.  5, 2019
 * @version Mar. 28, 2020
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
case class StringValue(s: String) extends ValueCommand {
  override def getSqlLiteralForSet = Some(s""""${s}"""")
}
case class NumberValue(n: Number) extends ValueCommand {
  override def getSqlLiteralForSet = Some(s"${n}")
}
trait ExtensionValueCommand extends ValueCommand

object ValueCommand {
  type CommandOrValue[T] = \/[ValueCommand, T]

  def NotExistCommand[T]: CommandOrValue[T] = -\/(NotExist)

  def parse(mode: UpdateMode, p: Record): Record = p.transform(_parse(mode, _))

  private def _parse(mode: UpdateMode, p: Field): List[Field] = {
    def result(r: Any) = List(p.update(List(r)))
    p.effectiveValue match {
      case Some(s) =>
        val isempty = s match {
          case Nil => true
          case m: Seq[_] if m.isEmpty => true
          case "" => true
          case _ => false
        }
        if (isempty) // XXX in case of collection
          mode.emptyStringMode match {
            case UpdateMode.StringEmptyMode => result(s)
            case UpdateMode.VoidEmptyMode => Nil
            case UpdateMode.NullEmptyMode => result(NullValue)
          }
          else
           s match {
              case ContentCommand(v) => result(v)
              case OverwriteCommand(v) => result(v)
              case m: NullCommand[_] => result(NullValue)
              case m: UnchangeCommand[_] => Nil
              case m => result(m)
            }
      case None =>  // The field has Nil value.
        mode.emptyValueMode match {
          case UpdateMode.StringEmptyMode => result("")
          case UpdateMode.VoidEmptyMode => Nil
          case UpdateMode.NullEmptyMode => result(NullValue)
        }
    }
  }

  implicit class CommandOrValueWrapper[T](val self: CommandOrValue[T]) extends AnyVal {
    def mapWithNull[A](f: T => A)(nullf: => A): Option[A] =
      ValueCommand.this.mapWithNull(self)(f)(nullf)
  }

  def mapWithNull[T, A](t: CommandOrValue[T])(f: T => A)(nullf: => A): Option[A] = {
    t match {
      case -\/(l) => l match {
        case NotExist => None
        case NullValue => Some(nullf)
        case m: StringValue => RAISE.unsupportedOperationFault // TODO
        case m: NumberValue => RAISE.unsupportedOperationFault // TODO
        case m: ExtensionValueCommand => RAISE.unsupportedOperationFault // TODO
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
