package org.goldenport.record.command

import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep. 17, 2015
 * @version Nov. 19, 2015
 * @author  ASAMI, Tomoharu
 */
sealed trait FieldCommand[T] {
  def value: T
  def getValueOrNoneForDelete: Option[Option[T]] = {
    throw new IllegalArgumentException(s"Unavailable command: $this")
  }

  protected def value_or_none_for_delete: Option[Option[T]] = {
    value match {
      case "" => Some(None)
      case _ => Some(Some(value))
    }
  }
}

case class ContentCommand[T](value: T) extends FieldCommand[T] {
  override def getValueOrNoneForDelete = value_or_none_for_delete
}

case class OverwriteCommand[T](value: T) extends FieldCommand[T] {
  override def getValueOrNoneForDelete = value_or_none_for_delete
}

case class AppendCommand[T](value: T) extends FieldCommand[T] {
}

case class RemoveCommand[T](value: T) extends FieldCommand[T] {
}

case class DeleteCommand[T](value: T) extends FieldCommand[T] {
}

case class ClearCommand[T](value: T) extends FieldCommand[T] {
}

object FieldCommand {
  val FIELD_CONTENT = "content"
  val FIELD_OVERWRITE = "overwrite"
  val FIELD_APPEND = "append"
  val FIELD_REMOVE = "remove"
  val FIELD_DELETE = "delete"
  val FIELD_CLEAR = "clear"

  val DELIMITER = "__"
  val DELIMITER_SIZE = DELIMITER.length

  def parseKeyCommand(s: String): Option[(String, String)] = {
    s.lastIndexOf(DELIMITER) match {
      case -1 => None
      case 0 => None
      case i =>
        val prefix = s.substring(0, i)
        val postfix = s.substring(i + DELIMITER_SIZE)
        if (postfix == "") None else Some(prefix, postfix)
    }
  }

  def getCmdString(rec: Record, key: Symbol, isoverwrite: Boolean): Option[FieldCommand[String]] = {
    getCmdString(rec, key.name, isoverwrite)
  }

  def getCmdString(rec: Record, key: String, isoverwrite: Boolean): Option[FieldCommand[String]] = {
    def makekey(cmd: String) = key + DELIMITER + cmd
    def getoverwritestring = if (isoverwrite)
      rec.getString(key)
    else
      rec.getConcreteString(key)
    getoverwritestring.map(OverwriteCommand(_)) orElse
    rec.getString(makekey(FIELD_CONTENT)).map(ContentCommand(_)) orElse
    rec.getString(makekey(FIELD_OVERWRITE)).map(OverwriteCommand(_)) orElse
    rec.getConcreteString(makekey(FIELD_APPEND)).map(AppendCommand(_)) orElse
    rec.getConcreteString(makekey(FIELD_REMOVE)).map(RemoveCommand(_)) orElse
    rec.getConcreteString(makekey(FIELD_DELETE)).map(DeleteCommand(_)) orElse
    rec.getConcreteString(makekey(FIELD_CLEAR)).map(ClearCommand(_))
  }

  def getCmdInt(rec: Record, key: Symbol, isoverwrite: Boolean): Option[FieldCommand[Int]] = {
    getCmdInt(rec, key.name, isoverwrite)
  }

  def getCmdInt(rec: Record, key: String, isoverwrite: Boolean): Option[FieldCommand[Int]] = {
    def makekey(cmd: String) = key + DELIMITER + cmd
    def f(k: String) = rec.getConcreteString(k).map(AnyUtils.toInt)
    def getoverwriteint = if (isoverwrite)
      rec.getString(key).map(AnyUtils.toInt)
    else
      f(key)
    getoverwriteint.map(OverwriteCommand(_)) orElse
    f(makekey(FIELD_CONTENT)).map(ContentCommand(_)) orElse
    f(makekey(FIELD_OVERWRITE)).map(OverwriteCommand(_)) orElse
    f(makekey(FIELD_APPEND)).map(AppendCommand(_)) orElse
    f(makekey(FIELD_REMOVE)).map(RemoveCommand(_)) orElse
    f(makekey(FIELD_DELETE)).map(DeleteCommand(_)) orElse
    f(makekey(FIELD_CLEAR)).map(ClearCommand(_))
  }
}
