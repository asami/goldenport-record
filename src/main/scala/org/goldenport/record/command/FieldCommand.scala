package org.goldenport.record.command

import java.util.Date
import java.sql.Timestamp
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep. 17, 2015
 *  version Nov. 19, 2015
 *  version Mar. 28, 2020
 * @version May. 20, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait FieldCommand[T] extends ExtensionValueCommand {
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

  def convert[A](f: T => A): FieldCommand[A]
}

case class ContentCommand[T](value: T) extends FieldCommand[T] {
  override def getValueOrNoneForDelete = value_or_none_for_delete
  def convert[A](f: T => A): ContentCommand[A] = ContentCommand(f(value))
}

case class OverwriteCommand[T](value: T) extends FieldCommand[T] {
  override def getValueOrNoneForDelete = value_or_none_for_delete
  def convert[A](f: T => A): OverwriteCommand[A] = OverwriteCommand(f(value))
}

case class NullCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): NullCommand[A] = NullCommand(f(value))
}

case class UnchangeCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): UnchangeCommand[A] = UnchangeCommand(f(value))
}

case class AppendCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): AppendCommand[A] = AppendCommand(f(value))
}

case class PrependCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): PrependCommand[A] = PrependCommand(f(value))
}

case class RemoveCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): RemoveCommand[A] = RemoveCommand(f(value))
}

case class DeleteCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): DeleteCommand[A] = DeleteCommand(f(value))
}

case class ClearCommand[T](value: T) extends FieldCommand[T] {
  def convert[A](f: T => A): ClearCommand[A] = ClearCommand(f(value))
}

object FieldCommand {
  val FIELD_CONTENT = "content"
  val FIELD_OVERWRITE = "overwrite"
  val FIELD_NULL = "null"
  val FIELD_UNCHANGE = "unchange"
  val FIELD_APPEND = "append"
  val FIELD_PREPEND = "prepend"
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

  def parse(mode: UpdateMode, p: Record): Record = p.transform(_parse(mode, _))

  private def _parse(mode: UpdateMode, p: Field): List[Field] =
    _get_command(mode, p).map(x => List(p.update(List(x)))).getOrElse(Nil)

  def getCommand(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Any]] = getCommand(mode, rec, key.name)

  def getCommand(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Any]] =
    rec.getField(key).
      map(_get_command(mode, _)).
      getOrElse(
        rec.getEffectiveValue(_make_key(key, FIELD_CONTENT)).map(ContentCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_OVERWRITE)).map(OverwriteCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_NULL)).map(NullCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_UNCHANGE)).map(UnchangeCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_APPEND)).map(AppendCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_PREPEND)).map(PrependCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_REMOVE)).map(RemoveCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_DELETE)).map(DeleteCommand(_)) orElse
          rec.getEffectiveValue(_make_key(key, FIELD_CLEAR)).map(ClearCommand(_))
      )

  private def _make_key(key: String, cmd: String) = key + DELIMITER + cmd

  private def _get_command(mode: UpdateMode, field: Field): Option[FieldCommand[Any]] =
    field.effectiveValue match {
      case Some(s) =>
        val isempty = s match {
          case Nil => true
          case m: Seq[_] if m.isEmpty => true
          case "" => true
          case _ => false
        }
        if (isempty) // XXX in case of collection
          mode.emptyStringMode match {
            case UpdateMode.StringEmptyMode => Some(OverwriteCommand(s))
            case UpdateMode.VoidEmptyMode => None
            case UpdateMode.NullEmptyMode => Some(NullCommand(s))
          }
          else
            Some(OverwriteCommand(s))
      case None =>  // The field has Nil value.
        mode.emptyValueMode match {
          case UpdateMode.StringEmptyMode => Some(OverwriteCommand(""))
          case UpdateMode.VoidEmptyMode => None
          case UpdateMode.NullEmptyMode => Some(NullCommand(Nil))
        }
    }

  def getCommandString(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[String]] = getCommandString(mode, rec, key)

  def getCommandBoolean(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Boolean]] = getCommandBoolean(mode, rec, key)

  def getCommandInt(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Int]] = getCommandInt(mode, rec, key)

  def getCommandFloat(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Float]] = getCommandFloat(mode, rec, key)

  def getCommandDouble(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Double]] = getCommandDouble(mode, rec, key)

  def getCommandDate(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Date]] = getCommandDate(mode, rec, key)

  def getCommandTimestamp(
    mode: UpdateMode,
    rec: Record,
    key: Symbol
  ): Option[FieldCommand[Timestamp]] = getCommandTimestamp(mode, rec, key)

  def getCommandString(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[String]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toString))

  def getCommandBoolean(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Boolean]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toBoolean))

  def getCommandInt(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Int]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toInt))

  def getCommandFloat(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Float]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toFloat))

  def getCommandDouble(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Double]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toDouble))

  def getCommandDate(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Date]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toDate))

  def getCommandTimestamp(
    mode: UpdateMode,
    rec: Record,
    key: String
  ): Option[FieldCommand[Timestamp]] = getCommand(mode, rec, key).map(_.convert(AnyUtils.toTimestamp))

  /*
   * legacy
   */
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
    rec.getString(makekey(FIELD_NULL)).map(NullCommand(_)) orElse
    rec.getString(makekey(FIELD_UNCHANGE)).map(UnchangeCommand(_)) orElse
    rec.getConcreteString(makekey(FIELD_APPEND)).map(AppendCommand(_)) orElse
    rec.getConcreteString(makekey(FIELD_PREPEND)).map(PrependCommand(_)) orElse
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
    f(makekey(FIELD_NULL)).map(NullCommand(_)) orElse
    f(makekey(FIELD_UNCHANGE)).map(UnchangeCommand(_)) orElse
    f(makekey(FIELD_APPEND)).map(AppendCommand(_)) orElse
    f(makekey(FIELD_PREPEND)).map(PrependCommand(_)) orElse
    f(makekey(FIELD_REMOVE)).map(RemoveCommand(_)) orElse
    f(makekey(FIELD_DELETE)).map(DeleteCommand(_)) orElse
    f(makekey(FIELD_CLEAR)).map(ClearCommand(_))
  }
}
