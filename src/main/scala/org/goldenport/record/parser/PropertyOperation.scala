package org.goldenport.record.parser

import org.goldenport.RAISE
import org.goldenport.parser.ParseResult
import org.goldenport.record.v3.{Record, Field}
import org.goldenport.record.command._

/*
 * @since   Mar. 21, 2021
 * @version Mar. 21, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait PropertyOperation {
  def name: String
  def postfix = s"__$name"

  def isMatchProperty(key: String): Boolean = key.endsWith(postfix)

  def unapply(key: String): Option[String] = {
    if (isMatchProperty(key))
      Some(key.dropRight(postfix.length))
    else
      None
  }
}

object PropertyOperation {
  case object ContentPropertyOperation extends PropertyOperation {
    def name = "content"

    // def getString(key: String, rec: Record): Option[String] = {
    //   val k = key + postfix
    //   rec.getString(k) orElse rec.getString(key) // TODO form
    // }
  }

  case object UnchangePropertyOperation extends PropertyOperation {
    def name = "unchange"
  }

  case object AppendPropertyOperation extends PropertyOperation {
    def name = "append"
  }

  case object OverwritePropertyOperation extends PropertyOperation {
    def name = "overwrite"
  }

  case object RemovePropertyOperation extends PropertyOperation {
    def name = "remove"
  }

  case object DeletePropertyOperation extends PropertyOperation {
    def name = "delete"
  }

  case object RemoveAllPropertyOperation extends PropertyOperation {
    def name = "remove-all"
  }

  case object DeleteAllPropertyOperation extends PropertyOperation {
    def name = "delete-all"
  }

  case object ClearPropertyOperation extends PropertyOperation {
    def name = "clear"
  }

  case object CommandPropertyOperation extends PropertyOperation {
    def name = "command"
  }

  case object SearchPropertyOperation extends PropertyOperation {
    def name = "search"
  }

  val elements = Vector(
    ContentPropertyOperation,
    UnchangePropertyOperation,
    AppendPropertyOperation,
    OverwritePropertyOperation,
    RemovePropertyOperation,
    DeletePropertyOperation,
    RemoveAllPropertyOperation,
    DeleteAllPropertyOperation,
    ClearPropertyOperation,
    CommandPropertyOperation,
    SearchPropertyOperation
  )

  def getOperation(f: Field): Option[PropertyOperation] =
    getOperationName(f).flatMap(name => elements.find(_.name == name))

  def getOperationName(f: Field): Option[String] = {
    val key = f.key.name
    key.lastIndexOf("__") match {
      case -1 => None
      case i => Some(key.substring(i + "__".length))
    }
  }

  // def makeOperation(p: UpdateMode): PropertyOperation = p match {
  //   case NoneUpdateMode => UnchangePropertyOperation
  //   case OverwriteUpdateMode => OverwritePropertyOperation
  //   case AppendUpdateMode => AppendPropertyOperation
  //   case LegacyUpdateMode => OverwritePropertyOperation
  // }

  def toCommand[T](op: PropertyOperation, p: T): ParseResult[FieldCommand[T]] = op match {
    case ContentPropertyOperation => ParseResult.success(ContentCommand(p))
    case UnchangePropertyOperation => ParseResult.success(UnchangeCommand(p))
    case AppendPropertyOperation => ParseResult.success(AppendCommand(p))
    case OverwritePropertyOperation => ParseResult.success(OverwriteCommand(p))
    case RemovePropertyOperation => ParseResult.success(RemoveCommand(p))
    case DeletePropertyOperation => RAISE.notImplementedYetDefect
    case RemoveAllPropertyOperation => RAISE.notImplementedYetDefect
    case DeleteAllPropertyOperation => RAISE.notImplementedYetDefect
    case ClearPropertyOperation => ParseResult.success(ClearCommand())
    case CommandPropertyOperation => _to_command(p)
    case SearchPropertyOperation => RAISE.notImplementedYetDefect
  }

  private def _to_command[T](p: T): ParseResult[FieldCommand[T]] = p match {
    case m if m == RemoveAllPropertyOperation.name => ParseResult.success(RemoveAllCommand())
    case _ => ParseResult.error(s"""Invalid command: ${p}""")
  }
}
