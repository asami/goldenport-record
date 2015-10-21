package org.goldenport.record.v2

import org.goldenport.record.command._

/*
 * @since   Sep. 17, 2015
 * @version Sep. 17, 2015
 * @author  ASAMI, Tomoharu
 */
trait CommandPart { self: Record =>
  def getCmdString(key: Symbol): Option[FieldCommand[String]] = {
    FieldCommand.getCmdString(this, key)
  }

  def getCmdString(key: String): Option[FieldCommand[String]] = {
    FieldCommand.getCmdString(this, key)
  }

  def getCmdInt(key: Symbol): Option[FieldCommand[Int]] = {
    FieldCommand.getCmdInt(this, key)
  }

  def getCmdInt(key: String): Option[FieldCommand[Int]] = {
    FieldCommand.getCmdInt(this, key)
  }
}
