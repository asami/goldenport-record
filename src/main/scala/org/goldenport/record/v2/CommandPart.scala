package org.goldenport.record.v2

import org.goldenport.record.command._

/*
 * @since   Sep. 17, 2015
 * @version Nov. 16, 2015
 * @author  ASAMI, Tomoharu
 */
trait CommandPart { self: Record =>
  def getCmdString(key: Symbol): Option[FieldCommand[String]] = {
    getCmdString(key, true)
  }

  def getCmdString(key: Symbol, overwritep: Boolean): Option[FieldCommand[String]] = {
    FieldCommand.getCmdString(this, key, overwritep)
  }

  def getCmdString(key: String): Option[FieldCommand[String]] = {
    getCmdString(key, true)
  }

  def getCmdString(key: String, overwritep: Boolean): Option[FieldCommand[String]] = {
    FieldCommand.getCmdString(this, key, overwritep)
  }

  def getCmdInt(key: Symbol): Option[FieldCommand[Int]] = {
    getCmdInt(key, true)
  }

  def getCmdInt(key: Symbol, overwritep: Boolean): Option[FieldCommand[Int]] = {
    FieldCommand.getCmdInt(this, key, overwritep)
  }

  def getCmdInt(key: String): Option[FieldCommand[Int]] = {
    getCmdInt(key, true)
  }

  def getCmdInt(key: String, overwritep: Boolean): Option[FieldCommand[Int]] = {
    FieldCommand.getCmdInt(this, key, overwritep)
  }
}
