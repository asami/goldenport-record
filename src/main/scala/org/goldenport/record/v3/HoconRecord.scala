package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.util.HoconUtils.RichConfig

/*
 * @since   Oct. 21, 2018
 * @version Oct. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class HoconRecord(hocon: RichConfig) extends IRecord {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def keyNames: List[String] = RAISE.notImplementedYetDefect
  def fields: Seq[Field] = RAISE.notImplementedYetDefect
  def isEmpty: Boolean = RAISE.notImplementedYetDefect
  def isDefined(key: String): Boolean = hocon.isDefined(key)
  def isDefined(key: Symbol): Boolean = RAISE.notImplementedYetDefect
  def get(key: String): Option[Any] = hocon.getStringOption(key)
  def get(key: Symbol): Option[Any] = get(key.name)
  def getList(key: String): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getList(key: Symbol): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getRecord(key: String): Option[Record] = RAISE.notImplementedYetDefect
  def getRecord(key: Symbol): Option[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: String): List[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: Symbol): List[Record] = RAISE.notImplementedYetDefect
  def +(rhs: IRecord): IRecord = RAISE.notImplementedYetDefect
}
