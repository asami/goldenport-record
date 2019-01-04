package org.goldenport.record.v3

import org.goldenport.RAISE

/*
 * @since   Oct. 15, 2018
 * @version Jan.  3, 2019
 * @author  ASAMI, Tomoharu
 */
case class DomRecord() extends IRecord {
  def keyNames: List[String] = ???
  def +(rhs: IRecord): IRecord = ???
  def fields: Seq[Field] = ???
  def get(key: Symbol): Option[Any] = ???
  def get(key: String): Option[Any] = ???
  def getList(key: Symbol): Option[List[Any]] = ???
  def getList(key: String): Option[List[Any]] = ???
  def getRecord(key: Symbol): Option[Record] = ???
  def getRecord(key: String): Option[Record] = ???
  def isDefined(key: Symbol): Boolean = ???
  def isDefined(key: String): Boolean = ???
  def isEmpty: Boolean = ???
  def takeRecordList(key: Symbol): List[Record] = ???
  def takeRecordList(key: String): List[Record] = ???
  def toRecord: Record = ???
}

object DomRecord {
  def create(p: String): DomRecord = RAISE.notImplementedYetDefect
}
