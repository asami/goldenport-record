package org.goldenport.record.v2

import org.goldenport.RAISE
import org.goldenport.record.v3.{IRecord, Record => Record3, Field => Field3}

/*
 * @since   Nov.  7, 2018
 * @version Nov.  7, 2018
 * @author  ASAMI, Tomoharu
 */
case class RecordRecord(record: Record) extends IRecord {
  def toRecord: Record3 = RAISE.notImplementedYetDefect
  def keyNames: List[String] = RAISE.notImplementedYetDefect
  def fields: Seq[Field3] = RAISE.notImplementedYetDefect
  def isEmpty: Boolean = RAISE.notImplementedYetDefect
  def isDefined(key: String): Boolean = record.isDefined(key)
  def isDefined(key: Symbol): Boolean = RAISE.notImplementedYetDefect
  def get(key: String): Option[Any] = record.getOne(key)
  def get(key: Symbol): Option[Any] = get(key.name)
  def getList(key: String): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getList(key: Symbol): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getRecord(key: String): Option[Record3] = RAISE.notImplementedYetDefect
  def getRecord(key: Symbol): Option[Record3] = RAISE.notImplementedYetDefect
  def takeRecordList(key: String): List[Record3] = RAISE.notImplementedYetDefect
  def takeRecordList(key: Symbol): List[Record3] = RAISE.notImplementedYetDefect
  def +(rhs: IRecord): IRecord = RAISE.notImplementedYetDefect
}

