package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.record.v2.{Schema => Schema2}

/*
 * @since   Oct. 15, 2018
 *  version Jan.  3, 2019
 *  version Apr. 20, 2019
 *  version Jul. 29, 2019
 *  version Aug. 22, 2019
 *  version Sep. 23, 2019
 *  version Oct. 16, 2019
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
case class JsonRecord() extends IRecord {
  def getSchema: Option[Schema2] = None
  def keySymbols: List[Symbol] = RAISE.notImplementedYetDefect
  def keyNames: List[String] = RAISE.notImplementedYetDefect
  def print: String = RAISE.notImplementedYetDefect
  def display: String = RAISE.notImplementedYetDefect
  def show: String = RAISE.notImplementedYetDefect
  def embed: String = display
  def fields: Seq[Field] = RAISE.notImplementedYetDefect
  def get(key: Symbol): Option[Any] = RAISE.notImplementedYetDefect
  def get(key: String): Option[Any] = RAISE.notImplementedYetDefect
  def getList(key: Symbol): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getList(key: String): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getRecord(key: Symbol): Option[Record] = RAISE.notImplementedYetDefect
  def getRecord(key: String): Option[Record] = RAISE.notImplementedYetDefect
  def isDefined(key: Symbol): Boolean = RAISE.notImplementedYetDefect
  def isDefined(key: String): Boolean = RAISE.notImplementedYetDefect
  def isEmpty: Boolean = RAISE.notImplementedYetDefect
  def takeRecordList(key: Symbol): List[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: String): List[Record] = RAISE.notImplementedYetDefect
  def toRecord: Record = RAISE.notImplementedYetDefect
  def update(rhs: IRecord): IRecord = RAISE.notImplementedYetDefect
  def complement(rhs: IRecord): IRecord = RAISE.notImplementedYetDefect
}

object JsonRecord {
  def create(p: String): JsonRecord = RAISE.notImplementedYetDefect
}
