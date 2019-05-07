package org.goldenport.record.v2

import org.goldenport.RAISE
import org.goldenport.record.v3.{IRecord, Record => Record3, Field => Field3}
import org.goldenport.record.v3.{FieldValue, EmptyValue, SingleValue, MultipleValue}

/*
 * @since   Nov.  7, 2018
 *  version Dec. 11, 2018
 *  version Jan. 21, 2019
 * @version Apr. 20, 2019
 * @author  ASAMI, Tomoharu
 */
case class RecordRecord(record: Record) extends IRecord {
  import RecordRecord._

  def toRecord: Record3 = toRecord3(record)
  def keyNames: List[String] = RAISE.notImplementedYetDefect
  def print: String = RAISE.notImplementedYetDefect
  def show: String = RAISE.notImplementedYetDefect
  def fields: Seq[Field3] = record.fields.map(toField3)
  def isEmpty: Boolean = record.isEmpty
  def isDefined(key: String): Boolean = record.isDefined(key)
  def isDefined(key: Symbol): Boolean = record.isDefined(key)
  def get(key: String): Option[Any] = record.getOne(key)
  def get(key: Symbol): Option[Any] = get(key.name)
  def getList(key: String): Option[List[Any]] = getList(Symbol(key))
  def getList(key: Symbol): Option[List[Any]] = record.getList(key) match {
    case Nil => None
    case x :: Nil => x match {
      case m: List[_] => Some(m)
      case _=> Some(List(x))
    }
    case xs => Some(xs)
  }
  def getRecord(key: String): Option[Record3] = record.getRecord(key).map(toRecord3)
  def getRecord(key: Symbol): Option[Record3] = record.getRecord(key).map(toRecord3)
  def takeRecordList(key: String): List[Record3] = record.getRecordList(key).map(toRecord3)
  def takeRecordList(key: Symbol): List[Record3] = record.getRecordList(key).map(toRecord3)
  def update(rhs: IRecord): IRecord = toRecord update rhs
  def complement(rhs: IRecord): IRecord = toRecord complement rhs
}

object RecordRecord {
  def toRecord3(p: Record): Record3 = Record3(p.fields.map(toField3))
  def toField3(p: Field): Field3 = Field3(p.key, toFieldValue(p.values))
  def toFieldValue(ps: List[Any]): FieldValue = ps match {
    case Nil => EmptyValue
    case x :: Nil => x match {
      case m: List[_] => MultipleValue(m.map(_v2_to_v3))
      case _=> SingleValue(_v2_to_v3(x))
    }
    case xs => MultipleValue(xs.map(_v2_to_v3))
  }

  private def _v2_to_v3(p: Any): Any = p match {
    case m: Record => toRecord3(m)
    case m => m
  }
}
