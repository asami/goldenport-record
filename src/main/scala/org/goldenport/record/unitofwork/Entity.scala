package org.goldenport.record.unitofwork

import org.goldenport.record.v2.Record

/*
 * @since   Dec.  3, 2015
 * @version Jan. 27, 2018
 * @author  ASAMI, Tomoharu
 */
trait Entity {
  def record: Record
}

object Entity {
  case class RecordEntity(record: Record) extends Entity {
  }

  def apply(p: Record): Entity = RecordEntity(p)
}
