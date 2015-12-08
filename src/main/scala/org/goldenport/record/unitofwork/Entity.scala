package org.goldenport.record.unitofwork

import org.goldenport.record.v2.Record

/*
 * @since   Dec.  3, 2015
 * @version Dec.  3, 2015
 * @author  ASAMI, Tomoharu
 */
trait Entity {
}

object Entity {
  case class RecordEntity(v: Record) extends Entity {
  }
}
