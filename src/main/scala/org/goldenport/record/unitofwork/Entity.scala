package org.goldenport.record.unitofwork

import org.goldenport.record.v2.Record

/*
 * @since   Dec.  3, 2015
 *  version Jan. 27, 2018
 *  version Mar. 28, 2018
 * @version Apr.  7, 2018
 * @author  ASAMI, Tomoharu
 */
trait Entity {
  def record: Record
  def getUnderlying: Option[AnyRef]
  def getAs[T]: Option[T] = getUnderlying.asInstanceOf[Option[T]]
  def as[T]: T = getAs.getOrElse {
    throw new IllegalStateException("no underlying entity")
  }
}

object Entity {
  case class RecordEntity(
    record: Record,
    getUnderlying: Option[AnyRef] = None
  ) extends Entity {
  }

  def apply(p: Record): Entity = RecordEntity(p, None)
  def apply(p: Record, underlying: AnyRef): Entity = RecordEntity(p, Some(underlying))
}
