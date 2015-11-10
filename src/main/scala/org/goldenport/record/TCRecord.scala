package org.goldenport.record

/*
 * @since   May. 31, 2015
 *  version Jun.  1, 2015
 *  version May. 31, 2015
 *  version Jun.  1, 2015
 * @author  ASAMI, Tomoharu
 */
trait TCRecord[T] {
  def toV2(rec: T): v2.Record
  def toV3(rec: T): v3.Record
}
