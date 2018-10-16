package org.goldenport.record.v2.projector

import org.goldenport.record.v2._

/*
 * @since   Jul. 19, 2018
 * @version Jul. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait Poker {
  def apply(rec: Record, value: Any): Record
}

object Poker {

}
