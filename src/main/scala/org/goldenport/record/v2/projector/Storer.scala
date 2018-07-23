package org.goldenport.record.v2.projector

import org.goldenport.record.v2._

/*
 * @since   Jul. 19, 2018
 * @version Jul. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class Storer(
  poker: Poker,
  converter: Converter
) {
  def apply(sink: Record, value: Any)(implicit ctx: ProjectorContext): Record = poker.apply(sink, converter.apply(value))
}

object Storer {

}
