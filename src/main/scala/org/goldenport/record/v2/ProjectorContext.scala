package org.goldenport.record.v2.projector

import org.goldenport.exception.RAISE
import org.goldenport.record.v2._

/*
 * @since   Jul. 21, 2018
 * @version Jul. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class ProjectorContext(
  column: Option[Column]
) {
  def attributeName: String = column.map(_.name).getOrElse(RAISE.noReachDefect)
}

object ProjectorContext {
  val default = ProjectorContext(None)

  def apply(column: Column): ProjectorContext = ProjectorContext(Some(column))
}
