package org.goldenport.record.v2.projector

import org.goldenport.exception.RAISE
import org.goldenport.record.v2._

/*
 * @since   Jul. 21, 2018
 * @version Jul. 28, 2018
 * @author  ASAMI, Tomoharu
 */
case class ProjectorContext(
  column: Option[Column],
  opaque: Option[Any]
) {
  def attributeName: String = column.map(_.name).getOrElse(RAISE.noReachDefect)

  def withColumn(p: Column) = copy(column = Some(p))
}

object ProjectorContext {
  val default = ProjectorContext(None, None)

  def apply(column: Column): ProjectorContext = ProjectorContext(Some(column), None)

  def createWithOpaque(column: Column, opaque: Any): ProjectorContext =
    ProjectorContext(Some(column), Some(opaque))
}
