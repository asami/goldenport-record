package org.goldenport.record.chart

import org.goldenport.i18n.I18NString

/*
 * @since   Sep. 19, 2019
 * @version Feb. 27, 2020
 * @author  ASAMI, Tomoharu
 */
case class Space(
  serieses: Vector[Series]
) {
  def +(rhs: Series): Space = Space(serieses :+ rhs)

  def getXLabel: Option[String] = serieses.headOption.flatMap(_.xLabel)
  def getYLabel: Option[String] = serieses.headOption.flatMap(_.yLabel)
  def getZLabel: Option[String] = serieses.headOption.flatMap(_.zLabel)
}

object Space {
  val empty = Space(Vector.empty)

  def apply(p: Series, ps: Series*): Space = Space(p +: ps.toVector)

  def apply(ps: List[Series]): Space = Space(ps.toVector)
}
