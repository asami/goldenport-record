package org.goldenport.record.chart

/*
 * @since   Sep. 19, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
case class Space(
  serieses: Vector[Series]
) {
  def +(rhs: Series): Space = Space(serieses :+ rhs)
}

object Space {
  val empty = Space(Vector.empty)

  def apply(p: Series): Space = Space(Vector(p))
}
