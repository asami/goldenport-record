package org.goldenport.record.chart

import java.net.URL

/*
 * @since   Sep. 19, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
trait Particle {
  def x: Double
  def y: Double
  def z: Double
  def label: Option[String]
  def tooltip: Option[String]
  def url: Option[URL]
}

case class Point(
  x: Double,
  y: Double,
  z: Double,
  label: Option[String],
  tooltip: Option[String],
  url: Option[URL]
) extends Particle
object Point {
  def apply(x: Double, y: Double, label: String): Point =
    Point(x, y, 0, Some(label), None, None)

  def apply(x: Double, y: Double): Point =
    Point(x, y, 0, None, None, None)
}
