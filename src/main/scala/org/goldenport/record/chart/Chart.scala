package org.goldenport.record.chart

import org.goldenport.i18n.I18NContext
import org.goldenport.matrix.{INumericalOperations, GoldenportNumericalOperations}
import org.goldenport.record.v3.ITable

/*
 * @since   Mar. 10, 2019
 *  version Sep. 19, 2019
 * @version Feb. 26, 2020
 * @author  ASAMI, Tomoharu
 */
case class Chart(
  title: Option[String],
  _xLabel: Option[String],
  _yLabel: Option[String],
  _zLabel: Option[String],
  useLegend: Boolean,
  useLabel: Boolean,
  useTooltip: Boolean,
  useUrl: Boolean,
  space: Space
) {
  def titleOrNull = title getOrElse null
  def xLabel: Option[String] = _xLabel orElse space.getXLabel
  def yLabel: Option[String] = _yLabel orElse space.getYLabel
  def zLabel: Option[String] = _zLabel orElse space.getZLabel
  def xLabelOrNull = _xLabel orElse space.getXLabel getOrElse null
  def yLabelOrNull = _yLabel orElse space.getYLabel getOrElse null
  def zLabelOrNull = _zLabel orElse space.getZLabel getOrElse null

  def withTitle(p: String) = copy(title = Some(p))
  def withXLabel(p: String) = copy(_xLabel = Some(p))
  def withYLabel(p: String) = copy(_yLabel = Some(p))
  def withZLabel(p: String) = copy(_zLabel = Some(p))
  def withSpace(p: Space) = copy(space = p)
  def withSeries(p: Series) = copy(space = Space(p))
}

object Chart {
  val empty = Chart(None, None, None, None, false, false, false, false, Space.empty)

  case class Builder(
    i18n: I18NContext,
    title: Option[String] = None,
    xLabel: Option[String] = None,
    yLabel: Option[String] = None,
    zLabel: Option[String] = None,
    useLegend: Boolean = true,
    useLabel: Boolean = true,
    useTooltip: Boolean = true,
    useUrl: Boolean = true,
    space: Space = Space.empty,
    numericalOperations: INumericalOperations = GoldenportNumericalOperations
  ) {
    def apply(): Chart = Chart(title, xLabel, yLabel, zLabel, useLegend, useLabel, useTooltip, useUrl, space)

    def apply(p: Space): Chart = Chart(title, xLabel, yLabel, zLabel, useLegend, useLabel, useTooltip, useUrl, p)

    def add(p: ITable) = {
      val schema = p.schema
      val xc = schema.columns(1) // XXX
      val yc = schema.columns(2) // XXX
      val tc = Some(schema.columns(0)) // XXX
      val series = Series.Builder(i18n, numericalOperations, xc, yc, tc)(p)
      copy(space = space + series)
    }
  }
}
