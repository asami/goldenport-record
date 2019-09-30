package org.goldenport.record.chart

import org.goldenport.i18n.I18NContext
import org.goldenport.record.v3.ITable

/*
 * @since   Mar. 10, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
case class Chart(
  title: Option[String],
  xLabel: Option[String],
  yLabel: Option[String],
  zLabel: Option[String],
  useLegend: Boolean,
  useLabel: Boolean,
  useTooltip: Boolean,
  useUrl: Boolean,
  space: Space
) {
  def titleOrNull = title getOrElse null
  def xLabelOrNull = xLabel getOrElse null
  def yLabelOrNull = yLabel getOrElse null
  def zLabelOrNull = zLabel getOrElse null

  def withTitle(p: String) = copy(title = Some(p))
  def withXLabel(p: String) = copy(xLabel = Some(p))
  def withYLabel(p: String) = copy(yLabel = Some(p))
  def withZLabel(p: String) = copy(zLabel = Some(p))
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
    space: Space = Space.empty
  ) {
    def apply(): Chart = Chart(title, xLabel, yLabel, zLabel, useLegend, useLabel, useTooltip, useUrl, space)

    def apply(p: Space): Chart = Chart(title, xLabel, yLabel, zLabel, useLegend, useLabel, useTooltip, useUrl, p)

    def add(p: ITable) = {
      val schema = p.schema
      val xc = schema.columns(1) // XXX
      val yc = schema.columns(2) // XXX
      val tc = Some(schema.columns(0)) // XXX
      val series = Series.Builder(i18n, xc, yc, tc)(p)
      copy(space = space + series)
    }
  }
}
