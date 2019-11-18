package org.goldenport.record.chart

import java.net.URL
import org.goldenport.RAISE
import org.goldenport.i18n.{I18NString, I18NContext}
import org.goldenport.record.v3.{ITable, Record}
import org.goldenport.record.v2.Column

/*
 * @since   Sep. 19, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
case class Series(
  name: String,
  label: Option[I18NString],
  elements: Vector[Particle],
  linesVisible: Boolean,
  shapsVisible: Boolean,
  useLabel: Boolean,
  useTooltip: Boolean,
  useUrl: Boolean
) {
  def getLabel(no: Int): Option[String] = elements.lift(no).flatMap(_.label)
  def getTooltip(no: Int): Option[String] = elements.lift(no).flatMap(_.tooltip)
  def getUrl(no: Int): Option[URL] = elements.lift(no).flatMap(_.url)
}

object Series {
  def shape(name: String, elements: Seq[Particle]): Series = Series(name, None, elements.toVector, false, true, true, true, true)
  def shape(name: String, label: Option[I18NString], elements: Seq[Particle]): Series = new Series(name, label, elements.toVector, false, true, true, true, true)
  def line(ps: Seq[Point]): Series = new Series("#line", None, ps.toVector, true, false, false, false, false)

  case class Builder(
    i18n: I18NContext,
    xColumn: Column,
    yColumn: Column,
    titleColumn: Option[Column]
  ) {
    def locale = i18n.locale
    val xLabel = xColumn.label(locale)
    val yLabel = yColumn.label(locale)
    val zlabel = None
    val title: String = titleColumn.map(_.label(locale)).getOrElse("No title")

    def apply(p: ITable): Series = {
      val records = p.toRecordVector
      _make_particle_series(records)
    }

    private def _make_particle_series(ps: Seq[Record]): Series = {
      val name = title
      val label = None
      val elements = _make_particles(ps)
      Series.shape(name, label, elements)
    }

    private def _make_particles(ps: Seq[Record]): Seq[Particle] = {
      val labelkey = titleColumn.get.name // TODO
      val xkey = xColumn.name
      val ykey = yColumn.name
      ps.flatMap(_make_particle(labelkey, xkey, ykey))
    }

    private def _make_particle(labelkey: String, xkey: String, ykey: String)(p: Record) = {
      (p.getString(labelkey), p.getDouble(xkey), p.getDouble(ykey)) match {
        case (Some(label), Some(x), Some(y)) => Some(Point(x, y, label))
        case _ => None
      }
    }

    private def _make_line_series(ps: Seq[Record]): Series = {
      val xs = Vector(_make_point(ps.head), _make_point(ps.last))
      Series.line(xs)
    }

    private def _make_point(p: Record): Point = {
      val xkey = xColumn.name
      val ykey = yColumn.name
        (p.getDouble(xkey), p.getDouble(ykey)) match {
        case (Some(x), Some(y)) => Point(x, y)
        case _ => RAISE.noReachDefect
      }
    }
  }
}
