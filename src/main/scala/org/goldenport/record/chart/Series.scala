package org.goldenport.record.chart

import java.net.URL
import org.goldenport.RAISE
import org.goldenport.i18n.{I18NString, I18NContext}
import org.goldenport.matrix.INumericalOperations
import org.goldenport.values.EnumRange
import org.goldenport.record.v3.{ITable, Record}
import org.goldenport.record.v2.Column

/*
 * @since   Sep. 19, 2019
 * @version Feb. 27, 2020
 * @author  ASAMI, Tomoharu
 */
case class Series(
  name: String,
  label: Option[String],
  xLabel: Option[String],
  yLabel: Option[String],
  zLabel: Option[String],
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
  def shape(name: String, elements: Seq[Particle]): Series = Series(name, None, None, None, None, elements.toVector, false, true, true, true, true)
  def shape(name: String, xlabel: String, ylabel: String, elements: Seq[Particle]): Series = Series(name, None, Some(xlabel), Some(ylabel), None, elements.toVector, false, true, true, true, true)
  def shape(name: String, label: Option[String], xlabel: String, ylabel: String, elements: Seq[Particle]): Series = new Series(name, label, Some(xlabel), Some(ylabel), None, elements.toVector, false, true, true, true, true)
  def line(xlabel: String, ylabel: String, ps: Seq[Point]): Series = new Series("#line", None, Some(xlabel), Some(ylabel), None, ps.toVector, true, false, false, false, false)
  def line(name: String, xlabel: String, ylabel: String, ps: Seq[Point]): Series = new Series(name, Some(name), Some(xlabel), Some(ylabel), None, ps.toVector, true, false, false, false, false)
  def line(name: String, p0: Point, p1: Point): Series = new Series(name, Some(name), None, None, None, Vector(p0, p1), true, false, false, false, false)

  case class Builder(
    i18n: I18NContext,
    numericalOperations: INumericalOperations,
    xColumn: Column,
    yColumn: Column,
    titleColumn: Option[Column]
  ) {
    def locale = i18n.locale
    val xLabel: String = xColumn.label(locale)
    val yLabel: String = yColumn.label(locale)
    val zlabel = None
    val title: String = titleColumn.map(_.label(locale)).getOrElse("No title")

    def apply(p: ITable): Series = {
      val records = p.toRecordVector
      _make_particle_series(records)
    }

    import org.apache.commons.math3.stat.regression.SimpleRegression

    def simpleRegression(p: ITable): Series = {
      val label = "単回帰" // TODO
      val ix = 0
      val iy = 1
      val range = EnumRange(ix, iy)
      val mx = p.matrix.makeDoubleMatrix.select(range)
      val sr = _sr(mx)
      val columns = mx.column(ix)
      val xmin = columns.min
      val xman = columns.max
      val xgap = (xman - xmin) * 0.1
      val x0 = xmin - xgap
      val x1 = xman + xgap
      val y0 = sr.predict(x0)
      val y1 = sr.predict(x1)
      val p0 = Point(x0, y0)
      val p1 = Point(x1, y1)
      Series.line(label, p0, p1)
    }

    import org.goldenport.matrix.IMatrix

    private def _sr(p: IMatrix[Double]) = {
      val li = 0
      val ri = 1
      val sr = new SimpleRegression(true)
      for (y <- 0 until p.height) {
        sr.addData(p.at(li, y), p.at(ri, y))
      }
      sr
    }

    private def _make_particle_series(ps: Seq[Record]): Series = {
      val name = title
      val label = None
      val elements = _make_particles(ps)
      Series.shape(name, label, xLabel, yLabel, elements)
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

    private def _make_line_series(name: String, ps: Seq[Record]): Series = {
      val xs = Vector(_make_point(ps.head), _make_point(ps.last))
      Series.line(name, xLabel, yLabel, xs)
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
