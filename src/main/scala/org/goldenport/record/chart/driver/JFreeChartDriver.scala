package org.goldenport.record.chart.driver

import java.awt.{Color, Paint, Shape, Stroke}
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.chart.axis.{ValueAxis, NumberAxis}
import org.jfree.data.xy.{Vector => _, _}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.goldenport.RAISE
import org.goldenport.log.Loggable
import org.goldenport.extension.IWindow
import org.goldenport.record.v3.ITable
import org.goldenport.record.chart._

/*
 * XYDataset
 * XYZDataset
 * CategoryDataset
 * 
 * CategoryPlot
 * CompassPlot
 * ContourPlot
 * DialPlot
 * FastScatterPlot
 * MeterPlot
 * MultiplePiePlot
 * PiePlot
 * PolarPlot
 * SpiderWebPlot
 * ThermometerPlot
 * WaferMapPlot
 * XYPlot
 * 
 * @since   Mar. 16, 2019
 *  version Mar. 18, 2019
 *  version Aug.  4, 2019
 *  version Sep. 19, 2019
 * @version Feb. 25, 2020
 * @author  ASAMI, Tomoharu
 */
class JFreeChartDriver() extends ChartDriver {
  val name = "jfreechart"

  def draw(chart: Chart): ChartWindow = {
    val space = chart.space
    val strategy = JFreeChartDriver.Strategy.create(chart)
    implicit val context = JFreeChartDriver.Context.default.withStrategy(strategy)
    val plot = new XYPlot()
    plot.setDomainAxis(strategy.domainAxisOrNull)
    plot.setRangeAxis(strategy.rangeAxisOrNull)
    val col = new XYSeriesCollection()
    val renderer = new XYLineAndShapeRenderer()
    _build_global(renderer, space)
    for ((s, i) <- space.serieses.zipWithIndex) {
      _build_series(col, renderer, i, s)
    }
    plot.setDataset(col)
    plot.setRenderer(renderer)
    val c = new JFreeChart(plot)
    val w = new ChartFrame("JFreeChart", c)
    w.setBounds(10, 10, 500, 500); // FIXME
    w.setVisible(true)
    ChartWindow(JFreeChartDriver.JFreeChartWindow(w))
  }

  private def _build_global(
    renderer: XYLineAndShapeRenderer,
    p: Space
  )(implicit context: JFreeChartDriver.Context) {
    val isurl = false // TODO
    if (isurl) {
      val legendGenerator = new org.jfree.chart.labels.XYSeriesLabelGenerator() {
        override def generateLabel(dataset: org.jfree.data.xy.XYDataset, series: Int): String = {
          p.serieses.lift(series).map(_.name) getOrElse null
        }
      }
      renderer.setLegendItemURLGenerator(legendGenerator)
      val a = new org.jfree.chart.urls.XYURLGenerator() {
        override def generateURL(dataset: org.jfree.data.xy.XYDataset, series: Int, item: Int): String = {
          null // TODO
        }
      }
      renderer.setURLGenerator(a)
    }
  }

  private def _build_series(
    col: XYSeriesCollection,
    renderer: XYLineAndShapeRenderer,
    no: Int,
    p: Series
  )(implicit context: JFreeChartDriver.Context) {
    val istooltip = true // TODO
    val islabel = true // TODO
    val series = new XYSeries(p.name) // TODO label (I18N)
    for (i <- p.elements) {
      series.add(i.x, i.y)
    }
    col.addSeries(series)
    // val dataset = new DefaultXYDataset()
    // dataset.set
    renderer.setSeriesPaint(no, context.paint(no))
    renderer.setSeriesFillPaint(no, context.fillPaint(no))
    renderer.setSeriesOutlinePaint(no, context.outlinePaint(no))
    renderer.setSeriesStroke(no, context.stroke(no))
    if (istooltip) {
      val tooltipGenerator = new org.jfree.chart.labels.XYToolTipGenerator() {
        override def generateToolTip(dataset: org.jfree.data.xy.XYDataset, series: Int, item: Int): String = {
          p.getTooltip(item) getOrElse null
        }
      }
      renderer.setSeriesToolTipGenerator(no, tooltipGenerator)
    }
    if (islabel) {
      val labelGenerator = new org.jfree.chart.labels.XYItemLabelGenerator() {
        override def generateLabel(dataset: org.jfree.data.xy.XYDataset, series: Int, item: Int): String = {
          p.getLabel(item) getOrElse null
        }
      }
      renderer.setSeriesItemLabelGenerator(no, labelGenerator)
      renderer.setSeriesItemLabelsVisible(no, true)
    }
    renderer.setSeriesLinesVisible(no, p.linesVisible)
    renderer.setSeriesShapesVisible(no, p.shapsVisible)
    renderer.setSeriesShape(no, context.shape(no))
    // style match {
    //   case '-' =>
    //     // default line type
    //     renderer.setSeriesLinesVisible(no, lines)
    //     renderer.setSeriesShapesVisible(no, shapes)
    //   case '.' =>
    //     renderer.setSeriesLinesVisible(no, false)
    //     renderer.setSeriesShapesVisible(no, true)
    //     renderer.setSeriesShape(no, Plot.dot)
    //   case '+' =>
    //     renderer.setSeriesLinesVisible(no, false)
    //     renderer.setSeriesShapesVisible(no, true)
    //     renderer.setSeriesShape(no, Plot.plus)
    //   case _ =>
    //     throw new IllegalArgumentException("Expected style to be one of - . or +")
    // }
  }

  def draw(p: ITable): ChartWindow = {
    // val strategy = JFreeChartDriver.Strategy.create(p.chart)
    // implicit val context = JFreeChartDriver.Context.default.withStrategy(strategy)

    // DefaultCategoryDataset data = new DefaultCategoryDataset();
    RAISE.unsupportedOperationFault
  }
}

object JFreeChartDriver {
  case class Strategy(
    orientation: PlotOrientation,
    xLabelOption: Option[String],
    yLabelOption: Option[String],
    zLabelOption: Option[String],
    useLegendOption: Option[Boolean],
    useTooltipOption: Option[Boolean],
    useLabelOption: Option[Boolean],
    useUrlOption: Option[Boolean],
    chart: Option[Chart]
  ) {
    def xLabelOrNull: String = xLabelOption orElse chart.flatMap(_.xLabel) getOrElse null
    def yLabelOrNull: String = yLabelOption orElse chart.flatMap(_.yLabel) getOrElse null.asInstanceOf[String]
    def zLabelOrNull: String = zLabelOption orElse chart.flatMap(_.zLabel) getOrElse null.asInstanceOf[String]
    def domainAxisOrNull: ValueAxis = {
      val r = new NumberAxis(xLabelOrNull)
      r.setAutoRangeIncludesZero(false)
      r
    }
    def rangeAxisOrNull: ValueAxis = {
      val r = new NumberAxis(yLabelOrNull)
      r.setAutoRangeIncludesZero(false)
      r
    }
    def useLegend: Boolean = useLegendOption orElse chart.map(_.useLegend) getOrElse false
    def useTooltip: Boolean = useTooltipOption orElse chart.map(_.useTooltip) getOrElse false
    def useLabel: Boolean = useLabelOption orElse chart.map(_.useLabel) getOrElse false
    def useUrl: Boolean = useUrlOption orElse chart.map(_.useUrl) getOrElse false
  }
  object Strategy {
    val empty = Strategy(PlotOrientation.HORIZONTAL, None, None, None, None, None, None, None, None)
    val default = empty

    def create(p: Option[Chart]): Strategy = p.map(create).getOrElse(default)
    def create(p: Chart): Strategy = empty.copy(chart = Some(p))
  }

  case class Context(
    paintSequence: Vector[Paint],
    outlinePaintSequence: Vector[Paint],
    fillPaintSequence: Vector[Paint],
    strokeSequence: Vector[Stroke],
    outlineStrokeSequence: Vector[Stroke],
    shapeSequence: Vector[Shape],
    strategy: Strategy
  ) {
    def paint(no: Int): Paint = paintSequence.lift(no).getOrElse(paintSequence.last)
    def outlinePaint(no: Int): Paint = outlinePaintSequence.lift(no).getOrElse(outlinePaintSequence.last)

    def fillPaint(no: Int): Paint = fillPaintSequence.lift(no).getOrElse(fillPaintSequence.last)
    def stroke(no: Int): Stroke = strokeSequence.lift(no).getOrElse(strokeSequence.last)
    def outlineStroke(no: Int): Stroke = outlineStrokeSequence.lift(no).getOrElse(outlineStrokeSequence.last)
    def shape(no: Int): Shape = shapeSequence.lift(no).getOrElse(shapeSequence.last)

    def withStrategy(p: Strategy) = copy(strategy = p)
  }
  object Context {
    val default = Context(
      DefaultDrawingSupplier.DEFAULT_PAINT_SEQUENCE.toVector,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE.toVector,
      DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE.toVector,
      DefaultDrawingSupplier.DEFAULT_STROKE_SEQUENCE.toVector,
      DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE.toVector,
      DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE.toVector,
      Strategy.empty
    )
  }

  case class JFreeChartWindow(window: ChartFrame) extends IWindow {
    def close() = RAISE.notImplementedYetDefect
  }
}
