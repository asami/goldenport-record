package org.goldenport.record.chart.driver

import org.goldenport.log.Loggable
import org.goldenport.record.chart._

/*
 * @since   Mar. 10, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
trait ChartDriver extends Loggable {
  def draw(p: Chart): ChartWindow
}
