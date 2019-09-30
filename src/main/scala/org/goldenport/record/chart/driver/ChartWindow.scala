package org.goldenport.record.chart.driver

import org.goldenport.log.Loggable
import org.goldenport.extension.IWindow

/*
 * @since   Sep. 19, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
case class ChartWindow(window: IWindow) {
  def close() = window.close()
}
