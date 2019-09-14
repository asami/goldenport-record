package org.goldenport.record.v2.util

import spire.math.Interval
import spire.math.interval._
import org.goldenport.record.v2._

/*
 * @since   Sep. 14, 2019
 * @version Sep. 14, 2019
 * @author  ASAMI, Tomoharu
 */
object SpireUtils {
  def toRecord[T](p: Interval[T]): Record = {
    Record.dataApp(
      "lower" -> toRecord(p.lowerBound),
      "higher" -> toRecord(p.upperBound),
      "text" -> p.toString
    )
  }

  def toRecord[T](p: Bound[T]): Record = p match {
    case Closed(v) => Record.dataApp("type" -> "closed", "value" -> v)
    case Open(v) => Record.dataApp("type" -> "open", "value" -> v)
    case Unbound() => Record.dataApp("type" -> "unbound")
    case EmptyBound() => Record.dataApp("type" -> "emptybound")
  }
}
