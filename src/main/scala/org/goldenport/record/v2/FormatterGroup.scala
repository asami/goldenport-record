package org.goldenport.record.v2

/*
 * @snice   Jul. 25, 2014
 * @version Jul. 25, 2014
 * @author  ASAMI, Tomoharu
 */
case class FormatterGroup(
  formatters: Seq[Formatter]
) {
  def format(column: Column, value: Option[List[Any]]): Option[String] = {
    formatters.toStream.flatMap(_.format(column, value)).headOption
  }
}
