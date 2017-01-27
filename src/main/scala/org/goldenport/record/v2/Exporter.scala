package org.goldenport.record.v2

/*
 * @since   Jan. 15, 2017
 * @version Jan. 16, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Exporter {
  def apply(p: Any): Any
}

trait ExternalExporter extends Exporter {
}
