package org.goldenport.record.v2

/*
 * @since   Jan. 15, 2017
 * @version Jan. 16, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Converter {
  def convertIn(p: Any): Any
  def convertOut(p: Any): Any
}

trait ExternalConverter extends Converter {
}
