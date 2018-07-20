package org.goldenport.record.v2.projector

import org.goldenport.exception.RAISE

/*
 * @since   Jul. 19, 2018
 * @version Jul. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait Converter {
  def apply(p: Any): Any
}

case object NoneConverter extends Converter {
  def apply(p: Any) = p
}

case object YYYYMMDDConverter extends Converter {
  def apply(p: Any) = RAISE.notImplementedYetDefect
}

object Converter {

}
