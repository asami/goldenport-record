package org.goldenport.record.v2

import org.goldenport.exception.RAISE

/*
 * @since   Jan. 15, 2017
 * @version Jul. 20, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Converter {
  def apply(p: Any): Any = convertIn(p)
  def convertIn(p: Any): Any
  def convertOut(p: Any): Any = convertIn(p)
}

trait ExternalConverter extends Converter {
}

package object converter {
  case object YYYYMMDDConverter extends Converter {
    def convertIn(p: Any): Any = RAISE.notImplementedYetDefect
  }
}
