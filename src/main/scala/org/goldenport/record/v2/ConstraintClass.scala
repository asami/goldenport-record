package org.goldenport.record.v2

import play.api.libs.json._
import org.goldenport.RAISE

/*
 * @since   Apr. 28, 2019
 * @version Apr. 28, 2019
 * @author  ASAMI, Tomoharu
 */
trait ConstraintClass {
  def fromJson(p: JsValue): Constraint = RAISE.notImplementedYetDefect
  def fromJsonOption(p: JsValue): Option[Constraint] = None // TODO
}

