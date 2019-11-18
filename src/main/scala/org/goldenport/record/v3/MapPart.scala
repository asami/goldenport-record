package org.goldenport.record.v3

import org.goldenport.exception.RAISE

/*
 * @since   Sep. 23, 2019
 * @version Sep. 23, 2019
 * @author  ASAMI, Tomoharu
 */
trait MapPart extends Map[String, Any] { self: Record =>
  def +[W >: Any](kv: (String, W)): Map[String, W] = update(Symbol(kv._1) -> kv._2)
  def -(key: String): Map[String, Any] = removeField(key)
  def iterator: Iterator[(String, Any)] = fields.iterator.flatMap(_.nameValue)
}

object MapPart {
}
