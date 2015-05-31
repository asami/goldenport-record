package org.goldenport.record.v1

/**
 * @since   Sep.  8, 2010
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
package object query {
  def isIdValueList(ids: List[Any]): Boolean = {
    !ids.exists(!isIdValue(_))
  }

  def isIdValue(id: Any): Boolean = {
    id match {
      case n: Number => true
      case s: String => true
      case v: Var => true
      case _ => false
    }
  }
}
