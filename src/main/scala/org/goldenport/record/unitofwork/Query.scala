package org.goldenport.record.unitofwork

/*
 * @since   Dec.  3, 2015
 * @version Apr.  3, 2018
 * @author  ASAMI, Tomoharu
 */
trait Query {
}

object Query {
  def apply(v: String): Query = StringQuery(v)

  case class StringQuery(v: String) extends Query {
  }
}
