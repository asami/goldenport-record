package org.goldenport.record.unitofwork

/*
 * @since   Dec.  3, 2015
 * @version Dec.  3, 2015
 * @author  ASAMI, Tomoharu
 */
trait Query {
}

object Query {
  case class StringQuery(v: String) extends Query {
  }
}
