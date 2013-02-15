package org.goldenport.record.v2

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jun.  9, 2010
 *  version Jul.  3, 2011
 *  version Nov. 29, 2011
 *  version Feb. 16, 2012
 * @version Jul. 28, 2012
 * @author  ASAMI, Tomoharu
 */
trait Record {
  def fields: Seq[Field]
}

trait Field {
}

object Record {
}

object Field {
}

case class CRecord(fields: Seq[Field]) {
}

case class CField(key: Symbol, values: List[Any]) {
}
