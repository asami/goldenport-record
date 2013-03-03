package org.goldenport.record.v2

/*
 * Derived from Credential
 * 
 * @snice   Jan.  6, 2013
 * @version Mar.  3, 2013
 * @author  asami
 */
sealed trait Principal {
  def asString: String
}

case class StringPrincipal(id: String) extends Principal {
  def asString = id
}
