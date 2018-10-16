package org.goldenport.record.v3

import scala.xml.Node
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 23, 2018
 * @version Aug. 23, 2018
 * @author  ASAMI, Tomoharu
 */
trait XmlPart { self: Record =>
  def toXml: Node = RAISE.notImplementedYetDefect
  def toXmlString: String = RAISE.notImplementedYetDefect
}

object XmlPart {
}
