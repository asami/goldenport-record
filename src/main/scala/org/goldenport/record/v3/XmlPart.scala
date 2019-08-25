package org.goldenport.record.v3

import scala.xml.Node
import org.goldenport.exception.RAISE
import org.goldenport.xml.dom.DomUtils

/*
 * @since   Aug. 23, 2018
 *  version Jul. 14, 2019
 * @version Aug. 23, 2019
 * @author  ASAMI, Tomoharu
 */
trait XmlPart { self: Record =>
  // override def getNodeName() = meta.schema.
  //   flatMap(_.xml.tagName).
  //   getOrElse("record")
  def toXml: Node = RAISE.notImplementedYetDefect
  def toXmlString: String = DomUtils.toHtmlFragmentText(this)
}

object XmlPart {
}
