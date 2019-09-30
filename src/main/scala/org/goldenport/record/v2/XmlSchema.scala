package org.goldenport.record.v2

import scalaz._, Scalaz._
import java.net.URI

/*
 * @since   Aug. 21, 2019
 * @version Sep. 30, 2019
 * @author  ASAMI, Tomoharu
 */
case class XmlSchema(
  namespaceUri: Option[String] = None,
  prefix: Option[String] = None,
  localName: Option[String] = None
) {
  lazy val tagName: Option[String] = localName.map(x =>
    prefix.map(p => s"$p:$x").getOrElse(x)
  )
}

object XmlSchema {
  val default = XmlSchema()

  def apply(name: String): XmlSchema = XmlSchema(None, None, Some(name))
}
