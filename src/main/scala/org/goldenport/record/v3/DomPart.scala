package org.goldenport.record.v3

import org.goldenport.RAISE
import org.w3c.dom._

/*
 * @since   Aug. 23, 2018
 *  version Sep. 20, 2018
 * @version Jan.  6, 2019
 * @author  ASAMI, Tomoharu
 */
trait DomPart extends DomNodeImpl with ElementNodeImpl { self: IRecord =>
  lazy val domNodes = fields.toVector.map { x =>
    if (x.isAttribute)
      AttributeNode(x, this)
    else
      ElementNode(x, this)
  }

  lazy val domElements: Vector[ElementNode] = domNodes collect {
    case m: ElementNode => m
  }

  lazy val domAttributes: Vector[AttributeNode] = domNodes collect {
    case m: AttributeNode => m
  }

  lazy val domValues: Vector[ValueNode] = domNodes collect {
    case m: ValueNode => m
  }

  /*
   * Accessor
   */
  // def getAttribute(p: String): String = ???
  // def getAttributeNS(p: String, q: String): String = RAISE.notImplementedYetDefect(s"${p}")
  // def getAttributeNode(p: String): Attr = RAISE.notImplementedYetDefect(s"${p}")
  // def getAttributeNodeNS(p: String, q: String): Attr = RAISE.notImplementedYetDefect(s"${p}")
  // def getElementsByTagName(p: String): NodeList = RAISE.notImplementedYetDefect(s"${p}")
  // def getElementsByTagNameNS(p: String, q: String): NodeList = RAISE.notImplementedYetDefect(s"${p}")
  // def getSchemaTypeInfo(): TypeInfo = RAISE.notImplementedYetDefect
  def getTagName(): String = RAISE.notImplementedYetDefect(this, "getTagName")
  // def hasAttribute(p: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def hasAttributeNS(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def getAttributes(): NamedNodeMap = RAISE.notImplementedYetDefect(this, "getAttributes")
  // def getBaseURI(): String = RAISE.notImplementedYetDefect
  // def getChildNodes(): NodeList = RAISE.notImplementedYetDefect(this, "getChildNodes")
  // def getFeature(p: String, q: String): Object = RAISE.notImplementedYetDefect(s"${p}")

  // def getFirstChild(): Node = domElements.headOption.getOrElse(null)

  // def getLastChild(): Node = RAISE.notImplementedYetDefect(this, "getLastChild")
  // def getLocalName(): String = RAISE.notImplementedYetDefect(this, "getLocalName")
  // def getNamespaceURI(): String = RAISE.notImplementedYetDefect(this, "getNamespaceURI")
  // def getNextSibling(): Node = RAISE.notImplementedYetDefect(this, "getNextSibling")
  // def getNodeName(): String = RAISE.notImplementedYetDefect(this, "getNodeName")
  // def getNodeType(): Short = Node.ELEMENT_NODE // Node.DOCUMENT_FRAGMENT_NODE
  // def getNodeValue(): String = RAISE.notImplementedYetDefect(this, "getNodeValue")
  // def getOwnerDocument(): Document = RAISE.notImplementedYetDefect(this, "getOwnerDocument")
  // def getParentNode(): Node = RAISE.notImplementedYetDefect(this, "getParentNode")
  // def getPrefix(): String = RAISE.notImplementedYetDefect(this, "getPrefix")
  // def getPreviousSibling(): Node = RAISE.notImplementedYetDefect(this, "getPreviousSibling")
  // def getTextContent(): String = RAISE.notImplementedYetDefect(this, "getTextContent")
  // def getUserData(p: String): Object = RAISE.notImplementedYetDefect(s"${p}")
  // def hasAttributes(): Boolean = RAISE.notImplementedYetDefect
  // def hasChildNodes(): Boolean = RAISE.notImplementedYetDefect
  // def insertBefore(p: Node, q: Node): Node = RAISE.notImplementedYetDefect(s"${p}")
  // def isDefaultNamespace(p: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def isEqualNode(p: Node): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def isSameNode(p: Node): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def isSupported(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def lookupNamespaceURI(p: String): String = RAISE.notImplementedYetDefect(s"${p}")
  // def lookupPrefix(p: String): String = RAISE.notImplementedYetDefect(s"${p}")
  // def normalize(): Unit = RAISE.notImplementedYetDefect
  // def cloneNode(p: Boolean): Node = RAISE.notImplementedYetDefect(s"${p}")
  // def compareDocumentPosition(p: Node): Short = RAISE.notImplementedYetDefect(s"${p}")

  /*
   * Mutator
   */
  // def removeAttribute(p: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def removeAttributeNS(p: String, q: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def removeAttributeNode(p: Attr): Attr = RAISE.notImplementedYetDefect(s"${p}")
  // def setAttribute(p: String, q: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setAttributeNS(p: String, q: String, r: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setAttributeNode(p: Attr): Attr = RAISE.notImplementedYetDefect(s"${p}")
  // def setAttributeNodeNS(p: Attr): Attr = RAISE.notImplementedYetDefect(s"${p}")
  // def setIdAttribute(p: String, q: Boolean): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setIdAttributeNS(p: String, q: String, r: Boolean): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setIdAttributeNode(p: Attr, q: Boolean): Unit = RAISE.notImplementedYetDefect(s"${p}")
  
  // Members declared in Node
  // def appendChild(p: Node): Node = RAISE.notImplementedYetDefect(s"${p}")

  // def removeChild(p: Node): Node = RAISE.notImplementedYetDefect(s"${p}")
  // def replaceChild(p: Node, q: Node): Node = RAISE.notImplementedYetDefect(s"${p}")
  // def setNodeValue(p: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setPrefix(p: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setTextContent(p: String): Unit = RAISE.notImplementedYetDefect(s"${p}")
  // def setUserData(p: String, q: Any, r: UserDataHandler): Object = RAISE.notImplementedYetDefect(s"${p}")
}
