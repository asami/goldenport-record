package org.goldenport.record.v3

import org.w3c.dom._
import org.goldenport.RAISE

/*
 * @since   Jan.  5, 2019
 * @version Jan.  6, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait DomNode extends Node with DomNodeImpl {
  def field: Field
  def parent: IRecord

  lazy val children: Vector[DomNode] = field.value match {
    case EmptyValue => Vector.empty
    case SingleValue(v) => v match {
      case m: IRecord => m.domNodes
      case m => Vector(ValueNode(field, parent))
    }
    case MultipleValue(vs) => RAISE.notImplementedYetDefect
  }

  lazy val domElements: Vector[ElementNode] = children collect {
    case m: ElementNode => m
  }

  lazy val domAttributes: Vector[AttributeNode] = children collect {
    case m: AttributeNode => m
  }

  lazy val domValues: Vector[ValueNode] = children collect {
    case m: ValueNode => m
  }
}

trait DomNodeImpl {
  def domElements: Vector[ElementNode]
  def domAttributes: Vector[AttributeNode]
  def domValues: Vector[ValueNode]

  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Node
  def getAttributes(): NamedNodeMap = RAISE.notImplementedYetDefect(this, "getAttributes")
  def getChildNodes(): NodeList = RAISE.notImplementedYetDefect(this, "getChildNodes")
  def getFeature(p: String, q: String): Object = RAISE.notImplementedYetDefect(s"${p}")

  def getFirstChild(): Node = domElements.headOption.getOrElse(null)
  def getLastChild(): Node = domElements.lastOption.getOrElse(null)

  def getLocalName(): String = RAISE.notImplementedYetDefect(this, "getLocalName")
  def getNamespaceURI(): String = RAISE.notImplementedYetDefect(this, "getNamespaceURI")
  def getNextSibling(): Node = RAISE.notImplementedYetDefect(this, "getNextSibling")
  def getNodeName(): String = RAISE.notImplementedYetDefect(this, "getNodeName")
  def getNodeValue(): String = RAISE.notImplementedYetDefect(this, "getNodeValue")
  def getOwnerDocument(): Document = RAISE.notImplementedYetDefect(this, "getOwnerDocument")
  def getParentNode(): Node = RAISE.notImplementedYetDefect(this, "getParentNode")
  def getPrefix(): String = RAISE.notImplementedYetDefect(this, "getPrefix")
  def getPreviousSibling(): Node = RAISE.notImplementedYetDefect(this, "getPreviousSibling")
  def getTextContent(): String = RAISE.notImplementedYetDefect(this, "getTextContent")
  def getUserData(p: String): Object = RAISE.notImplementedYetDefect(s"${p}")
  def hasAttributes(): Boolean = RAISE.notImplementedYetDefect(this, "getUserData")
  def hasChildNodes(): Boolean = RAISE.notImplementedYetDefect(this, "getUserData")

  def isDefaultNamespace(p: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def isEqualNode(p: Node): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def isSameNode(p: Node): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def isSupported(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def lookupNamespaceURI(p: String): String = RAISE.notImplementedYetDefect(s"${p}")
  def lookupPrefix(p: String): String = RAISE.notImplementedYetDefect(s"${p}")
  def getBaseURI(): String = RAISE.notImplementedYetDefect(this, "getBaseURI")
  def normalize(): Unit = RAISE.notImplementedYetDefect(this, "getBaseURI")
  def cloneNode(p: Boolean): Node = RAISE.notImplementedYetDefect(s"${p}")
  def compareDocumentPosition(p: Node): Short = RAISE.notImplementedYetDefect(s"${p}")

  /*
   * Mutator
   */
  // Members declared in org.w3c.dom.Node
  def appendChild(p: Node): Node = RAISE.unsupportedOperationFault(s"${p}")
  def insertBefore(p: Node, q: Node): Node = RAISE.unsupportedOperationFault(s"${p}")

  def removeChild(p: Node): Node = RAISE.unsupportedOperationFault(s"${p}")
  def replaceChild(p: Node, q: Node): Node = RAISE.unsupportedOperationFault(s"${p}")
  def setNodeValue(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def setPrefix(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def setTextContent(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def setUserData(p: String, q: Any, r: UserDataHandler): Object = RAISE.unsupportedOperationFault(s"${p}")
}

case class AttributeNode(
  field: Field,
  parent: IRecord
) extends DomNode with Attr {
  def name = field.name
  /*
   * Accessor
   */
  // Node
  def getNodeType(): Short = Node.ATTRIBUTE_NODE
  // Attr
  def getName(): String = field.name
  def getOwnerElement(): Element = RAISE.notImplementedYetDefect
  def getSchemaTypeInfo(): TypeInfo = RAISE.notImplementedYetDefect
  def getSpecified(): Boolean = RAISE.notImplementedYetDefect
  def getValue(): String = field.value.asString
  def isId(): Boolean = field.name.equalsIgnoreCase("id")

  /*
   * Mutator
   */
  // Attr
  def setValue(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
}

case class ElementNode(
  field: Field,
  parent: IRecord
) extends DomNode with Element with ElementNodeImpl {
  def name = field.name

  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Element
  def getTagName(): String = field.name
}

trait ElementNodeImpl {
  def domElements: Vector[ElementNode]
  def domAttributes: Vector[AttributeNode]
  def domValues: Vector[ValueNode]

  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.ELEMENT_NODE
  // Members declared in org.w3c.dom.Element
  def getSchemaTypeInfo(): TypeInfo = RAISE.notImplementedYetDefect
  def hasAttribute(name: String): Boolean = domAttributes.exists(_.name == name)
  def hasAttributeNS(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def getAttribute(name: String): String = Option(getAttributeNode(name)).map(_.getValue).getOrElse(null)
  def getAttributeNS(p: String, q: String): String = RAISE.notImplementedYetDefect(s"${p}")
  def getAttributeNode(name: String): Attr = domAttributes.find(_.name == name).getOrElse(null)
  def getAttributeNodeNS(p: String, q: String): Attr = RAISE.notImplementedYetDefect(s"${p}")
  def getElementsByTagName(name: String): NodeList = DomNodeList(domElements.filter(_.name == name))
  def getElementsByTagNameNS(p: String, q: String): NodeList = RAISE.notImplementedYetDefect(s"${p}")

  /*
   * Mutator
   */
  // Members declared in org.w3c.dom.Element
   def removeAttribute(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
   def removeAttributeNS(p: String, q: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
   def removeAttributeNode(p: Attr): Attr = RAISE.unsupportedOperationFault(s"${p}")
   def setAttribute(p: String, q: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
   def setAttributeNS(p: String, q: String, r: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
   def setAttributeNode(p: Attr): Attr = RAISE.unsupportedOperationFault(s"${p}")
   def setAttributeNodeNS(p: Attr): Attr = RAISE.unsupportedOperationFault(s"${p}")
   def setIdAttribute(p: String, q: Boolean): Unit = RAISE.unsupportedOperationFault(s"${p}")
   def setIdAttributeNS(p: String, q: String, r: Boolean): Unit = RAISE.unsupportedOperationFault(s"${p}")
   def setIdAttributeNode(p: Attr, q: Boolean): Unit = RAISE.unsupportedOperationFault(s"${p}")
}

case class ValueNode(
  field: Field,
  parent: IRecord
) extends DomNode with Text {
  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.TEXT_NODE
  // Members declared in org.w3c.dom.CharacterData
  def getData(): String = field.asString
  def getLength(): Int = field.asString.length
  def substringData(p: Int, q: Int): String = field.asString.substring(p, q)

  // Members declared in org.w3c.dom.Text
  def getWholeText(): String = getData()
  def isElementContentWhitespace(): Boolean = false

  /*
   * Mutator
   */
  // Members declared in org.w3c.dom.CharacterData
  def setData(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def appendData(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def insertData(p: Int, q: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def replaceData(p: Int, q: Int, r: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
  def deleteData(p: Int, q: Int): Unit = RAISE.unsupportedOperationFault(s"${p}")

  // Members declared in org.w3c.dom.Text
  def replaceWholeText(p: String): Text = RAISE.unsupportedOperationFault(s"${p}")
  def splitText(p: Int): Text = RAISE.unsupportedOperationFault(s"${p}")
}

case class DomNodeList(nodes: Vector[DomNode]) extends NodeList {
  def getLength(): Int = nodes.length
  def item(i: Int): Node = nodes(i)
}
