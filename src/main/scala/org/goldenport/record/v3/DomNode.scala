package org.goldenport.record.v3

import org.w3c.dom._
import org.goldenport.RAISE
import org.goldenport.collection.VectorMap
import org.goldenport.xml.dom.ImmutableNodeImpl

/*
 * @since   Jan.  5, 2019
 *  version Jul. 14, 2019
 * @version Aug. 23, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait DomNode extends Node with DomNodeImpl {
  def field: Field
  def parent: IRecord

  lazy val meta = field.meta.column.map(_.xml)
  lazy val prefix: Option[String] = meta.flatMap(_.prefix)
  lazy val namespaceUri: Option[String] = meta.flatMap(_.namespaceUri)
  private lazy val _local_name: String = meta.flatMap(_.name) getOrElse field.name
  lazy val name: String = prefix.map(x => s"$x:${_local_name}").getOrElse(_local_name)

  def getLocalName(): String = _local_name

  lazy val children: Vector[DomNode] = field.value match {
    case EmptyValue => Vector.empty
    case SingleValue(v) => v match {
      case m: IRecord => m.domNodes
      case m => Vector(ValueNode(field, parent))
    }
    case MultipleValue(vs) => RAISE.notImplementedYetDefect
  }

  lazy val domContents: Vector[DomNode] = children collect {
    case m: ElementNode => m
    case m: ValueNode => m
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

trait DomNodeImpl extends ImmutableNodeImpl { self: Node =>
  def domContents: Vector[DomNode]
  def domElements: Vector[ElementNode]
  def domAttributes: Vector[AttributeNode]
  def domValues: Vector[ValueNode]

  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Node
  def getFeature(p: String, q: String): Object = RAISE.notImplementedYetDefect(s"${p}")
  def getNamespaceURI(): String = RAISE.notImplementedYetDefect(this, "getNamespaceURI")
  // def getLocalName(): String = RAISE.notImplementedYetDefect(this, "getLocalName")
  def hasAttribute(name: String): Boolean = domAttributes.exists(_.name == name)
  def hasAttributeNS(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def getAttribute(name: String): String = Option(getAttributeNode(name)).map(_.getValue).getOrElse(null)
  def getAttributeNS(p: String, q: String): String = RAISE.notImplementedYetDefect(s"${p}")
  def getAttributes(): NamedNodeMap = DomNamedNodeMap(domAttributes)
  def getChildNodes(): NodeList = DomNodeList(domContents)
  def getFirstChild(): Node = domElements.headOption.getOrElse(null)
  def getLastChild(): Node = domElements.lastOption.getOrElse(null)
  def getNextSibling(): Node = RAISE.notImplementedYetDefect(this, "getNextSibling")
  def getNodeName(): String = RAISE.notImplementedYetDefect(this, s"getNodeName(${getClass.getSimpleName})")
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
//  def normalize(): Unit = RAISE.notImplementedYetDefect(this, "getBaseURI")
//  def cloneNode(p: Boolean): Node = RAISE.notImplementedYetDefect(s"${p}")
  def compareDocumentPosition(p: Node): Short = RAISE.notImplementedYetDefect(s"${p}")
  // Members declared in org.w3c.dom.Element
  def getAttributeNode(name: String): Attr = domAttributes.find(_.name == name).getOrElse(null)
  def getAttributeNodeNS(p: String, q: String): Attr = RAISE.notImplementedYetDefect(s"${p}")

  /*
   * Mutator
   */
  // Members declared in org.w3c.dom.Node
//  def appendChild(p: Node): Node = RAISE.unsupportedOperationFault(s"${p}")
//  def insertBefore(p: Node, q: Node): Node = RAISE.unsupportedOperationFault(s"${p}")

//  def removeChild(p: Node): Node = RAISE.unsupportedOperationFault(s"${p}")
//  def replaceChild(p: Node, q: Node): Node = RAISE.unsupportedOperationFault(s"${p}")
//  def setNodeValue(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
//  def setPrefix(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
//  def setTextContent(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
//  def setUserData(p: String, q: Any, r: UserDataHandler): Object = RAISE.unsupportedOperationFault(s"${p}")
}

case class AttributeNode(
  field: Field,
  parent: IRecord
) extends DomNode with Attr {
  /*
   * Accessor
   */
  // Node
  def getNodeType(): Short = Node.ATTRIBUTE_NODE
  // Attr
  def getSchemaTypeInfo(): TypeInfo = RAISE.notImplementedYetDefect
  def getOwnerElement(): Element = parent
  def getSpecified(): Boolean = true
  def getName(): String = name
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
  // def name = field.name

  // /*
  //  * Accessor
  //  */
  // // Members declared in org.w3c.dom.Element
  def getTagName(): String = name
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
  override lazy val children = Vector.empty

  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.TEXT_NODE
  override def getLocalName(): String = null
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

case class DomNodeKey(ns: Option[String], name: String)
object DomNodeKey {
  def apply(name: String): DomNodeKey = DomNodeKey(None, name)
  def apply(ns: String, name: String): DomNodeKey = DomNodeKey(Some(ns), name)
}

case class DomNamedNodeMap(m: VectorMap[DomNodeKey, Node]) extends NamedNodeMap {
  def getLength(): Int = m.size
  def getNamedItem(name: String): Node = m.get(DomNodeKey(name)) getOrElse null
  def getNamedItemNS(ns: String, name: String): Node = m.get(DomNodeKey(ns, name)) getOrElse null
  def item(i: Int): Node = m.valueVector.lift(i) getOrElse null
  def removeNamedItem(name: String): Node = RAISE.unsupportedOperationFault
  def removeNamedItemNS(name: String, ns: String): Node = RAISE.unsupportedOperationFault
  def setNamedItem(node: Node): Node = RAISE.unsupportedOperationFault
  def setNamedItemNS(node: Node): Node = RAISE.unsupportedOperationFault
}
object DomNamedNodeMap {
  def apply(ps: Vector[AttributeNode]): DomNamedNodeMap =
    DomNamedNodeMap(VectorMap(ps.map(x => DomNodeKey(x.name) -> x)))
}
