package org.goldenport.record.v3

import org.goldenport.RAISE
import org.w3c.dom._
import org.goldenport.collection.VectorMap

/*
 * @since   Aug. 23, 2018
 *  version Sep. 20, 2018
 *  version Jan.  6, 2019
 *  version Aug. 23, 2019
 *  version Sep. 30, 2019
 * @version Nov. 29, 2019
 * @author  ASAMI, Tomoharu
 */
trait DomPart extends DomNodeImpl with ElementNodeImpl { self: IRecord =>
  import DomPart._

  def domPolicy: DomNode.Policy = DomNode.XsltPolicy // TODO

  lazy val domNodes: Vector[DomNode] = domNodes(domPolicy)

  def domNodes(p: DomNode.Policy): Vector[DomNode] = p match {
    case DomNode.ElementPolicy => dom_nodes_policy_element
    case DomNode.AttributePolicy => dom_nodes_policy_attribute
    case DomNode.XsltPolicy => dom_nodes_policy_xslt
  }

  protected final def dom_nodes_policy_element = fields.toVector.map { x =>
    if (x.isAttributeOption == Some(true))
      AttributeNode(x, this)
    else
      FieldElementNode(x, this) // TODO or RecordElementNode
  }

  protected final def dom_nodes_policy_attribute = fields.toVector.map { x =>
    if (x.isAttribute)
      AttributeNode(x, this)
    else
      FieldElementNode(x, this) // TODO or RecordElementNode
  }

  protected final def dom_nodes_policy_xslt = fields.toVector.flatMap { x =>
    if (x.isAttribute)
      Vector(AttributeNode(x, this), FieldElementNode(x, this))
    else
      Vector(FieldElementNode(x, this)) // TODO or RecordElementNode
  }

  lazy val domContents: Vector[DomNode] = domNodes collect {
    case m: ElementNode => m
    case m: ValueNode => m
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

  lazy val document: RecordDocument = parent.map(_.document).getOrElse(RecordDocument())

  /*
   * Accessor
   */
  // def getSchemaTypeInfo(): TypeInfo = RAISE.notImplementedYetDefect

  override def getParentNode(): Node = document

  override def getTagName(): String = getSchema.flatMap(_.xml.tagName).getOrElse(IRecord.DEFAULT_TAG_NAME)
  override def getLocalName(): String = getSchema.flatMap(_.xml.localName).getOrElse(IRecord.DEFAULT_TAG_NAME)

  // def hasAttribute(p: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def hasAttributeNS(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  // def getAttribute(p: String): String = ???
  // def getAttributeNS(p: String, q: String): String = RAISE.notImplementedYetDefect(s"${p}")
  // def getAttributeNode(p: String): Attr = RAISE.notImplementedYetDefect(s"${p}")
  // def getAttributeNodeNS(p: String, q: String): Attr = RAISE.notImplementedYetDefect(s"${p}")

  // override def getAttributes(): NamedNodeMap = {
  //   val as = fields.filter(_.isAttribute).map(x => 
  //     (NodeKey(None, x.name), RecordAttr(x.key.name, x.asString))
  //   )
  //   RecordNamedNodeMap(VectorMap(as))
  // }

//  override def getAttributes(): NamedNodeMap = DomNamedNodeMap(domAttributes)

  // def getBaseURI(): String = RAISE.notImplementedYetDefect
  // def getFeature(p: String, q: String): Object = RAISE.notImplementedYetDefect(s"${p}")
  // def getNamespaceURI(): String = RAISE.notImplementedYetDefect(this, "getNamespaceURI")

//  override def getChildNodes(): NodeList = DomNodeList(domElements)

  // def getFirstChild(): Node = domElements.headOption.getOrElse(null)

  // def getLastChild(): Node = RAISE.notImplementedYetDefect(this, "getLastChild")

  // def getElementsByTagName(p: String): NodeList = RAISE.notImplementedYetDefect(s"${p}")
  // def getElementsByTagNameNS(p: String, q: String): NodeList = RAISE.notImplementedYetDefect(s"${p}")

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

object DomPart {
  // trait ImmutableNodeImpl extends Node {
  //   def appendChild(node: Node): Node = RAISE.unsupportedOperationFault
  //   def cloneNode(deep: Boolean): Node = RAISE.unsupportedOperationFault
  //   // def compareDocumentPosition(node: Node): Short = RAISE.unsupportedOperationFault
  //   // def getAttributes(): NamedNodeMap = RAISE.unsupportedOperationFault
  //   // def getBaseURI(): String = RAISE.unsupportedOperationFault
  //   // def getChildNodes(): NodeList = RAISE.unsupportedOperationFault
  //   // def getFeature(x$1: String,x$2: String): Object = RAISE.unsupportedOperationFault
  //   // def getFirstChild(): Node = RAISE.unsupportedOperationFault
  //   // def getLastChild(): Node = RAISE.unsupportedOperationFault
  //   // def getLocalName(): String = RAISE.unsupportedOperationFault
  //   // def getNamespaceURI(): String = RAISE.unsupportedOperationFault
  //   // def getNextSibling(): Node = RAISE.unsupportedOperationFault
  //   // def getNodeName(): String = RAISE.unsupportedOperationFault
  //   // def getNodeType(): Short = RAISE.unsupportedOperationFault
  //   // def getNodeValue(): String = RAISE.unsupportedOperationFault
  //   // def getOwnerDocument(): Document = RAISE.unsupportedOperationFault
  //   // def getParentNode(): Node = RAISE.unsupportedOperationFault
  //   // def getPrefix(): String = RAISE.unsupportedOperationFault
  //   // def getPreviousSibling(): Node = RAISE.unsupportedOperationFault
  //   // def getTextContent(): String = RAISE.unsupportedOperationFault
  //   // def getUserData(x$1: String): Object = RAISE.unsupportedOperationFault
  //   // def hasAttributes(): Boolean = RAISE.unsupportedOperationFault
  //   // def hasChildNodes(): Boolean = RAISE.unsupportedOperationFault
  //   // def insertBefore(node: Node,x$2: Node): Node = RAISE.unsupportedOperationFault
  //   // def isDefaultNamespace(x$1: String): Boolean = RAISE.unsupportedOperationFault
  //   // def isEqualNode(node: Node): Boolean = RAISE.unsupportedOperationFault
  //   // def isSameNode(node: Node): Boolean = RAISE.unsupportedOperationFault
  //   // def isSupported(x$1: String,x$2: String): Boolean = RAISE.unsupportedOperationFault
  //   // def lookupNamespaceURI(x$1: String): String = RAISE.unsupportedOperationFault
  //   // def lookupPrefix(x$1: String): String = RAISE.unsupportedOperationFault
  //   def normalize(): Unit = RAISE.unsupportedOperationFault
  //   def removeChild(node: Node): Node = RAISE.unsupportedOperationFault
  //   def replaceChild(node: Node, node2: Node): Node = RAISE.unsupportedOperationFault
  //   def setNodeValue(value: String): Unit = RAISE.unsupportedOperationFault
  //   def setPrefix(prefix: String): Unit = RAISE.unsupportedOperationFault
  //   def setTextContent(text: String): Unit = RAISE.unsupportedOperationFault
  //   def setUserData(key: String, data: Any, handler: UserDataHandler): Object = RAISE.unsupportedOperationFault
  // }

  // unused
  case class NodeKey(ns: Option[String], name: String)
  object NodeKey {
    def apply(name: String): NodeKey = NodeKey(None, name)
    def apply(ns: String, name: String): NodeKey = NodeKey(Some(ns), name)
  }

  // unused
  case class RecordNamedNodeMap(m: VectorMap[NodeKey, Node]) extends NamedNodeMap {
    def getLength(): Int = m.size
    def getNamedItem(name: String): Node = m.get(NodeKey(None, name)) getOrElse null
    def getNamedItemNS(ns: String, name: String): Node = m.get(NodeKey(Some(ns), name)) getOrElse null
    def item(i: Int): Node = m.valueVector.lift(i) getOrElse null
    def removeNamedItem(name: String): Node = RAISE.unsupportedOperationFault
    def removeNamedItemNS(name: String, ns: String): Node = RAISE.unsupportedOperationFault
    def setNamedItem(node: Node): Node = RAISE.unsupportedOperationFault
    def setNamedItemNS(node: Node): Node = RAISE.unsupportedOperationFault
  }

  import org.goldenport.xml.dom.{ImmutableNodeImpl, ImmutableNodeStub}

  // unused
  case class RecordAttr(name: String, value: String) extends Attr with ImmutableNodeImpl {
    // Members declared in org.w3c.dom.Node
    def compareDocumentPosition(node: Node): Short = RAISE.unsupportedOperationFault
    def getAttributes(): NamedNodeMap = RAISE.unsupportedOperationFault
    def getBaseURI(): String = RAISE.unsupportedOperationFault
    def getChildNodes(): NodeList = RAISE.unsupportedOperationFault
    def getFeature(feature: String, version: String): Object = RAISE.unsupportedOperationFault
    def getFirstChild(): Node = RAISE.unsupportedOperationFault
    def getLastChild(): Node = RAISE.unsupportedOperationFault
    def getLocalName(): String = RAISE.unsupportedOperationFault
    def getNamespaceURI(): String = RAISE.unsupportedOperationFault
    def getNextSibling(): Node = RAISE.unsupportedOperationFault
    def getNodeName(): String = RAISE.unsupportedOperationFault
    def getNodeType(): Short = RAISE.unsupportedOperationFault
    def getNodeValue(): String = RAISE.unsupportedOperationFault
    def getOwnerDocument(): Document = RAISE.unsupportedOperationFault
    def getParentNode(): Node = RAISE.unsupportedOperationFault
    def getPrefix(): String = RAISE.unsupportedOperationFault
    def getPreviousSibling(): Node = RAISE.unsupportedOperationFault
    def getTextContent(): String = RAISE.unsupportedOperationFault
    def getUserData(data: String): Object = RAISE.unsupportedOperationFault
    def hasAttributes(): Boolean = RAISE.unsupportedOperationFault
    def hasChildNodes(): Boolean = RAISE.unsupportedOperationFault
    def isDefaultNamespace(uri: String): Boolean = RAISE.unsupportedOperationFault
    def isEqualNode(node: Node): Boolean = RAISE.unsupportedOperationFault
    def isSameNode(node: Node): Boolean = RAISE.unsupportedOperationFault
    def isSupported(feature: String, version: String): Boolean = RAISE.unsupportedOperationFault
    def lookupNamespaceURI(prefix: String): String = RAISE.unsupportedOperationFault
    def lookupPrefix(uri: String): String = RAISE.unsupportedOperationFault
   // Members declared in org.w3c.dom.Attr
    def getName(): String = name
    def getValue(): String = value
    def getOwnerElement(): org.w3c.dom.Element = RAISE.unsupportedOperationFault
    def getSchemaTypeInfo(): org.w3c.dom.TypeInfo = RAISE.unsupportedOperationFault
    def getSpecified(): Boolean = true
    def isId(): Boolean = RAISE.unsupportedOperationFault
    // mutable
    def setValue(value: String): Unit = RAISE.unsupportedOperationFault
  }

  // unused
  case class RecordText(text: String) extends Text with ImmutableNodeImpl {
    // Members declared in org.w3c.dom.Node
    def compareDocumentPosition(node: Node): Short = RAISE.unsupportedOperationFault
    def getAttributes(): NamedNodeMap = RAISE.unsupportedOperationFault
    def getBaseURI(): String = RAISE.unsupportedOperationFault
    def getChildNodes(): NodeList = RAISE.unsupportedOperationFault
    def getFeature(feature: String, version: String): Object = RAISE.unsupportedOperationFault
    def getFirstChild(): Node = RAISE.unsupportedOperationFault
    def getLastChild(): Node = RAISE.unsupportedOperationFault
    def getLocalName(): String = RAISE.unsupportedOperationFault
    def getNamespaceURI(): String = RAISE.unsupportedOperationFault
    def getNextSibling(): Node = RAISE.unsupportedOperationFault
    def getNodeName(): String = RAISE.unsupportedOperationFault
    def getNodeType(): Short = RAISE.unsupportedOperationFault
    def getNodeValue(): String = RAISE.unsupportedOperationFault
    def getOwnerDocument(): Document = RAISE.unsupportedOperationFault
    def getParentNode(): Node = RAISE.unsupportedOperationFault
    def getPrefix(): String = RAISE.unsupportedOperationFault
    def getPreviousSibling(): Node = RAISE.unsupportedOperationFault
    def getTextContent(): String = RAISE.unsupportedOperationFault
    def getUserData(data: String): Object = RAISE.unsupportedOperationFault
    def hasAttributes(): Boolean = RAISE.unsupportedOperationFault
    def hasChildNodes(): Boolean = RAISE.unsupportedOperationFault
    def isDefaultNamespace(uri: String): Boolean = RAISE.unsupportedOperationFault
    def isEqualNode(node: Node): Boolean = RAISE.unsupportedOperationFault
    def isSameNode(node: Node): Boolean = RAISE.unsupportedOperationFault
    def isSupported(feature: String, version: String): Boolean = RAISE.unsupportedOperationFault
    def lookupNamespaceURI(prefix: String): String = RAISE.unsupportedOperationFault
    def lookupPrefix(uri: String): String = RAISE.unsupportedOperationFault
    // Members declared in org.w3c.dom.CharacterData
    def getData(): String = RAISE.unsupportedOperationFault
    def getLength(): Int = RAISE.unsupportedOperationFault
    def substringData(from: Int, to: Int): String = RAISE.unsupportedOperationFault
    // mutable
    def appendData(data: String): Unit = RAISE.unsupportedOperationFault
    def deleteData(from: Int, to: Int): Unit = RAISE.unsupportedOperationFault
    def insertData(pos: Int, data: String): Unit = RAISE.unsupportedOperationFault
    def replaceData(from: Int, to: Int, data: String): Unit = RAISE.unsupportedOperationFault
    def setData(data: String): Unit = RAISE.unsupportedOperationFault
    // Members declared in org.w3c.dom.Text
    def getWholeText(): String = RAISE.unsupportedOperationFault
    def isElementContentWhitespace(): Boolean = RAISE.unsupportedOperationFault
    // mutable
    def replaceWholeText(data: String): Text = RAISE.unsupportedOperationFault
    def splitText(pos: Int): Text = RAISE.unsupportedOperationFault
  }

  case class RecordDocument() extends Document with ImmutableNodeImpl with ImmutableNodeStub {
    def getNodeType(): Short = Node.DOCUMENT_NODE

    // Node
    override def getParentNode() = null

    // Document
    def adoptNode(x$1: Node): Node = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createAttribute(x$1: String): Attr = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createAttributeNS(x$1: String,x$2: String): Attr = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createCDATASection(x$1: String): CDATASection = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createComment(x$1: String): Comment = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createDocumentFragment(): DocumentFragment = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createElement(x$1: String): Element = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createElementNS(x$1: String,x$2: String): Element = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createEntityReference(x$1: String): EntityReference = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createProcessingInstruction(x$1: String,x$2: String): ProcessingInstruction = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def createTextNode(x$1: String): Text = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getDoctype(): DocumentType = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getDocumentElement(): Element = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getDocumentURI(): String = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getDomConfig(): DOMConfiguration = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getElementById(x$1: String): Element = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getElementsByTagName(x$1: String): NodeList = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getElementsByTagNameNS(x$1: String,x$2: String): NodeList = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getImplementation(): DOMImplementation = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getInputEncoding(): String = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getStrictErrorChecking(): Boolean = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getXmlEncoding(): String = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getXmlStandalone(): Boolean = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def getXmlVersion(): String = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def importNode(x$1: Node,x$2: Boolean): Node = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def normalizeDocument(): Unit = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def renameNode(x$1: Node,x$2: String,x$3: String): Node = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def setDocumentURI(x$1: String): Unit = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def setStrictErrorChecking(x$1: Boolean): Unit = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def setXmlStandalone(x$1: Boolean): Unit = RAISE.unsupportedOperationFault(this, "RecordDocument")
    def setXmlVersion(x$1: String): Unit = RAISE.unsupportedOperationFault(this, "RecordDocument")
  }
}
