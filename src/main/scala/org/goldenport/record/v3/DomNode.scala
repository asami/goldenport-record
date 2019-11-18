package org.goldenport.record.v3

import org.w3c.dom._
import org.goldenport.RAISE
import org.goldenport.collection.VectorMap
import org.goldenport.xml.dom.ImmutableNodeImpl
import org.goldenport.value._

/*
 * @since   Jan.  5, 2019
 *  version Jul. 14, 2019
 *  version Aug. 23, 2019
 * @version Sep. 30, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait DomNode extends Node with DomNodeImpl {
  def prefix: Option[String]
  def namespaceUri: Option[String]
  def localName: String
  def qualifiedName: String = prefix.map(x => s"$x:${localName}").getOrElse(localName)

  override def getNamespaceURI(): String = namespaceUri getOrElse null
  override def getNodeName(): String = qualifiedName
  override def getLocalName(): String = localName
}
object DomNode {
  sealed trait Policy extends NamedValueInstance
  object Policy extends EnumerationClass[Policy] {
    val elements = Vector(ElementPolicy, AttributePolicy, XsltPolicy)
  }
  case object ElementPolicy extends Policy {
    val name = "element"
  }
  case object AttributePolicy extends Policy {
    val name = "attribute"
  }
  case object XsltPolicy extends Policy {
    val name = "xslt"
  }
}


trait RecordDomNode extends DomNode {
}

sealed trait FieldDomNode extends DomNode {
  def field: Field
  def parent: IRecord

  lazy val meta = field.meta.column.map(_.xml)
  lazy val prefix: Option[String] = meta.flatMap(_.prefix)
  lazy val namespaceUri: Option[String] = meta.flatMap(_.namespaceUri)
  lazy val localName: String = meta.flatMap(_.name) getOrElse field.name

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

  // Node
  override def getNodeValue(): String = field.value.asString

  override def getNextSibling(): Node = {
    val cs = parent.domContents
    if (cs.length < 2) {
      null
    } else {
      for (i <- 0 until parent.domContents.length - 1) {
        cs(i) match {
          case m: FieldDomNode if m.field == field => return cs(i + 1)
          case _ => // do nothing
        }
      }
      null
    }
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
  def getLocalName(): String = RAISE.notImplementedYetDefect(this, "getLocalName")
  def hasAttribute(name: String): Boolean = domAttributes.exists(_.qualifiedName == name)
  def hasAttributeNS(p: String, q: String): Boolean = RAISE.notImplementedYetDefect(s"${p}")
  def getAttribute(name: String): String = Option(getAttributeNode(name)).map(_.getValue).getOrElse(null)
  def getAttributeNS(p: String, q: String): String = RAISE.notImplementedYetDefect(s"${p}")
  def getAttributes(): NamedNodeMap = DomNamedNodeMap(domAttributes)
  def getChildNodes(): NodeList = DomNodeList(domContents)
  def getFirstChild(): Node = domContents.headOption.getOrElse(null)
  def getLastChild(): Node = domContents.lastOption.getOrElse(null)
  def getNextSibling(): Node = RAISE.notImplementedYetDefect(this, "getNextSibling")
  def getNodeName(): String = RAISE.notImplementedYetDefect(this, s"getNodeName(${getClass.getSimpleName})")
  def getNodeValue(): String = RAISE.notImplementedYetDefect(this, "getNodeValue")
  def getOwnerDocument(): Document = DocumentNode.default
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
  // Members declared in org.w3c.dom.Elementx
  def getAttributeNode(name: String): Attr = domAttributes.find(_.qualifiedName == name).getOrElse(null)
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
) extends FieldDomNode with Attr {
  /*
   * Accessor
   */
  // Node
  def getNodeType(): Short = Node.ATTRIBUTE_NODE
  // Attr
  def getSchemaTypeInfo(): TypeInfo = RAISE.notImplementedYetDefect
  def getOwnerElement(): Element = parent
  def getSpecified(): Boolean = true
  def getName(): String = qualifiedName
  def getValue(): String = getNodeValue()
  def isId(): Boolean = field.name.equalsIgnoreCase("id")

  /*
   * Mutator
   */
  // Attr
  def setValue(p: String): Unit = RAISE.unsupportedOperationFault(s"${p}")
}

trait ElementNode extends DomNode with Element with ElementNodeImpl {
  def qualifiedName: String
}

// case class RecordElementNode(
//   record: IRecord
// ) extends ElementNode with RecordDomNode {
// }

case class FieldElementNode(
  field: Field,
  parent: IRecord
) extends ElementNode with FieldDomNode {
  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Element
  def getTagName(): String = qualifiedName
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
  def getElementsByTagName(name: String): NodeList = DomNodeList(domElements.filter(_.qualifiedName == name))
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
) extends FieldDomNode with Text {
  override lazy val children = Vector.empty

  /*
   * Accessor
   */
  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.TEXT_NODE
  override def getLocalName(): String = null
  override def getNextSibling(): Node = null // XXX avoid a bug? of getNextSibling in FieldDomNode.
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

case class DocumentNode() extends DomNode with Document {
  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.DOCUMENT_NODE

  // Members declared in org.goldenport.record.v3.DomNodeImpl
  def domAttributes: Vector[AttributeNode] = RAISE.notImplementedYetDefect
  def domContents: Vector[DomNode] = RAISE.notImplementedYetDefect
  def domElements: Vector[ElementNode] = RAISE.notImplementedYetDefect
  def domValues: Vector[ValueNode] = RAISE.notImplementedYetDefect
  
  // Members declared in org.goldenport.record.v3.DomNode
  def localName: String = RAISE.notImplementedYetDefect
  def namespaceUri: Option[String] = RAISE.notImplementedYetDefect
  def prefix: Option[String] = RAISE.notImplementedYetDefect

  // Members declared in org.w3c.dom.Document
  def adoptNode(x$1: Node): Node = RAISE.notImplementedYetDefect
  def createAttribute(x$1: String): Attr = RAISE.notImplementedYetDefect
  def createAttributeNS(x$1: String,x$2: String): Attr = RAISE.notImplementedYetDefect
  def createCDATASection(x$1: String): CDATASection = RAISE.notImplementedYetDefect
  def createComment(x$1: String): Comment = RAISE.notImplementedYetDefect
  def createDocumentFragment(): DocumentFragment = RAISE.notImplementedYetDefect
  def createElement(x$1: String): Element = RAISE.notImplementedYetDefect
  def createElementNS(x$1: String,x$2: String): Element = RAISE.notImplementedYetDefect
  def createEntityReference(x$1: String): EntityReference = RAISE.notImplementedYetDefect
  def createProcessingInstruction(x$1: String,x$2: String): ProcessingInstruction = RAISE.notImplementedYetDefect
  def createTextNode(x$1: String): Text = RAISE.notImplementedYetDefect
  def getDoctype(): DocumentType = RAISE.notImplementedYetDefect
  def getDocumentElement(): Element = RAISE.notImplementedYetDefect
  def getDocumentURI(): String = RAISE.notImplementedYetDefect
  def getDomConfig(): DOMConfiguration = RAISE.notImplementedYetDefect
  def getElementById(x$1: String): Element = RAISE.notImplementedYetDefect
  def getElementsByTagName(x$1: String): NodeList = RAISE.notImplementedYetDefect
  def getElementsByTagNameNS(x$1: String,x$2: String): NodeList = RAISE.notImplementedYetDefect
  def getImplementation(): DOMImplementation = RAISE.notImplementedYetDefect
  def getInputEncoding(): String = RAISE.notImplementedYetDefect
  def getStrictErrorChecking(): Boolean = RAISE.notImplementedYetDefect
  def getXmlEncoding(): String = RAISE.notImplementedYetDefect
  def getXmlStandalone(): Boolean = RAISE.notImplementedYetDefect
  def getXmlVersion(): String = RAISE.notImplementedYetDefect
  def importNode(x$1: Node,x$2: Boolean): Node = RAISE.notImplementedYetDefect
  def normalizeDocument(): Unit = RAISE.notImplementedYetDefect
  def renameNode(x$1: Node,x$2: String,x$3: String): Node = RAISE.notImplementedYetDefect
  def setDocumentURI(x$1: String): Unit = RAISE.notImplementedYetDefect
  def setStrictErrorChecking(x$1: Boolean): Unit = RAISE.notImplementedYetDefect
  def setXmlStandalone(x$1: Boolean): Unit = RAISE.notImplementedYetDefect
  def setXmlVersion(x$1: String): Unit = RAISE.notImplementedYetDefect
}
object DocumentNode {
  val default = DocumentNode()
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
    DomNamedNodeMap(VectorMap(ps.map(x => DomNodeKey(x.qualifiedName) -> x)))
}
