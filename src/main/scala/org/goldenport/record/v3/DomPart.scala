package org.goldenport.record.v3

/*
 * @since   Aug. 23, 2018
 * @version Sep. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait DomPart { self: IRecord =>
  def getAttribute(x$1: String): String = ???
  def getAttributeNS(x$1: String,x$2: String): String = ???
  def getAttributeNode(x$1: String): org.w3c.dom.Attr = ???
  def getAttributeNodeNS(x$1: String,x$2: String): org.w3c.dom.Attr = ???
  def getElementsByTagName(x$1: String): org.w3c.dom.NodeList = ???
  def getElementsByTagNameNS(x$1: String,x$2: String): org.w3c.dom.NodeList = ???
  def getSchemaTypeInfo(): org.w3c.dom.TypeInfo = ???
  def getTagName(): String = ???
  def hasAttribute(x$1: String): Boolean = ???
  def hasAttributeNS(x$1: String,x$2: String): Boolean = ???
  def removeAttribute(x$1: String): Unit = ???
  def removeAttributeNS(x$1: String,x$2: String): Unit = ???
  def removeAttributeNode(x$1: org.w3c.dom.Attr): org.w3c.dom.Attr = ???
  def setAttribute(x$1: String,x$2: String): Unit = ???
  def setAttributeNS(x$1: String,x$2: String,x$3: String): Unit = ???
  def setAttributeNode(x$1: org.w3c.dom.Attr): org.w3c.dom.Attr = ???
  def setAttributeNodeNS(x$1: org.w3c.dom.Attr): org.w3c.dom.Attr = ???
  def setIdAttribute(x$1: String,x$2: Boolean): Unit = ???
  def setIdAttributeNS(x$1: String,x$2: String,x$3: Boolean): Unit = ???
  def setIdAttributeNode(x$1: org.w3c.dom.Attr,x$2: Boolean): Unit = ???
  
  // Members declared in org.w3c.dom.Node
  def appendChild(x$1: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def cloneNode(x$1: Boolean): org.w3c.dom.Node = ???
  def compareDocumentPosition(x$1: org.w3c.dom.Node): Short = ???
  def getAttributes(): org.w3c.dom.NamedNodeMap = ???
  def getBaseURI(): String = ???
  def getChildNodes(): org.w3c.dom.NodeList = ???
  def getFeature(x$1: String,x$2: String): Object = ???
  def getFirstChild(): org.w3c.dom.Node = ???
  def getLastChild(): org.w3c.dom.Node = ???
  def getLocalName(): String = ???
  def getNamespaceURI(): String = ???
  def getNextSibling(): org.w3c.dom.Node = ???
  def getNodeName(): String = ???
  def getNodeType(): Short = ???
  def getNodeValue(): String = ???
  def getOwnerDocument(): org.w3c.dom.Document = ???
  def getParentNode(): org.w3c.dom.Node = ???
  def getPrefix(): String = ???
  def getPreviousSibling(): org.w3c.dom.Node = ???
  def getTextContent(): String = ???
  def getUserData(x$1: String): Object = ???
  def hasAttributes(): Boolean = ???
  def hasChildNodes(): Boolean = ???
  def insertBefore(x$1: org.w3c.dom.Node,x$2: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def isDefaultNamespace(x$1: String): Boolean = ???
  def isEqualNode(x$1: org.w3c.dom.Node): Boolean = ???
  def isSameNode(x$1: org.w3c.dom.Node): Boolean = ???
  def isSupported(x$1: String,x$2: String): Boolean = ???
  def lookupNamespaceURI(x$1: String): String = ???
  def lookupPrefix(x$1: String): String = ???
  def normalize(): Unit = ???
  def removeChild(x$1: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def replaceChild(x$1: org.w3c.dom.Node,x$2: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def setNodeValue(x$1: String): Unit = ???
  def setPrefix(x$1: String): Unit = ???
  def setTextContent(x$1: String): Unit = ???
  def setUserData(x$1: String,x$2: Any,x$3: org.w3c.dom.UserDataHandler): Object = ???
}
