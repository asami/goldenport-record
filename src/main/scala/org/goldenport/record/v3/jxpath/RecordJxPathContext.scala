package org.goldenport.record.v3.jxpath

import java.util.Locale
import org.apache.commons.jxpath._
import org.apache.commons.jxpath.ri.{JXPathContextReferenceImpl, QName}
import org.apache.commons.jxpath.ri.model.{NodePointerFactory, NodePointer}
import org.apache.commons.jxpath.ri.model.beans.{PropertyOwnerPointer, PropertyPointer}
import org.apache.commons.jxpath.util.ValueUtils
import org.goldenport.RAISE
import org.goldenport.record.v3._

/*
 * @since   Nov. 29, 2019
 *  version Nov. 30, 2019
 * @version Dec.  4, 2019
 * @author  ASAMI, Tomoharu
 */
class RecordJxPathContext(
  parent: JXPathContext,
  val record: IRecord
) extends JXPathContextReferenceImpl(parent, record, RecordNodePointer.create(record)) {
}

// class RecordJxPathContext0(
//   parent: JXPathContext,
//   val record: IRecord
// ) extends JXPathContext(parent, record) {

//   class RecordIterator() extends java.util.Iterator[AnyRef] {
//     private var _fields = record.fields

//     def hasNext() = _fields.nonEmpty
//     def next() = {
//       val r = _fields.head
//       _fields = _fields.tail
//       RecordPointer(r)
//     }
//   }
// case class RecordPointer(field: Field) extends PointerBase {
//   def compare_To(p: PointerBase): Int = RAISE.notImplementedYetDefect(this, "compare_To")
//   // Members declared in org.apache.commons.jxpath.Pointer
//   def asPath(): String = RAISE.notImplementedYetDefect(this, "asPath")
//   def getNode(): Object = RAISE.notImplementedYetDefect(this, "getNode")
//   def getRootNode(): Object = RAISE.notImplementedYetDefect(this, "getRootNode")
//   def getValue(): Object = RAISE.notImplementedYetDefect(this, "getValue")
//   def setValue(x$1: Any): Unit = RAISE.notImplementedYetDefect(this, "getValue")
// }
//    protected[jxpath] def compilePath(x$1: String): CompiledExpression = RAISE.notImplementedYetDefect(this, "compilePath")
//    def createPath(x$1: String): Pointer = RAISE.notImplementedYetDefect(this, "createPath")
//    def createPathAndSetValue(x$1: String,x$2: Any): Pointer = RAISE.notImplementedYetDefect(this, "createPathAndSetValue")
//    def getContextPointer(): Pointer = RAISE.notImplementedYetDefect(this, "getContextPointer")
//    def getPointer(x$1: String): Pointer = RAISE.notImplementedYetDefect(this, "getPointer")
//    def getRelativeContext(x$1: Pointer): JXPathContext = RAISE.notImplementedYetDefect(this, "getRelativeContext")
//    def getValue(x$1: String,x$2: Class[_]): Object = RAISE.notImplementedYetDefect(this, "getValue")
//    def getValue(x$1: String): Object = RAISE.notImplementedYetDefect(this, "getValue")
//    def iterate(x$1: String): java.util.Iterator[_] = new RecordIterator()
//    def iteratePointers(x$1: String): java.util.Iterator[_] = RAISE.notImplementedYetDefect(this, "iteratePointers")
//    def removeAll(x$1: String): Unit = RAISE.notImplementedYetDefect(this, "removeAll")
//    def removePath(x$1: String): Unit = RAISE.notImplementedYetDefect(this, "removePath")
//    def setValue(x$1: String,x$2: Any): Unit = RAISE.notImplementedYetDefect(this, "setValue")
// }

object RecordJxPathContext {
  JXPathContextReferenceImpl.addNodePointerFactory(RecordNodePointerFactory())

  def newContext(bean: IRecord): RecordJxPathContext = new RecordJxPathContext(null, bean)
}

/*
 * DynamicPointer with RecordDynamicPropertyHandler is not available because IRecord supports DOM interfaces.
 * DOMPointer accepts IRecord as DOM objects before DynamicPointer turn.
 * Therefore use RecordNodePointer before DOMPointer.
 */
class RecordNodePointer(
  parent: NodePointer,
  name: QName,
  val record: IRecord,
  locale: Locale
) extends PropertyOwnerPointer(parent, locale) {
  // Members declared in org.apache.commons.jxpath.ri.model.NodePointer
  def getName(): QName = name

  def getBaseValue(): Object = record

  def isCollection(): Boolean = false

  def getLength(): Int = 1

  def isLeaf(): Boolean = record.isEmpty

  // Members declared in org.apache.commons.jxpath.ri.model.beans.PropertyOwnerPointer
  def getPropertyPointer(): PropertyPointer = {
    new RecordPropertyPointer(this)
  }
}
object RecordNodePointer{
  def create(record: IRecord) = new RecordNodePointer(null, new QName(""), record, null)
  def create(name: QName, record: IRecord, locale: Locale) = new RecordNodePointer(null, name, record, locale)
  def create(parent: NodePointer, name: QName, record: IRecord) = new RecordNodePointer(parent, name, record, null)
}

class RecordPropertyPointer(node: RecordNodePointer) extends PropertyPointer(node) {
  def record = node.record
  def fields = record.fields
  private var _property_name: Option[String] = None
  private var _value: Option[Any] = None

  // Members declared in org.apache.commons.jxpath.ri.model.NodePointer
  def getBaseValue(): Object = record
  def setValue(p: Any): Unit = _value = if (p == null) null else Some(p)

  // Members declared in org.apache.commons.jxpath.ri.model.beans.PropertyPointer
  def getPropertyCount(): Int = fields.length
  def getPropertyName(): String = _property_name.getOrElse(null)
  def getPropertyNames(): Array[String] = record.keyNames.toArray
  protected def isActualProperty(): Boolean = propertyIndex != PropertyPointer.UNSPECIFIED_PROPERTY
  def setPropertyName(p: String): Unit = _property_name = Some(p)

  override def getImmediateNode(): Object =
    if (propertyIndex == PropertyPointer.UNSPECIFIED_PROPERTY)
      getBaseValue()
    else
      fields.lift(propertyIndex).map(_value).getOrElse(null)

  private def _value(p: Field) = p.value.getValue.map(ValueUtils.getValue).getOrElse(null)
}

case class RecordNodePointerFactory() extends NodePointerFactory {
  def getOrder(): Int = 1

  def createNodePointer(name: QName, bean: Any, locale: Locale): NodePointer =
    bean match {
      case m: IRecord => RecordNodePointer.create(name, m, locale)
      case _ => null
    }

   def createNodePointer(parent: NodePointer, name: QName, bean: Object): NodePointer =
    bean match {
      case m: IRecord => RecordNodePointer.create(parent, name, m)
      case _ => null
    }
}
