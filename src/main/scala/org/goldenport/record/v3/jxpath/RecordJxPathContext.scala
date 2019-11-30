package org.goldenport.record.v3.jxpath

import org.apache.commons.jxpath._
import org.apache.commons.jxpath.ri.{JXPathContextReferenceImpl, QName}
import org.apache.commons.jxpath.ri.model.beans.{PropertyOwnerPointer, PropertyPointer}
import org.goldenport.RAISE
import org.goldenport.record.v3._

/*
 * @since   Nov. 29, 2019
 * @version Nov. 30, 2019
 * @author  ASAMI, Tomoharu
 */
class RecordJxPathContext(
  parent: JXPathContext,
  val record: IRecord
) extends JXPathContextReferenceImpl(parent, record) { // , RecordPointer(record)) {
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
  def newContext(bean: IRecord): RecordJxPathContext = new RecordJxPathContext(null, bean)
}

case class RecordPointer(record: IRecord) extends PropertyOwnerPointer(null) {
  // Members declared in org.apache.commons.jxpath.ri.model.NodePointer
  def getBaseValue(): Object = record
  def isCollection(): Boolean = true
  def getLength(): Int = 1
  def isLeaf(): Boolean = false

  // Members declared in org.apache.commons.jxpath.ri.model.beans.PropertyOwnerPointer
  def getPropertyPointer(): PropertyPointer = {
    RAISE.notImplementedYetDefect(this, "getPropertyPointer")
  }

  def getName(): QName = RAISE.notImplementedYetDefect(this, "getPropertyPointer")
}
