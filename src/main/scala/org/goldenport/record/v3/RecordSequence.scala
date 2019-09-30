package org.goldenport.record.v3

import org.w3c.dom._
import org.goldenport.record.v3.sql.RecordIterator

/*
 * @since   Mar. 30, 2019
 *  version Apr.  6, 2019
 *  version Jul. 28, 2019
 *  version Aug.  3, 2019
 * @version Sep. 30, 2019
 * @author  ASAMI, Tomoharu
 */
case class RecordSequence(irecords: Vector[IRecord]) extends DocumentFragment with DomNodeImpl {
  lazy val toRecords: Vector[Record] = irecords.map(_.toRecord)

  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.DOCUMENT_FRAGMENT_NODE
  override def getLocalName(): String = "#record-sequence"

  // Members declared in org.goldenport.record.v3.DomNodeImpl
  def domContents: Vector[DomNode] = domElements
  def domAttributes: Vector[AttributeNode] = Vector.empty
  def domElements: Vector[ElementNode] = toRecords
  def domValues: Vector[ValueNode] = Vector.empty
}

object RecordSequence {
  val empty = RecordSequence(Vector.empty)

  def apply(ps: Iterator[IRecord]): RecordSequence = new RecordSequence(ps.toVector)
  def apply(ps: Iterable[IRecord]): RecordSequence = new RecordSequence(ps.toVector)

  def createClose(iter: RecordIterator): RecordSequence = try {
    RecordSequence(iter.toVector)
  } finally {
    iter.close()
  }
}
