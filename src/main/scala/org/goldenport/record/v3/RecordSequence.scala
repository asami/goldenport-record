package org.goldenport.record.v3

import org.w3c.dom._
import play.api.libs.json._
import org.goldenport.i18n.I18NContext
import org.goldenport.collection.VectorMap
import org.goldenport.record.v2.{Schema => Schema2}
import org.goldenport.record.v3.sql.RecordIterator

/*
 * @since   Mar. 30, 2019
 *  version Apr.  6, 2019
 *  version Jul. 28, 2019
 *  version Aug.  3, 2019
 *  version Sep. 30, 2019
 *  version Oct.  7, 2019
 *  version Mar. 30, 2020
 *  version Mar.  2, 2021
 *  version Jun. 25, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
case class RecordSequence(
  irecords: Vector[IRecord],
  schema: Option[Schema2] = None
) extends DocumentFragment with DomNodeImpl {
  def toRecords: Vector[Record] = irecords.map(_.toRecord)
  def toTable: Table = Table.create(this)
  def toTable(i18nContext: I18NContext, header: Table.HeaderStrategy) = Table.create(i18nContext, header, schema, this)
  def toJson: JsArray = JsArray(irecords.map(_.toRecord.toJson))

  // Members declared in org.w3c.dom.Node
  def getNodeType(): Short = Node.DOCUMENT_FRAGMENT_NODE
  override def getLocalName(): String = "#record-sequence"

  // Members declared in org.goldenport.record.v3.DomNodeImpl
  def domContents: Vector[DomNode] = domElements
  def domAttributes: Vector[AttributeNode] = Vector.empty
  def domElements: Vector[ElementNode] = toRecords
  def domValues: Vector[ValueNode] = Vector.empty

  def map(f: IRecord => IRecord): RecordSequence = RecordSequence(irecords.map(f))
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

  def createClose(schema: Schema2, iter: RecordIterator): RecordSequence = try {
    RecordSequence(iter.toVector, Some(schema))
  } finally {
    iter.close()
  }

  def create(p: VectorMap[String, String]): RecordSequence = create(Vector(p))

  def create(ps: Seq[VectorMap[String, String]]): RecordSequence =
    RecordSequence(ps.map(Record.create))
}
