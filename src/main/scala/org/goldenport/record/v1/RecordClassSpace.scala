package org.goldenport.record.v1

import scala.collection.immutable.Stream
import scala.collection.immutable.Stream.Empty
import java.sql.ResultSet
import org.goldenport.util.QSymbol

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Aug. 29, 2010
 *  version Jul.  3, 2011
 *  version Feb. 14, 2012
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
class RecordClassSpace(val classes: List[RecordClass])  {
  def find(uri: QSymbol): Option[RecordClass] = {
    classes.find(_.uri == uri)
  }

  def findSchema(uri: QSymbol): Option[RecordSchema] = {
    find(uri).map(_.schema)
  }

  def findSchema(uri: String): Option[RecordSchema] = findSchema(QSymbol(uri))

  // XXX in case of not existing schema
  def validate(uri: QSymbol, r: Record, ctx: RecordContext): Record = {
    findSchema(uri).map(s => s.validate(r, ctx)).getOrElse(r)
  }

  // XXX in case of not existing schema
  def validate(uri: QSymbol, rs: RecordSet, ctx: RecordContext): RecordSet = {
    findSchema(uri).map(s => new RecordSchemaRecordSet(rs, s, ctx)).getOrElse(rs)
  }
}

object RecordClassSpace {
  def apply(classes: RecordClass*): RecordClassSpace = {
    new RecordClassSpace(classes.toList)
  }
}

object EmptyRecordClassSpace extends RecordClassSpace(Nil)
