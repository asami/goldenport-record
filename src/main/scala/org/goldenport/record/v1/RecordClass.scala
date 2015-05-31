package org.goldenport.record.v1

import org.goldenport.util.QSymbol

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Aug. 29, 2010
 *  version Oct.  5, 2010
 *  version Feb. 14, 2012
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
class RecordClass(val uri: QSymbol, val schema: RecordSchema, val kindMap: Map[String, String] = Map.empty) {
  def name = uri.name
  def path = uri.path

  def getKind: String = getKind("")

  def getKind(key: String) = kindMap.getOrElse(key, name)
}

object RecordClass {
  def apply(uri: Symbol, schema: RecordSchema) = {
    new RecordClass(QSymbol(uri), schema)
  }
}
