package org.goldenport.record.store

import org.goldenport.record.v3._

/*
 * @since   Mar. 24, 2019
 * @version Mar. 31, 2019
 * @author  ASAMI, Tomoharu
 */
trait Store {
  def fetch(collection: Symbol, id: Id): Option[Record]
  def query(collection: Symbol, q: Query): RecordSequence
  def insert(collection: Symbol, rec: Record): Id
  def update(collection: Symbol, id: Id, rec: Record): Unit
  def delete(collection: Symbol, id: Id): Unit
}
