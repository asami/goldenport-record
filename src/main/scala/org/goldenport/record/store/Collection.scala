package org.goldenport.record.store

import org.goldenport.record.v3._

/*
 * @since   Mar. 30, 2019
 * @version Mar. 31, 2019
 * @author  ASAMI, Tomoharu
 */
trait Collection {
  def fetch(id: Id): Option[Record]
  def query(q: Query): RecordSequence
  def insert(rec: Record): Id
  def update(id: Id, rec: Record): Unit
  def delete(id: Id): Unit
}
