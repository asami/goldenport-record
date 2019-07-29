package org.goldenport.record.store

import org.goldenport.record.v3._

/*
 * @since   Mar. 30, 2019
 *  version Apr.  7, 2019
 * @version Jul. 15, 2019
 * @author  ASAMI, Tomoharu
 */
trait Collection {
  def name: Symbol
  def get(id: Id): Option[Record]
  def query(q: Query): RecordSequence
  def insert(rec: IRecord): Id
  def inserts(rs: Seq[IRecord]): Seq[Id] = rs.map(insert)
  def update(id: Id, rec: IRecord): Unit
  def delete(id: Id): Unit
  def drop(): Unit
}
