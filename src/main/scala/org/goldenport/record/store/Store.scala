package org.goldenport.record.store

import org.goldenport.record.v2.Schema
import org.goldenport.record.v3._

/*
 * @since   Mar. 24, 2019
 *  version Apr.  7, 2019
 *  version Oct.  5, 2019
 * @version May. 28, 2021
 * @author  ASAMI, Tomoharu
 */
trait Store {
  def get(collection: Symbol, id: Id): Option[Record]
  def select(collection: Symbol, q: Query): RecordSequence
  def insert(collection: Symbol, rec: Record): Id
  def update(collection: Symbol, id: Id, rec: Record): Unit
  def delete(collection: Symbol, id: Id): Unit
  def create(collection: Symbol, schema: Schema): Collection
  def drop(collection: Symbol): Unit
  def define(collection: Symbol, schema: Schema): Collection =
    define(collection, Store.MetaData(schema))
  def define(collection: Symbol, meta: Store.MetaData): Collection
  def getCollection(collection: Symbol): Option[Collection]
}
object Store {
  case class MetaData(
    table: Option[String],
    schema: Schema
  )
  object MetaData {
    def apply(p: Schema): MetaData = MetaData(None, p)
  }
}
