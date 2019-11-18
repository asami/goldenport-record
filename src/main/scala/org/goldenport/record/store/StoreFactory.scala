package org.goldenport.record.store

import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.sql.SqlContext

/*
 * @since   Mar. 24, 2019
 *  version Apr. 15, 2019
 * @version Oct.  5, 2019
 * @author  ASAMI, Tomoharu
 */
class StoreFactory(val config: RichConfig, val sqlContext: SqlContext) {
  lazy val sqlStores: Map[Symbol, SqlStore] = {
    val a = sqlContext.takeDatabaseNames.map(x => x -> new SqlStore(x, sqlContext))
    a.toMap
  }

  def createCollection(store: Option[Symbol], name: Symbol, schema: Schema): Unit =
    store.map(createCollection(_, name, schema)).getOrElse(createCollection(name, schema))

  def createCollection(name: Symbol, schema: Schema): Unit =
    createCollection(SqlContext.KEY_DEFAULT, name, schema)

  def createCollection(store: Symbol, name: Symbol, schema: Schema): Unit =
    getStore(store).map(_.create(name, schema)).getOrElse {
      RAISE.invalidArgumentFault(s"Unavailable store: ${store.name}")
    }

  def getStore(name: Symbol): Option[Store] = sqlStores.get(name)

  def getCollection(store: Option[Symbol], collection: Symbol): Option[Collection] =
    store.map(getCollection(_, collection)).getOrElse(getCollection(collection))

  def getCollection(collection: Symbol): Option[Collection] = getCollection(SqlContext.KEY_DEFAULT, collection)

  def getCollection(store: Symbol, collection: Symbol): Option[Collection] =
    getStore(store).flatMap(_.getCollection(collection))

  def defineCollection(store: Option[Symbol], collection: Symbol, schema: Schema): Collection =
    store.map(defineCollection(_, collection, schema)).getOrElse(defineCollection(collection, schema))

  def defineCollection(collection: Symbol, schema: Schema): Collection = defineCollection(SqlContext.KEY_DEFAULT, collection, schema)

  def defineCollection(store: Symbol, collection: Symbol, schema: Schema): Collection =
    getStore(store).map(_.define(collection, schema)).
      getOrElse(RAISE.invalidArgumentFault(s"Unknown store: ${store.name}"))
}


object StoreFactory {
}
