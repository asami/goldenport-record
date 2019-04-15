package org.goldenport.record.store

import org.goldenport.hocon.RichConfig
import org.goldenport.record.v3.sql.SqlContext

/*
 * @since   Mar. 24, 2019
 * @version Apr.  6, 2019
 * @author  ASAMI, Tomoharu
 */
class StoreFactory(val config: RichConfig, val sqlContext: SqlContext) {
  lazy val sqlStores: Map[Symbol, SqlStore] = {
    val a = sqlContext.takeDatabaseNames.map(x => x -> new SqlStore(x, sqlContext))
    a.toMap
  }

  def getStore(name: Symbol): Option[Store] = sqlStores.get(name)

  def getCollection(store: Option[Symbol], collection: Symbol): Option[Collection] =
    store.map(getCollection(_, collection)).getOrElse(getCollection(collection))

  def getCollection(collection: Symbol): Option[Collection] = getCollection(SqlContext.KEY_DEFAULT, collection)

  def getCollection(store: Symbol, collection: Symbol): Option[Collection] =
    getStore(store) collect {
      case m: SqlStore => SqlStore.SqlCollection(m, collection)
    }
}


object StoreFactory {
}
