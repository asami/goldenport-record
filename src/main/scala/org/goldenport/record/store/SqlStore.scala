package org.goldenport.record.store

import scala.collection.mutable
import org.goldenport.RAISE
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3._
import org.goldenport.record.v3.sql.{SqlContext, SqlBuilder}

/*
 * @since   Apr.  5, 2019
 *  version Apr. 16, 2019
 * @version May.  9, 2019
 * @author  ASAMI, Tomoharu
 */
class SqlStore(
  val name: Symbol,
  val sqlContext: SqlContext
) extends Store {
  private val _collections = new mutable.HashMap[Symbol, Collection]

  def getCollection(collection: Symbol): Option[Collection] =
    _collections.get(collection) orElse _make_collection(collection)

  private def _make_collection(collection: Symbol): Option[Collection] = synchronized {
    _collections.get(collection) orElse {
      if (sqlContext.isExists(collection))
        Some(new SqlStore.SqlCollection(this, collection))
      else
        None
    }
  }

  def takeCollection(collection: Symbol): Collection =
    getCollection(collection).getOrElse(RAISE.noSuchElementFault("Store collection '${collection}'"))

  def get(collection: Symbol, id: Id): Option[Record] =
    takeCollection(collection).get(id)

  def query(collection: Symbol, q: Query): RecordSequence =
    takeCollection(collection).query(q)

  def insert(collection: Symbol, rec: Record): Id =
    takeCollection(collection).insert(rec)

  def update(collection: Symbol, id: Id, rec: Record): Unit =
    takeCollection(collection).update(id, rec)

  def delete(collection: Symbol, id: Id): Unit =
    takeCollection(collection).delete(id)

  def create(collection: Symbol, schema: Schema): Unit = {
    // if (getCollection(collection).isDefined)
    //   RAISE.illegalStateFault(s"Table '${collection.name}' already exits.")
    val builder = SqlBuilder.create(collection.name, schema)
    val sql = builder.createTable()
    sqlContext.execute(name, sql)
  }

  def drop(collection: Symbol): Unit =
    takeCollection(collection).drop()
}

object SqlStore {
  case class SqlCollection(
    store: SqlStore,
    name: Symbol,
    tableName: Option[String] = None
  ) extends Collection {
    protected val id_column = "id"
    protected final def store_name = store.name
    protected lazy val table_name = tableName getOrElse name.name

    private lazy val _sql_builder = SqlBuilder(table_name, id_column)

    def get(id: Id): Option[Record] = {
      val sql = _sql_builder.get(id)
      store.sqlContext.queryHeadOption(store_name, sql)
    }

    def query(q: Query): RecordSequence = {
      val sql = s"""SELECT * FROM ${table_name} WHERE ${q.where} LIMIT 10"""
      store.sqlContext.querySequence(store_name, sql)
    }

    def insert(rec: Record): Id = {
      val idoption = rec.get('id)
      val columns = ???
      val values = ???
      val sql = s"""INSERT INTO ${table_name} $columns ($values)"""
      store.sqlContext.mutate(store_name, sql)
      val id = idoption.getOrElse(_fetch_insert_id)
      Id.create(id)
    }

    private def _fetch_insert_id: Any = _fetch_insert_id_mysql

    // http://www.mysqltutorial.org/mysql-last_insert_id.aspx
    private def _fetch_insert_id_mysql = {
      val sql = s"""SELECT LAST_INSERT_ID()"""
      store.sqlContext.queryHeadOption(store_name, sql).getOrElse(-1)
    }

    def update(id: Id, rec: Record): Unit = {
      val sql = _sql_builder.update(id.string, rec)
      store.sqlContext.mutate(store_name, sql)
    }

    def delete(id: Id): Unit = {
      val sql = s"""DELETE FROM ${table_name} WHERE id = '${id.string}"""
      store.sqlContext.mutate(store_name, sql)
    }

    def drop(): Unit = {
      val sql = s"""DROP TABLE ${table_name}"""
      store.sqlContext.execute(store_name, sql)
    }
  }

  class SqlSchemaCollction(
    val store: SqlStore,
    val name: Symbol,
    val schema: Schema
  ) extends Collection {
    def get(id: Id): Option[Record] = RAISE.notImplementedYetDefect
    def query(q: Query): RecordSequence = RAISE.notImplementedYetDefect
    def insert(rec: Record): Id = RAISE.notImplementedYetDefect
    def update(id: Id, rec: Record): Unit = RAISE.notImplementedYetDefect
    def delete(id: Id): Unit = RAISE.notImplementedYetDefect
    def drop(): Unit = RAISE.notImplementedYetDefect
  }
}
