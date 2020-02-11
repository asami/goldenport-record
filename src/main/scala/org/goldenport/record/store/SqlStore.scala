package org.goldenport.record.store

import scala.collection.mutable
import org.goldenport.RAISE
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3._
import org.goldenport.record.v3.sql.{SqlContext, SqlBuilder}
import org.goldenport.record.sql.SqlU

/*
 * @since   Apr.  5, 2019
 *  version Apr. 16, 2019
 *  version May.  9, 2019
 *  version Jul. 15, 2019
 *  version Oct.  7, 2019
 * @version Nov. 27, 2019
 * @author  ASAMI, Tomoharu
 */
class SqlStore(
  val name: Symbol,
  val sqlContext: SqlContext
) extends Store {
  import SqlStore._

  private var _collections: Map[Symbol, Collection] = Map.empty

  private def _set_collection(key: Symbol, c: Collection): Collection = synchronized {
    _collections = _collections + (key -> c)
    c
  }

  def getCollection(collection: Symbol): Option[Collection] =
    _collections.get(collection) orElse _make_collection(collection)

  private def _make_collection(collection: Symbol): Option[Collection] = synchronized {
    _collections.get(collection) orElse {
      if (sqlContext.isExists(collection))
        Some(new SqlCollection(this, collection))
      else
        None
    }
  }

  def takeCollection(collection: Symbol): Collection =
    getCollection(collection).getOrElse(RAISE.noSuchElementFault("Store collection '${collection}'"))

  def get(collection: Symbol, id: Id): Option[Record] =
    takeCollection(collection).get(id)

  def select(collection: Symbol, q: Query): RecordSequence =
    takeCollection(collection).select(q)

  def insert(collection: Symbol, rec: Record): Id =
    takeCollection(collection).insert(rec)

  def update(collection: Symbol, id: Id, rec: Record): Unit =
    takeCollection(collection).update(id, rec)

  def delete(collection: Symbol, id: Id): Unit =
    takeCollection(collection).delete(id)

  def create(collection: Symbol, schema: Schema): Collection = {
    // if (getCollection(collection).isDefined)
    //   RAISE.illegalStateFault(s"Table '${collection.name}' already exits.")
    val builder = SqlBuilder.create(collection.name, schema)
    val sql = builder.createTable()
    sqlContext.execute(name, sql)
    define(collection, schema)
  }

  def drop(collection: Symbol): Unit =
    takeCollection(collection).drop()

  def define(collection: Symbol, schema: Schema): Collection = define(collection, None, schema)

  def define(collection: Symbol, tablename: Option[String], schema: Schema): Collection = {
    val c = new SqlSchemaCollection(this, collection, tablename, schema)
    _set_collection(collection, c)
  }
}

object SqlStore {
  trait SqlCollectionBase extends Collection {
    def store: SqlStore
    def name: Symbol
    def tableName: Option[String]
    def getSchema: Option[Schema]

    implicit private val _sql_context = store.sqlContext
    protected val id_column = "id"
    protected final def store_name = store.name
    protected lazy val table_name = tableName orElse getSchema.flatMap(_.sql.tableName) getOrElse name.name

    private lazy val _sql_builder = SqlBuilder(table_name, id_column, getSchema)

    def get(id: Id): Option[Record] = {
      val sql = _sql_builder.get(id)
      store.sqlContext.selectHeadOption(store_name, sql)
    }

    def select(q: Query): RecordSequence = {
      // TODO SqlBuilder
      val limit = 10
      val sql = s"""SELECT * FROM ${table_name} WHERE ${q.where(getSchema)} LIMIT $limit"""
      store.sqlContext.selectSequence(store_name, getSchema, sql)
    }

    def insert(rec: IRecord): Id = {
      val idoption = rec.get('id)
      // val columns = rec.fields.map(_.key.name).map(x => s"`$x`").mkString(", ")
      // val values = rec.fields.map(_.value match {
      //   case EmptyValue => "NULL"
      //   case SingleValue(v) => v match {
      //     case m: Record => RAISE.notImplementedYetDefect
      //     case m => SqlU.literal(m)
      //   }
      //   case m: MultipleValue => RAISE.notImplementedYetDefect
      // }).mkString(",")
      val sql = _sql_builder.insert(rec)
      // val sql = s"""INSERT INTO ${table_name} ($columns) ($values)"""
      store.sqlContext.mutate(store_name, sql)
      val id = idoption.getOrElse(_fetch_insert_id)
      Id.create(id)
    }

    private def _fetch_insert_id: Any = _fetch_insert_id_mysql

    // http://www.mysqltutorial.org/mysql-last_insert_id.aspx
    private def _fetch_insert_id_mysql = {
      val sql = s"""SELECT LAST_INSERT_ID()"""
      store.sqlContext.selectHeadOption(store_name, sql).getOrElse(-1)
    }

    def update(id: Id, rec: IRecord): Unit = {
      val sql = _sql_builder.update(id.string, rec)
      store.sqlContext.mutate(store_name, sql)
    }

    def delete(id: Id): Unit = {
      // TODO SqlBuilder
      val sql = s"""DELETE FROM ${table_name} WHERE id = '${id.string}"""
      store.sqlContext.mutate(store_name, sql)
    }

    def create(): Unit = {
      val schema = getSchema getOrElse RAISE.illegalStateFault(s"Store '${store_name}' does not have a schema.")
      val sql = _sql_builder.createTable()
      store.sqlContext.execute(store_name, sql)
    }

    def drop(): Unit = {
      // TODO SqlBuilder
      val sql = s"""DROP TABLE ${table_name}"""
      store.sqlContext.execute(store_name, sql)
    }
  }

  class SqlCollection(
    val store: SqlStore,
    val name: Symbol,
    val tableName: Option[String] = None
  ) extends SqlCollectionBase {
    def getSchema = None
  }

  class SqlSchemaCollection(
    val store: SqlStore,
    val name: Symbol,
    val tableName: Option[String],
    val schema: Schema
  ) extends SqlCollectionBase {
    def getSchema = Some(schema)
  }
}
