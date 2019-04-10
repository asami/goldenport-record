package org.goldenport.record.v3.sql

import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig
import org.goldenport.record.v3.{IRecord, Record, RecordSequence}

/*
 * @since   Mar. 23, 2019
 * @version Apr.  8, 2019
 * @author  ASAMI, Tomoharu
 */
class SqlContext(config: RichConfig) {
  import SqlContext._

  private val _factories: Vector[SqlConnectionFactory] = Vector(new HikariCPFactory(config))
  private var _connections: Vector[DatabaseSlot] = Vector.empty

  private def _factory(db: Symbol): SqlConnectionFactory =
    _get_factory(db) getOrElse RAISE.noSuchElementFault(s"Unknown database: ${db}")

  private def _get_factory(db: Symbol): Option[SqlConnectionFactory] = _factories.toStream.find(_.isAcceptDatabase(db)).headOption

  def addProperties(p: IRecord) = {
    _factories.map(_.addProperties(p))
    this
  }

  def takeDatabaseNames: List[Symbol] = _factories.toList.flatMap(_.databaseNames)

  def query(sql: String): IndexedSeq[Record] = query(KEY_DEFAULT, sql)

  def query(database: Symbol, sql: String): IndexedSeq[Record] =
    querySequence(database, sql).vector

  def queryIterator(sql: String): RecordIterator = queryIterator(KEY_DEFAULT, sql)

  def queryIterator(database: Symbol, sql: String): RecordIterator = {
    val conn = takeConnection(database)
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery(sql)
    ResultSetRecordIterator.create(rs)
  }

  def querySequence(sql: String): RecordSequence = RecordSequence.createClose(queryIterator(sql))

  def querySequence(database: Symbol, sql: String): RecordSequence =
    RecordSequence.createClose(queryIterator(database, sql))

  def queryFold[T](sql: String)(f: RecordIterator => T): T = queryFold(KEY_DEFAULT, sql)(f)

  def queryFold[T](database: Symbol, sql: String)(f: RecordIterator => T): T =
    f(queryIterator(database, sql))

  def queryHeadOption(database: Symbol, sql: String): Option[Record] = {
    var iter: RecordIterator = null
    try {
      iter = queryIterator(database, sql)
      if (iter.hasNext)
        Some(iter.next)
      else
        None
    } finally {
      if (iter != null)
        iter.close()
    }
  }

  def mutate(sql: String): Int = mutate(KEY_DEFAULT, sql)

  def mutate(database: Symbol, sql: String): Int = {
    val conn = takeConnection(database)
    val stmt = conn.createStatement()
    stmt.executeUpdate(sql)
  }

  def execute(database: Symbol, sql: String): Unit = {
    val conn = takeConnection(database)
    val stmt = conn.createStatement()
    stmt.execute(sql)
  }

  def isExists(db: Symbol): Boolean = _get_factory(db).isDefined

  def takeConnection(): java.sql.Connection = takeConnection(KEY_DEFAULT)

  def takeConnection(key: Symbol): java.sql.Connection = // concurrency
    _get_connection(key).map(_.connection) getOrElse {
      val a = _factory(key).openConnection(key)
      synchronized {
        _get_connection(key).map { x =>
          a.close()
          x.connection
        }.getOrElse {
          _connections = _connections :+ DatabaseSlot(key, a)
          a
        }
      }
    }

  private def _get_connection(key: Symbol): Option[DatabaseSlot] =
    _connections.find(_.key == key)

  def commit(): Unit = {
    RAISE.notImplementedYetDefect
  }

  def abort(): Unit = {
    RAISE.notImplementedYetDefect
  }

  def close(): Unit = {
    RAISE.notImplementedYetDefect
  }
}

object SqlContext {
  val KEY_DEFAULT = 'default

  val empty = new SqlContext(RichConfig.empty)

  case class DatabaseConfig(
    key: Symbol,
    url: String,
    driver: Option[String],
    user: Option[String],
    password: Option[String]
  )

  case class DatabaseSlot(
    key: Symbol,
    connection: java.sql.Connection
  )

  def create(p: RichConfig): SqlContext = new SqlContext(p)
}
