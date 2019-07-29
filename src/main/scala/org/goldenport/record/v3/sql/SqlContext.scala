package org.goldenport.record.v3.sql

import scala.util.Try
import java.sql.ResultSet
import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig
import org.goldenport.collection.NonEmptyVector
import org.goldenport.record.v3.{IRecord, Record, RecordSequence}

/*
 * @since   Mar. 23, 2019
 *  version Apr.  8, 2019
 *  version May. 25, 2019
 * @version Jul. 26, 2019
 * @author  ASAMI, Tomoharu
 */
class SqlContext(
  val config: RichConfig,
//  factories: NonEmptyVector[SqlConnectionFactory]
  val transaction: SqlContext.TransactionStrategy
) {
  import SqlContext._

  def addProperties(p: IRecord) = {
    transaction.addProperties(p)
    this
  }

  def takeDatabaseNames: List[Symbol] = transaction.takeDatabaseNames

  def query(sql: String): IndexedSeq[IRecord] = query(KEY_DEFAULT, sql)

  def query(database: Symbol, sql: String): IndexedSeq[IRecord] =
    querySequence(database, sql).irecords

  def querySequence(sql: String): RecordSequence = RecordSequence.createClose(queryIterator(sql))

  def querySequence(database: Symbol, sql: String): RecordSequence =
    RecordSequence.createClose(queryIterator(database, sql))

  def queryIterator(sql: String): RecordIterator = queryIterator(KEY_DEFAULT, sql)

  def queryIterator(database: Symbol, sql: String): RecordIterator = transaction.queryIterator(database) { conn => 
    val stmt = conn.createStatement()
    stmt.executeQuery(sql)
  }

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

  def mutate(database: Symbol, sql: String): Int = transaction.execute(database) { conn => 
    val stmt = conn.createStatement()
    stmt.executeUpdate(sql)
  }

  def execute(database: Symbol, sql: String): Unit = transaction.execute(database) { conn =>
    val stmt = conn.createStatement()
    stmt.execute(sql)
  }

  def isExists(db: Symbol): Boolean = transaction.isExists(db)

  // def openConnection(): java.sql.Connection = openConnection(KEY_DEFAULT)

  // def openConnection(key: Symbol): java.sql.Connection = 
  //   _factory(key).openConnection(key)

  def commit(): Unit = transaction.commit()

  def abort(): Unit = transaction.abort()

  def close(): Unit = transaction.close()

  //private val _factories: Vector[SqlConnectionFactory] = Vector(new HikariCPFactory(config))
  // private val _factories = factories.vector
  // private var _connections: Vector[DatabaseSlot] = Vector.empty

  // private def _factory(db: Symbol): SqlConnectionFactory =
  //   _get_factory(db) getOrElse RAISE.noSuchElementFault(s"Unknown database: ${db}")

  // private def _get_factory(db: Symbol): Option[SqlConnectionFactory] = _factories.toStream.find(_.isAcceptDatabase(db)).headOption

  // def addProperties(p: IRecord) = {
  //   _factories.map(_.addProperties(p))
  //   this
  // }

  // def takeDatabaseNames: List[Symbol] = _factories.toList.flatMap(_.databaseNames)

  // def query(sql: String): IndexedSeq[Record] = query(KEY_DEFAULT, sql)

  // def query(database: Symbol, sql: String): IndexedSeq[Record] =
  //   querySequence(database, sql).vector

  // def querySequence(sql: String): RecordSequence = RecordSequence.createClose(queryIterator(sql))

  // def querySequence(database: Symbol, sql: String): RecordSequence =
  //   RecordSequence.createClose(queryIterator(database, sql))

  // def queryIterator(sql: String): RecordIterator = queryIterator(KEY_DEFAULT, sql)

  // def queryIterator(database: Symbol, sql: String): RecordIterator = {
  //   val conn = openConnection(database)
  //   val stmt = conn.createStatement()
  //   val rs = stmt.executeQuery(sql)
  //   ConnectionResultSetRecordIterator.create(rs)
  // }

  // def queryFold[T](sql: String)(f: RecordIterator => T): T = queryFold(KEY_DEFAULT, sql)(f)

  // def queryFold[T](database: Symbol, sql: String)(f: RecordIterator => T): T =
  //   f(queryIterator(database, sql))

  // def queryHeadOption(database: Symbol, sql: String): Option[Record] = {
  //   var iter: RecordIterator = null
  //   try {
  //     iter = queryIterator(database, sql)
  //     if (iter.hasNext)
  //       Some(iter.next)
  //     else
  //       None
  //   } finally {
  //     if (iter != null)
  //       iter.close()
  //   }
  // }

  // def mutate(sql: String): Int = mutate(KEY_DEFAULT, sql)

  // def mutate(database: Symbol, sql: String): Int = {
  //   val conn = takeConnection(database)
  //   val stmt = conn.createStatement()
  //   stmt.executeUpdate(sql)
  // }

  // def execute(database: Symbol, sql: String): Unit = {
  //   val conn = takeConnection(database)
  //   val stmt = conn.createStatement()
  //   stmt.execute(sql)
  // }

  // def isExists(db: Symbol): Boolean = _get_factory(db).isDefined

  // def openConnection(): java.sql.Connection = openConnection(KEY_DEFAULT)

  // def openConnection(key: Symbol): java.sql.Connection = 
  //   _factory(key).openConnection(key)

  // private def takeConnection(): java.sql.Connection = takeConnection(KEY_DEFAULT)

  // private def takeConnection(key: Symbol): java.sql.Connection = // concurrency
  //   _get_connection(key).map(_.connection) getOrElse {
  //     val a = _factory(key).openConnection(key)
  //     synchronized {
  //       _get_connection(key).map { x =>
  //         a.close()
  //         x.connection
  //       }.getOrElse {
  //         _connections = _connections :+ DatabaseSlot(key, a)
  //         a
  //       }
  //     }
  //   }

  // private def _get_connection(key: Symbol): Option[DatabaseSlot] =
  //   _connections.find(_.key == key)

  // def commit(): Unit = {
  //   RAISE.notImplementedYetDefect
  // }

  // def abort(): Unit = {
  //   RAISE.notImplementedYetDefect
  // }

  // def close(): Unit = {
  //   RAISE.notImplementedYetDefect
  // }
}

object SqlContext {
  val KEY_DEFAULT = 'default

  val empty = new SqlContext(
    RichConfig.empty,
    new AutoCommitTransaction(NonEmptyVector(new PlainCPFactory(RichConfig.empty)))
  )

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

  sealed trait TransactionStrategy {
    def factories: NonEmptyVector[SqlConnectionFactory]

    private val _factories = factories.vector
    private var _connections: Vector[DatabaseSlot] = Vector.empty

    private def _factory(db: Symbol): SqlConnectionFactory =
      _get_factory(db) getOrElse RAISE.noSuchElementFault(s"Unknown database: ${db}")

    private def _get_factory(db: Symbol): Option[SqlConnectionFactory] = _factories.toStream.find(_.isAcceptDatabase(db)).headOption

    def isExists(db: Symbol): Boolean = _get_factory(db).isDefined

    def addProperties(p: IRecord): Unit =
      _factories.map(_.addProperties(p))

    def takeDatabaseNames: List[Symbol] = _factories.toList.flatMap(_.databaseNames)

    def openConnection(): java.sql.Connection = openConnection(KEY_DEFAULT)

    def openConnection(key: Symbol): java.sql.Connection =
      _factory(key).openConnection(key)

    def queryIterator(db: Symbol)(body: java.sql.Connection => ResultSet): RecordIterator

    def execute[T](db: Symbol)(body: java.sql.Connection => T): T

    def commit(): Unit

    def abort(): Unit

    def close(): Unit

    protected def take_connection(): java.sql.Connection = take_connection(KEY_DEFAULT)

    protected def take_connection(key: Symbol): java.sql.Connection = // concurrency
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
  }

  class EachTimeTransaction(
    val factories: NonEmptyVector[SqlConnectionFactory]
  ) extends TransactionStrategy {
    def queryIterator(db: Symbol)(body: java.sql.Connection => ResultSet): RecordIterator = {
      val conn = openConnection(db)
      val rs = body(conn)
      ConnectionResultSetRecordIterator.create(rs)
    }

    def execute[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = openConnection(db)
      try {
        val r = body(conn)
        conn.commit()
        r
      } catch {
        case e: Throwable => conn.rollback(); throw e
      } finally {
        conn.close()
      }
    }

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

  class AutoCommitTransaction(
    val factories: NonEmptyVector[SqlConnectionFactory]
  ) extends TransactionStrategy {
    def queryIterator(db: Symbol)(body: java.sql.Connection => ResultSet): RecordIterator = {
      val conn = take_connection(db)
      val rs = body(conn)
      ConnectionResultSetRecordIterator.create(rs)
    }

    def execute[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = take_connection(db)
      try {
        conn.setAutoCommit(false)
        val r = body(conn)
        conn.commit()
        r
      } catch {
        case e: Throwable => conn.rollback(); throw e
      }
    }

    private def _execute_h2[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = take_connection(db)
      conn.setAutoCommit(true)
      body(conn)
    }

    private def _execute_mysql[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = take_connection(db)
      try {
        conn.setAutoCommit(false)
        val r = body(conn)
        conn.commit()
        r
      } catch {
        case e: Throwable => conn.rollback(); throw e
      }
    }

    def commit(): Unit = {}
    def abort(): Unit = {}
    def close(): Unit = {}
  }

  class ScopeTransaction(
    val factories: NonEmptyVector[SqlConnectionFactory]
  ) extends TransactionStrategy {
    def queryIterator(db: Symbol)(body: java.sql.Connection => ResultSet): RecordIterator = {
      val conn = take_connection(db)
      val rs = body(conn)
      ResultSetRecordIterator.create(rs)
    }

    def execute[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = take_connection(db)
      conn.setAutoCommit(false)
      body(conn)
    }

    def commit(): Unit = { // TODO
      RAISE.notImplementedYetDefect
    }

    def abort(): Unit = {
      RAISE.notImplementedYetDefect
    }

    def close(): Unit = {
      RAISE.notImplementedYetDefect
    }
  }

  def createSync(p: RichConfig): SqlContext = new SqlContext(
    p,
    new AutoCommitTransaction(NonEmptyVector(new PlainCPFactory(p)))
  )

  def createConnectionPool(p: RichConfig): SqlContext = createHikari(p)

  def createHikari(p: RichConfig): SqlContext = new SqlContext(
    p,
    new ScopeTransaction(NonEmptyVector(new HikariCPFactory(p)))
  )
}
