package org.goldenport.record.v3.sql

import scala.util.Try
import java.sql.ResultSet
import java.sql.Connection._
import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig
import org.goldenport.collection.NonEmptyVector
import org.goldenport.value._
import org.goldenport.record.v3.{IRecord, Record, RecordSequence}
import org.goldenport.record.v2.Schema
import org.goldenport.record.query.QueryExpression

/*
 * @since   Mar. 23, 2019
 *  version Apr.  8, 2019
 *  version May. 25, 2019
 *  version Jul. 26, 2019
 *  version Oct. 31, 2019
 * @version Nov. 19, 2019
 * @author  ASAMI, Tomoharu
 */
class SqlContext(
  val config: RichConfig,
//  factories: NonEmptyVector[SqlConnectionFactory]
  val transaction: SqlContext.TransactionStrategy,
  val isolation: SqlContext.IsolationLevel,
  val syntax: SqlContext.SyntaxRule,
  val queryContext: QueryExpression.Context
) {
  import SqlContext._

  def isWhereUndefinedColumn: Boolean = false // TODO

  def addProperties(p: IRecord) = {
    transaction.addProperties(p)
    this
  }

  def takeDatabaseNames: List[Symbol] = transaction.takeDatabaseNames

  def select(sql: String): IndexedSeq[IRecord] = select(KEY_DEFAULT, sql)

  def select(database: Symbol, sql: String): IndexedSeq[IRecord] =
    selectSequence(database, sql).irecords

  def selectSequence(sql: String): RecordSequence = RecordSequence.createClose(selectIterator(sql))

  def selectSequence(database: Symbol, schema: Option[Schema], sql: String): RecordSequence =
    schema.map(selectSequence(database, _, sql)).getOrElse(selectSequence(database, sql))

  def selectSequence(database: Symbol, schema: Schema, sql: String): RecordSequence =
    RecordSequence.createClose(schema, selectIterator(database, schema, sql))

 def selectSequence(database: Symbol, sql: String): RecordSequence =
    RecordSequence.createClose(selectIterator(database, sql))

  def selectIterator(sql: String): RecordIterator = selectIterator(KEY_DEFAULT, sql)

  def selectIterator(database: Symbol, schema: Schema, sql: String): RecordIterator = transaction.queryIterator(database, schema) { conn => 
    val stmt = conn.createStatement()
    stmt.executeQuery(sql)
  }

  def selectIterator(database: Symbol, sql: String): RecordIterator = transaction.queryIterator(database) { conn => 
    val stmt = conn.createStatement()
    stmt.executeQuery(sql)
  }

  def selectFold[T](sql: String)(f: RecordIterator => T): T = selectFold(KEY_DEFAULT, sql)(f)

  def selectFold[T](database: Symbol, sql: String)(f: RecordIterator => T): T =
    f(selectIterator(database, sql))

  def selectHeadOption(database: Symbol, sql: String): Option[Record] = {
    var iter: RecordIterator = null
    try {
      iter = selectIterator(database, sql)
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
    new AutoCommitTransaction(NonEmptyVector(new PlainCPFactory(RichConfig.empty))),
    TransactionReadUncommitted,
    MySql,
    QueryExpression.Context.default
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

    def queryIterator(db: Symbol, schema: Schema)(body: java.sql.Connection => ResultSet): RecordIterator

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

    def queryIterator(db: Symbol, schema: Schema)(body: java.sql.Connection => ResultSet): RecordIterator = {
      val conn = openConnection(db)
      val rs = body(conn)
      ConnectionResultSetRecordIterator.create(schema, rs)
    }

    def execute[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = openConnection(db)
      try {
        conn.setAutoCommit(true)
        val r = body(conn)
        // conn.commit() // should not use in auto commit mode.
        r
      } catch {
        case e: Throwable =>
          // conn.rollback() // should not use in auto commit mode.
          throw e
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

    def queryIterator(db: Symbol, schema: Schema)(body: java.sql.Connection => ResultSet): RecordIterator = {
      val conn = take_connection(db)
      val rs = body(conn)
      ConnectionResultSetRecordIterator.create(schema, rs)
    }

    def execute[T](db: Symbol)(body: java.sql.Connection => T): T = {
      val conn = take_connection(db)
      try {
        conn.setAutoCommit(true)
        val r = body(conn)
        // conn.commit() // should not use in auto commit mode.
        r
      } catch {
        case e: Throwable =>
          // conn.rollback() // should not use in auto commit mode.
          throw e
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

    def queryIterator(db: Symbol, schema: Schema)(body: java.sql.Connection => ResultSet): RecordIterator = {
      val conn = take_connection(db)
      val rs = body(conn)
      ResultSetRecordIterator.create(schema, rs)
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

  def createEachTime(p: RichConfig, query: QueryExpression.Context): SqlContext = {
    val isolation = TransactionReadUncommitted // TODO
    val db = MySql
    new SqlContext(
      p,
      new EachTimeTransaction(NonEmptyVector(new PlainCPFactory(p))),
      isolation,
      db,
      query
    )
  }

  def createAutoCommit(p: RichConfig, query: QueryExpression.Context): SqlContext = {
    val isolation = TransactionReadUncommitted // TODO
    val db = MySql
    new SqlContext(
      p,
      new AutoCommitTransaction(NonEmptyVector(new PlainCPFactory(p))),
      isolation,
      db,
      query
    )
  }

  def createConnectionPool(p: RichConfig, query: QueryExpression.Context): SqlContext = createHikari(p, query)

  def createHikari(p: RichConfig, query: QueryExpression.Context): SqlContext = {
    val isolation = TransactionReadUncommitted // TODO
    val db = MySql
    new SqlContext(
      p,
      new ScopeTransaction(NonEmptyVector(new HikariCPFactory(p))),
      isolation,
      db,
      query
    )
  }

  sealed trait IsolationLevel extends NamedValueInstance {
    def jdbc: Int
  }
  object IsolationLevel extends EnumerationClass[IsolationLevel] {
    val elements = Vector(
      TransactionNone,
      TransactionReadUncommitted,
      TransactionReadCommitted,
      TransactionRepeatableRead,
      TransactionSerializable
    )
  }
  case object TransactionNone extends IsolationLevel {
    val name = "none"
    val jdbc = TRANSACTION_NONE
  }
  case object TransactionReadUncommitted extends IsolationLevel {
    val name = "read-uncommitted"
    val jdbc = TRANSACTION_READ_UNCOMMITTED
  }
  case object TransactionReadCommitted extends IsolationLevel {
    val name = "read-committed"
    val jdbc = TRANSACTION_READ_COMMITTED
  }
  case object TransactionRepeatableRead extends IsolationLevel {
    val name = "repeatable-read"
    val jdbc = TRANSACTION_REPEATABLE_READ
  }
  case object TransactionSerializable extends IsolationLevel {
    val name = "serializable"
    val jdbc = TRANSACTION_SERIALIZABLE
  }

  sealed trait SyntaxRule extends NamedValueInstance {
    def tableNameLiteral(p: String): String = '"' + p + '"'
    def columnNameLiteral(p: String): String = '"' + p + '"'
  }
  object SyntaxRule extends EnumerationClass[SyntaxRule] {
    val elements = Vector(
      Ansi,
      MySql,
      PostgreSql,
      Oracale
    )
  }
  case object Ansi extends SyntaxRule {
    val name = "ansi"
  }
  case object MySql extends SyntaxRule {
    val name = "mysql"

    override def tableNameLiteral(p: String): String = s"`$p`"
    override def columnNameLiteral(p: String): String = s"`$p`"
  }
  case object PostgreSql extends SyntaxRule {
    val name = "postgresql"
  }
  case object Oracale extends SyntaxRule {
    val name = "oracle"
  }
}
