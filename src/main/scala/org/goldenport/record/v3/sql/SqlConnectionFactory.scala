package org.goldenport.record.v3.sql

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.Strings.totokens
import org.goldenport.hocon.RichConfig
import org.goldenport.record.v3.{IRecord, Record, Field}
import SqlContext._

/*
 * @since   Mar. 23, 2019
 *  version Mar. 30, 2019
 *  version Apr.  6, 2019
 *  version May. 29, 2020
 * @version Oct.  3, 2021
 * @author  ASAMI, Tomoharu
 */
trait SqlConnectionFactory {
  def config: RichConfig

  private lazy val _initial_database_configs: Map[Symbol, SqlContext.DatabaseConfig] =
    config.getRichConfigOption("db").map(db =>
      db.childRichConfigMap flatMap {
        case (name, v) => Some(
          DatabaseConfig(
            Symbol(name),
            v.takeString("url"),
            v.getStringOption("driver"),
            v.getStringOption("user"),
            v.getStringOption("password")
          )
        )
      }
    ).map(_.map(x => x.key -> x).toMap).getOrElse(Map.empty)

  private var _additional_database_configs: Map[Symbol, SqlContext.DatabaseConfig] = Map.empty

  private var _default_database_configs: Map[Symbol, SqlContext.DatabaseConfig] = Map.empty

  def setDefault(driver: String, url: String): Unit = synchronized {
    _default_database_configs = Map('default -> SqlContext.DatabaseConfig('default, url, Some(driver), None, None))
  }

  def addProperties(p: IRecord) = synchronized {
    // println(s"SqlConnectionFactory#addProperties $p")
    case class Z(xs: Map[Symbol, Record] = Map.empty) {
      def r: Map[Symbol, SqlContext.DatabaseConfig] = {
        // println(s"addProperties xs: $xs")
        xs.map {
          case (name, v) =>
            name -> SqlContext.DatabaseConfig(
              name,
              v.takeString('url),
              v.getString("driver"),
              v.getString("user"),
              v.getString("password")
            )
        }
      }
      def +(rhs: Field) = {
        // println(s"SqlConnectionFactory before: ${rhs}")
        // println(s"""SqlConnectionFactory tokens: ${totokens(rhs.name, ".")}""")
        totokens(rhs.name, ".") match {
          case Nil => this
          case "db" :: Nil =>
            // println(s"SqlConnectionFactory in: ${rhs.value}")
            // rhs.value
            RAISE.notImplementedYetDefect
          case "db" :: db :: prop :: Nil =>
            // println(s"SqlConnectionFactory match: $db, $prop")
            // println(s"SqlConnectionFactory match2: ${xs |+| Map(Symbol(db) -> Record.data(prop -> rhs.value))}")
            Z(xs |+| Map(Symbol(db) -> Record.data(prop -> rhs.value)))
          case _ => this
        }
      }
    }
    _additional_database_configs = _additional_database_configs ++ p.fields./:(Z())(_+_).r
    this
  }

  def isAcceptDatabase(db: Symbol): Boolean = true

  def openConnection(db: Symbol): java.sql.Connection =
    open_Connection(_config(db))

  private def _config(key: Symbol): DatabaseConfig = {
    // println(s"_config: ${_additional_database_configs}")
    _additional_database_configs.get(key) orElse _initial_database_configs.get(key) orElse _default_database_configs.get(key) getOrElse RAISE.noSuchElementFault(s"Database not found: ${key.name}")
  }

  def databaseNames: List[Symbol] = {
    val a = _additional_database_configs.map(_._2.key)
    val b = _initial_database_configs.map(_._2.key)
    val c = _default_database_configs.map(_._2.key)
    (a ++ b ++ c).toList.distinct
  }

  protected def open_Connection(slot: DatabaseConfig): java.sql.Connection
}
