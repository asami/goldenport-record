package org.goldenport.record.v3.sql

import scala.collection.mutable
import com.zaxxer.hikari._
import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig
import SqlContext._

/*
 * @since   Mar. 23, 2019
 * @version Mar. 24, 2019
 * @author  ASAMI, Tomoharu
 */
class HikariCPFactory(val config: RichConfig) extends SqlConnectionFactory {
  private val _datasources: mutable.Map[Symbol, HikariDataSource] = mutable.Map.empty

  protected def open_Connection(db: DatabaseConfig): java.sql.Connection =
    _datasource(db).getConnection

  private def _datasource(db: DatabaseConfig): HikariDataSource = synchronized {
    _datasources.get(db.key) getOrElse _build(db)
  }

  private def _build(db: DatabaseConfig): HikariDataSource = {
    val config = new HikariConfig()
    config.setJdbcUrl(db.url)
    db.driver.foreach(config.setDriverClassName)
    db.user.foreach(config.addDataSourceProperty("user", _))
    db.password.foreach(config.addDataSourceProperty("password", _))
    _build(db.key, config)
  }

  private def _build(name: Symbol, c: HikariConfig) = {
    // // キャッシュ系の設定(任意)
    c.addDataSourceProperty("cachePrepStmts", "true")
    c.addDataSourceProperty("prepStmtCacheSize", "250")
    c.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    // // サーバサイドプリペアードステートメントを使用する(任意)
    c.addDataSourceProperty("useServerPrepStmts", "true")
    // // 最小接続数まで接続を確保できない時に例外を投げる
    // c.setInitializationFailFast(true)
    // // 接続をテストするためのクエリ
    c.setConnectionInitSql("SELECT 1")
    val ds = new HikariDataSource(c)
    _register(name, ds)
    ds
  }

  private def _register(name: Symbol, ds: HikariDataSource) {
    _datasources += (name -> ds)
  }
}

object HikariCPSqlContext {
}
