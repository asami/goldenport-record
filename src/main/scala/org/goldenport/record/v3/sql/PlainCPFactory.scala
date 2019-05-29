package org.goldenport.record.v3.sql

import java.sql.DriverManager
import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig
import SqlContext._

/*
 * @since   May. 22, 2019
 * @version May. 22, 2019
 * @author  ASAMI, Tomoharu
 */
class PlainCPFactory(val config: RichConfig) extends SqlConnectionFactory {
  protected def open_Connection(db: DatabaseConfig): java.sql.Connection = {
    db.driver.foreach(Class.forName)
    (db.user, db.password) match {
      case (Some(u), Some(p)) => DriverManager.getConnection(db.url, u, p)
      case (Some(u), None) => DriverManager.getConnection(db.url, u, "")
      case _ => DriverManager.getConnection(db.url)
    }
  }
}
