package org.goldenport.record.v2

import scalaz._, Scalaz._
import scala.util.control.Exception.allCatch
import java.sql.{Connection, Clob}
import java.io.Reader
import java.text.SimpleDateFormat

/*
 * Derived from Driver.
 * 
 * @since   Nov. 24, 2012
 *  version Dec. 29, 2012
 *  version Jan. 16, 2013
 * @version Mar.  3, 2013
 * @author  ASAMI, Tomoharu
 */
trait QueryDriver extends RecordConstants {
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.sql.SQLException])
  def select(start: Option[Int], limit: Option[Int], orderby: Seq[(String, OrderBy)], props: Map[String, String]): RecordSet

  // XXX RestController
  protected final def prop_name(n: String) = {
    n.split("[.]").last.toLowerCase
  }
}

object NullQueryDriver extends QueryDriver {
  def select(start: Option[Int], limit: Option[Int], orderby: Seq[(String, OrderBy)], props: Map[String, String]) = {
    sys.error("driver not be specified.")
  }
}
