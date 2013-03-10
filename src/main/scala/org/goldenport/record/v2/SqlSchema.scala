package org.goldenport.record.v2

import scalaz._, Scalaz._
import java.net.URI

/*
 * @since   Dec. 28, 2012
 *  version Dec. 31, 2012
 *  version Jan.  9, 2013
 * @version Mar. 10, 2013
 * @author  ASAMI, Tomoharu
 */
case class SqlSchema(actions: Seq[SqlAction]) {
  def useId = actions.exists(_.useId)
}

object NullSqlSchema extends SqlSchema(Nil)
