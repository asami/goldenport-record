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
trait MutateDriver extends RecordConstants {
  def insert(record: Record) {
    insert(RecordSet(List(record)))
  }
  def insert(records: RecordSet) {
    throw new UnsupportedOperationException()
  }
  def insertId(record: Record): Long = {
    throw new UnsupportedOperationException()
  }

  def update(record: Record) {
    update(RecordSet(List(record)))
  }
  def update(records: RecordSet) {
    throw new UnsupportedOperationException()
  }

  def delete(record: Record) {
    delete(RecordSet(List(record)))
  }
  def delete(records: RecordSet) {
    throw new UnsupportedOperationException()
  }

  def delete(id: String) {
    delete(List(id))
  }
  def delete(ids: Seq[String]) {
    throw new UnsupportedOperationException()
  }

  // XXX RestController
  protected final def prop_name(n: String) = {
    n.split("[.]").last.toLowerCase
  }
}

object NullMutateDriver extends MutateDriver {
}
