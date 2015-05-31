package org.goldenport.record.ext

import _root_.scalikejdbc._
import org.goldenport.record._
import org.goldenport.record.v2
import org.goldenport.record.v3

/*
 * @since   May. 31, 2015
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
package object scalikejdbc {
  implicit def SqlTCRecordSet[T, E <: WithExtractor](implicit session: DBSession) = new TCRecordSet[SQL[T, E]] {
    // def exportTo(rs: SQL[T, E], file: String) = {
    //   ???
    // }

    def toV2(rs: SQL[T, E]) = {
      ???
    }

    // def toV3(rs: SQL[T, E]) = {
    //   ???
    // }

    def foreach(rs: SQL[T, E], op: TCRecord[SQL[T, E]] => Unit): Unit = {
      ???
    }

    def foldLeft[A](rs: SQL[T, E], z: A)(op: (A, TCRecord[SQL[T, E]]) => A): A = {
      ???
    }
  }

  implicit object WrappedResultSetTCRecord extends TCRecord[WrappedResultSet] {
    private def _to_vector(wrs: WrappedResultSet): Vector[(String, Any)] = {
      val meta = wrs.metaData
        (1 to wrs.metaData.getColumnCount).foldLeft(Vector.empty[(String, Any)]) { (z, i) =>
          val label = meta.getColumnLabel(i)
          Option(wrs.any(label)).map(x => z :+ (label -> x)) getOrElse z
        }
    }

    def toV2(wrs: WrappedResultSet): v2.Record = {
      val xs = _to_vector(wrs)
      v2.Record.create(xs)
    }

    def toV3(wrs: WrappedResultSet): v3.Record = {
      val xs = _to_vector(wrs)
      v3.Record.fromDataSeq(xs)
    }
  }
}
