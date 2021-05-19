package org.goldenport.record.v3

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import org.goldenport.xsv.Xsv
import org.goldenport.xsv.Lxsv

/*
 * @since   Jul. 21, 2019
 *  version Oct.  5, 2019
 * @version Apr. 25, 2021
 * @author  ASAMI, Tomoharu
 */
trait LxsvPart { self: Record =>
  lazy val toLxsv: Lxsv = {
    val xs = fields.flatMap(x => try {
      x.keyValue
    } catch {
      case NonFatal(e) => Some((x.key, s"${e}"))
    })
    Lxsv(Xsv.RecordStrategy, xs)
  }
}

object LxsvPart {
}
