package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.xsv.Lxsv

/*
 * @since   Jul. 21, 2019
 * @version Jul. 21, 2019
 * @author  ASAMI, Tomoharu
 */
trait LxsvPart { self: Record =>
  lazy val toLxsv: Lxsv = Lxsv(fields.flatMap(_.keyValue))
}

object LxsvPart {
}
