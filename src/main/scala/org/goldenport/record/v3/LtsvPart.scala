package org.goldenport.record.v3

import org.goldenport.exception.RAISE

/*
 * @since   Aug. 23, 2018
 * @version Aug. 24, 2018
 * @author  ASAMI, Tomoharu
 */
trait LtsvPart { self: Record =>
  def toLtsv: String = {
    fields.map(_.toLtsv).mkString("\t")
  }

  def toLtsvPart: String = {
    fields.map(x => "\t" + x.toLtsv).mkString
  }
}

object LtsvPart {
}
