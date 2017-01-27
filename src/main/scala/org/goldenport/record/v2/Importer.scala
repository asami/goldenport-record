package org.goldenport.record.v2

/*
 * @since   Jan. 15, 2017
 * @version Jan. 21, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Importer {
  def apply(p: Any): Any
}

case object TrimImporter extends Importer {
  def apply(p: Any): Any = p match {
    case m: String => m.trim
    case _ => p
  }
}

trait ExternalImporter extends Importer {
}
