package org.goldenport.record.v2.projector

import org.goldenport.record.v2._

/*
 * @since   Jul. 19, 2018
 * @version Jul. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait Importer extends org.goldenport.record.v2.Importer {
  def fetcher: Fetcher

  def apply(column: Column, src: Record): Option[Any] = fetcher.apply(src)
}

object Importer {
  def pathname(p: String): Importer = FetcherImporter(Fetcher.pathname(p))
}

case class FetcherImporter(
  fetcher: Fetcher
) extends Importer {
}
