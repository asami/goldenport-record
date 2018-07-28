package org.goldenport.record.v2.projector

import org.goldenport.record.v2._

/*
 * @since   Jul. 19, 2018
 * @version Jul. 28, 2018
 * @author  ASAMI, Tomoharu
 */
trait Importer extends org.goldenport.record.v2.Importer {
  def fetcher: Fetcher
  def default: Option[Any]

  def apply(column: Column, src: Record): Option[Any] =
    fetcher.apply(src)(ProjectorContext(column)).orElse(default)

  override def apply(ctx: ProjectorContext, column: Column, src: Record): Option[Any] =
    fetcher.apply(src)(ctx.withColumn(column)).orElse(default)
}

object Importer {
  def fetcher(p: Fetcher): Importer = FetcherImporter(p, None)
  def fetcher(p: Fetcher, d: Any): Importer = FetcherImporter(p, Some(d))

  def pathname(p: String): Importer = FetcherImporter(Fetcher.pathname(p), None)
  def pathname(p: String, d: Any): Importer = FetcherImporter(Fetcher.pathname(p), Some(d))
  lazy val yyyymmdd: Importer = FetcherImporter(Fetcher.yyyymmdd, None)
  def yyyymmdd(d: Any): Importer = FetcherImporter(Fetcher.yyyymmdd, Some(d))
  def partialFunction(pf: PartialFunction[Any, Any]): Importer = FetcherImporter(Fetcher.partialFunction(pf), None)
}

case class FetcherImporter(
  fetcher: Fetcher,
  default: Option[Any] = None
) extends Importer {
}
