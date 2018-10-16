package org.goldenport.record.v2.projector

import org.goldenport.record.v2._

/*
 * @since   Jul. 20, 2018
 * @version Jul. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class Loader(
  fetcher: Fetcher,
  storer: Storer
) {
  def apply(src: Record, sink: Record)(implicit ctx: ProjectorContext): Record =
    fetcher.apply(src).fold(sink)(storer.apply(sink, _))
}
  
