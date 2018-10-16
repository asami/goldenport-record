package org.goldenport.record.v2.projector

import org.goldenport.record.v2._

/*
 * @since   Jul. 20, 2018
 * @version Jul. 21, 2018
 * @author  ASAMI, Tomoharu
 */
trait Builder {
  def apply(src: Record, sink: Record)(implicit ctx: ProjectorContext): Record
}

case class LoaderBuilder(loaders: Vector[Loader]) extends Builder {
  def apply(src: Record, sink: Record)(implicit ctx: ProjectorContext): Record =
    loaders./:(sink)((z, x) => x(src, z))
}

object Builder {
}
