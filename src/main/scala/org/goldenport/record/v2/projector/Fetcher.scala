package org.goldenport.record.v2.projector

import org.goldenport.record.v2._
import org.goldenport.values.PathName

/*
 * @since   Jul. 19, 2018
 * @version Jul. 23, 2018
 * @author  ASAMI, Tomoharu
 */
case class Fetcher(
  peeker: Peeker,
  converter: Converter
) {
  def apply(src: Record)(implicit ctx: ProjectorContext): Option[Any] = peeker.apply(src).map(converter.apply)
}

object Fetcher {
  def pathname(p: String): Fetcher = Fetcher(PathNamePeeker(p), NoneConverter)
  def pathname(p: PathName): Fetcher = Fetcher(PathNamePeeker(p), NoneConverter)
  lazy val yyyymmdd = Fetcher(AttributePeeker, YYYYMMDDConverter)
  def partialFunction(pf: PartialFunction[Any, Any]): Fetcher =
    Fetcher(AttributePeeker, PartialFunctionConverter(pf))
}
