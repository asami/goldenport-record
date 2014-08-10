package org.goldenport.record.v2.util

import org.goldenport.record.v2._

/*
 * @snice   Aug.  9, 2014
 * @version Aug. 10, 2014
 * @author  asami
 */
case class Merger(
  srcAttributes: Seq[Symbol],
  updateAttributes: Seq[Symbol],
  mapAttributes: Map[Symbol, (List[Any], List[Any]) => List[Any]]
) {
  def merge(src: Record, update: Record): Record = {
    update.fields.foldLeft(src)(_merge(src, update))
  }

  private def _merge(src: Record, update: Record)(z: Record, x: Field): Record = {
    z.get(x.key) match {
      case Some(s) => z.updateS(x.key -> merge(src, update, x.key, s, x.values))
      case None => z :+ x
    }
  }

  def merge(
    src: Record, update: Record,
    key: Symbol, srcv: List[Any], updatev: List[Any]): List[Any] = {
    // src, update: for future use
    mapAttributes.get(key).map(_(srcv, updatev)) getOrElse {
      if (srcAttributes.contains(key))
        srcv
      else if (updateAttributes.contains(key))
        updatev
      else
        throw new UnsupportedOperationException(s"Undefined key: $key")
    }
  }
}
