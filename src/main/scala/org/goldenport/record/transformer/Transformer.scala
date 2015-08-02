package org.goldenport.record.transformer

import scala.util.control.NonFatal
import org.goldenport.record._
import org.goldenport.record.v3._

/*
 * @since   Jun.  1, 2015
 *  version Jul.  5, 2015
 * @version Aug.  2, 2015
 * @author  ASAMI, Tomoharu
 */
case class Transformer(rule: TransformerRule) {
  def transform(rec: Record): Record = {
//    println(s"Transformer#transform $rule $rec")
    try {
      val r = rule.transform(TransformerContext.create(rec)).out
      r.addSourceAtMostOnce(rec)
    } catch {
      case NonFatal(e) => rec.withException(e)
    }
  }
}
