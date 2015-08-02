package org.goldenport.record.transformer

import scala.util.control.NonFatal
import org.goldenport.record.v3._
import org.goldenport.record.v2.{Schema, Column, ValidationResult}

/*
 * @since   Jun.  1, 2015
 * @version Jul. 20, 2015
 * @author  ASAMI, Tomoharu
 */
case class TransformerRule(rules: Seq[TransformRule]) extends Function1[Record, Record] {
  def transform(context: TransformerContext): TransformerContext = {
    if (rules.isEmpty)
      context.asis
    else
      rules.foldLeft(context) { (z, x) =>
        if (z.in.isDeadLetter) {
          z.asis
        } else {
          val r = try {
            x.transform(z)
          } catch {
            case NonFatal(e) =>
              context.withException(e)
          }
          r
        }
      }
  }

  def apply(rec: Record): Record = {
    try {
      transform(TransformerContext.create(rec)).out
    } catch {
      case NonFatal(e) =>
        rec.withException(e)
    }
  }
}

object TransformerRule {
  def create(rules: TransformRule*) = TransformerRule(rules)
}
