package org.goldenport.record.transformer

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.goldenport.record.v2.{Schema, Column, XInt}
import org.goldenport.record.v3._
import org.goldenport.record.transformer._
import org.goldenport.record.transformer.TransformerRule._

/**
 * @since   Jun.  1, 2015
 * @version Aug.  2, 2015
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TransformerSpec extends WordSpec with Matchers with GivenWhenThen with GeneratorDrivenPropertyChecks {
  "transform" should {
    "column" which {
      "1to1 string" in {
        val rec = Record.data('a -> "one")
        val rule = TransformerRule.create(
          ColumnRule1to1(Column("a"), Column("x"))
        )
        val f = Transformer(rule)
//        println("XXX = " + f.transform(rec).toLog)
        f.transform(rec) should be(Record.data('x -> "one"))
      }
      "1to1 int string" in {
        val rec = Record.data('a -> 1)
        val rule = TransformerRule.create(
          ColumnRule1to1(Column("a"), Column("x"))
        )
        val f = Transformer(rule)
        f.transform(rec) should be(Record.data('x -> "1"))
      }
    }
    "schema" which {
      "1to1 string" in {
        val rec = Record.data('a -> "one")
        val schema = Schema(Vector(Column("a")))
        val rule = TransformerRule.create(
          SchemaRule(schema)
        )
        val f = Transformer(rule)
        f.transform(rec) should be(Record.data('a -> "one"))
      }
      "1to1 int string" in {
        val rec = Record.data('a -> 1)
        val schema = Schema(Vector(Column("a")))
        val rule = TransformerRule.create(
          SchemaRule(schema)
        )
        val f = Transformer(rule)
        f.transform(rec) should be(Record.data('a -> "1"))
      }
      "filter" in {
        val rec = Record.data('a -> "one", 'b -> "two")
        val schema = Schema(Vector(Column("a")))
        val rule = TransformerRule.create(
          SchemaRule(schema)
        )
        val f = Transformer(rule)
        f.transform(rec) should be(Record.data('a -> "one"))
      }
    }
  }
  "failure" should {
    "a" which {
      "1to1 string" in {
        val rec = Record.data('a -> "one")
        val rule = TransformerRule.create(
          ColumnRule1(Column("a", XInt))
        )
        val f = Transformer(rule)
        val r = f.transform(rec)
        r.isValid should be(false)
      }
    }
  }
}
