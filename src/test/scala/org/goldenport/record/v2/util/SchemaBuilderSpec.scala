package org.goldenport.record.v2.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.GivenWhenThen
import org.goldenport.record.v2._, SchemaBuilder._
import org.goldenport.record.v2.projector.{Importer => PImporter}

/*
 * @since   Sep.  4, 2018
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class SchemaBuilderSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "typical" should {
    "typical" in {
      val r = SchemaBuilder.create(
        CLI("one", "ONE", PImporter.yyyymmdd),
        CLI("two", "TWO", PImporter.pathname("/a/b/c")),
        CLTV("three", "THREE", XToken, "ok")
      )
      println(r)
    }
  }
}
