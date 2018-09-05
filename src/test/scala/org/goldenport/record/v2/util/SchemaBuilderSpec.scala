package org.goldenport.record.v2.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.record.v2._, SchemaBuilder._
import org.goldenport.record.v2.projector.{Importer => PImporter}

/*
 * @since   Sep.  4, 2018
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SchemaBuilderSpec extends WordSpec with Matchers with GivenWhenThen {
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
