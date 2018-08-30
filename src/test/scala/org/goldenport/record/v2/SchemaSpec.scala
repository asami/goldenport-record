package org.goldenport.record.v2

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._

/*
 * @since   Aug. 24, 2018
 * @version Aug. 24, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SchemaSpec extends WordSpec with Matchers {
  "unmarshall" should {
    "json" which {
      import Schema.json._
      "typical" in {
        val json = """{
  "columns": [{
    "name":"one"
  },{
    "name":"two",
    "datatype":"int"
  }]
}"""
        Schema.json.unmarshall(json) should be(Schema(List(
          Column("one"),
          Column("two", XInt)
        )))
      }
    }
  }
}
