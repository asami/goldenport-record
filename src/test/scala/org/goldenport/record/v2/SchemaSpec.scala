package org.goldenport.record.v2

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.record.v2.projector.{Importer => PImporter}
import org.goldenport.record.v2.util.SchemaBuilder, SchemaBuilder._

/*
 * @since   Aug. 24, 2018
 * @version Sep.  5, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SchemaSpec extends WordSpec with Matchers {
  val importerFactory = ImporterFactory(PImporter.defaultImporterClass)
  val schemaFactory = SchemaFactory(importerFactory)

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
      "importer" in {
        val json = """{
  "columns": [{
    "name":"one",
    "label":"ONE",
    "importer":"yyyymmdd"
  },{
    "name":"two",
    "label":"TWO",
    "datatype":"int",
    "importer": {
      "name":"pathname",
      "pathname":"/a/b/c"
    }
  },{
    "name":"three",
    "label":"THREE",
    "datatype":"token",
    "default":"ok"
  }]
}"""
        val r = schemaFactory.unmarshall(json)
        // println(r.columns.map(_.importer))
        // println(r.columns.map(_.form))
        val s = SchemaBuilder.create(
          CLI("one", "ONE", PImporter.yyyymmdd),
          CLTI("two", "TWO", XInt, PImporter.pathname("/a/b/c")),
          CLTV("three", "THREE", XToken, "ok")
        )
        // println(Schema.comp(r, s))
        r should be(s)
      }
//       "API-2777" in { // TODO migrate to lib
//         val json = """{
//   "columns": [{
//     "name":"sku_number",
//     "label":"品番(SKU)"
//   },{
//     "name":"ean",
//     "label":"JANコード"
//   },{
//     "name":"product_name",
//     "label":"商品名",
//     "importer": {
//       "name":"pathname",
//       "pathname":"/whole/product_name"
//     }
//   },{
//     "name":"color_name",
//     "label":"カラー名",
//     "importer": {
//       "name":"tag_list_leaf_label",
//       "pathname":"productclass_tag",
//       "tagname":"company.product.color"
//     }
//   },{
//     "name":"size_name",
//     "label":"サイズ名",
//     "importer": {
//       "name":"tag_list_leaf_label",
//       "pathname":"product_tag"
//       "tagname":"company.product.size"
//     }
//   },{
//     "name":"fragile_code",
//     "label":"割れ物区分",
//     "default": "0"
//   },{
//     "name":"siteid",
//     "label":"サイトID",
//     "default": "0"
//   }]
// }"""
//         val r = schemaFactory.unmarshall(json)
//         // println(r.columns.map(_.importer))
//         // println(r.columns.map(_.form))
//         val s = SchemaBuilder.create(
//           CLI("one", "ONE", PImporter.yyyymmdd),
//           CLTI("two", "TWO", XInt, PImporter.pathname("/a/b/c")),
//           CLTV("three", "THREE", XToken, "ok")
//         )
//         // println(Schema.comp(r, s))
//         r should be(s)
//       }
     }
  }
}
