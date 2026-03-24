package org.goldenport.record.v3

import java.net.URL
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Jun. 22, 2019
 *  version Jun. 23, 2019
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class TableVisualizerSpec extends AnyWordSpec with Matchers {
  "a" should {
    "b" which {
      "c" in {
        val rs = Vector(
          Record.data(
            "A" -> "a",
            "B" -> "b"
          )
        )
        val table = Table(rs)
        val tv = TableVisualizer()
        val t = tv.plainText(table)
        println(t)
      }
      "hader" in {
        val header = Table.Head("One", "Two")
        val data = Vector(
          Record.data(
            "A" -> "a",
            "B" -> "b"
          )
        )
        val table = Table(header, data)
        val tv = TableVisualizer.thick
        val t = tv.plainText(table)
        println(t)
      }
    }
  }
}
