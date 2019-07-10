package org.goldenport.record.v3

import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Jun. 22, 2019
 * @version Jun. 23, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TableVisualizerSpec extends WordSpec with Matchers {
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
