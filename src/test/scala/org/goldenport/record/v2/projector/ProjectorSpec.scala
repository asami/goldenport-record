package org.goldenport.record.v2.projector

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.record.v2._

/*
 * @since   Sep. 24, 2015
 *  version Oct.  9, 2015
 *  version Mar. 11, 2016
 * @version May. 16, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ProjectorSpec extends WordSpec with Matchers with GivenWhenThen {
  "Projector" should {
    "typical" which {
      val schema = Schema(Vector(
        Column("a", label = Some("LabelA")),
        Column("b", label = Some("LabelB"))))
      val projector = Projector(schema)
      val rec = Record.data("a" -> "AAA", "b" -> "BBB")
      "typical" in {
        projector.apply(rec) should be(\/-(rec))
      }
      "unordered" in {
        val a = Record.data("b" -> "BBB", "a" -> "AAA")
        projector.apply(a) should be(\/-(rec))
      }
      "label" in {
        val a = Record.data("LabelA" -> "AAA", "LabelB" -> "BBB")
        projector.apply(a) should be(\/-(rec))
      }
    }
    "alias" which {
      val schema = Schema(Vector(
        Column("a", aliases = List("AliasA")),
        Column("b", aliases = List("AliasB"))))
      val projector = Projector(schema)
      val rec = Record.data("a" -> "AAA", "b" -> "BBB")
      "typical" in {
        projector.apply(rec) should be(\/-(rec))
      }
      "unordered" in {
        val a = Record.data("b" -> "BBB", "a" -> "AAA")
        projector.apply(a) should be(\/-(rec))
      }
      "alias" in {
        val a = Record.data("AliasA" -> "AAA", "AliasB" -> "BBB")
        projector.apply(a) should be(\/-(rec))
      }
    }
    "multiplicity" which {
      val in = Record.data(
        "a" -> "AAA", "a_1" -> "AAA1", "a_2" -> "AAA2", "a_3" -> "AAA3"
      )
      "typical" in {
        val schema = Schema(Vector(
          Column("a", label = Some("LabelA"), multiplicity = MZeroMore)))
        val projector = Projector(schema)
        val out = Record.data("a" -> List("AAA", "AAA1", "AAA2", "AAA3"))
        projector.apply(in) should be(\/-(out))
      }
      "single" in {
        val schema = Schema(Vector(
          Column("a", label = Some("LabelA"))))
        val projector = Projector(schema)
        val out = Record.data("a" -> List("AAA"))
        projector.apply(in) should be(\/-(out))
      }
    }
    "path" which {
      val schema = Schema(Vector(
        Column("a", aliases = List("path/x")),
        Column("b", aliases = List("path/y"))))
      val projector = Projector(schema)
      val in = Record.data("path" -> Record.data("x" -> "AAA", "y" -> "BBB"))
      "typical" in {
        val out = Record.data("a" -> "AAA", "b" -> "BBB")
        projector.apply(in) should be(\/-(out))
      }
    }
  }
}
