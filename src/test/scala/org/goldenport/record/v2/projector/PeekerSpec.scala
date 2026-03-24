package org.goldenport.record.v2.projector

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.GivenWhenThen
import scalaz._, Scalaz._
import org.goldenport.record.v2._

/*
 * @since   Jul. 20, 2018
 *  version Jul. 21, 2018
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class PeekerSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "PathNamePeeker" should {
    implicit val ctx = ProjectorContext.default
    "typical" in {
      val rec = Record.dataApp(
        "a" -> 10
      )
      PathNamePeeker("a").apply(rec) should be(Some(10))
    }
    "nest" in {
      val rec = Record.dataApp(
        "a" -> Record.dataApp(
          "b" -> 10
        )
      )
      PathNamePeeker("a/b").apply(rec) should be(Some(10))
    }
    "nest 3" in {
      val rec = Record.dataApp(
        "a" -> Record.dataApp(
          "b" -> Record.dataApp(
            "c" -> 10
          )
        )
      )
      PathNamePeeker("a/b/c").apply(rec) should be(Some(10))
    }
    "nest 3 and array" in {
      val rec = Record.dataApp(
        "a" -> Record.dataApp(
          "b" -> Record.dataApp(
            "c" -> List(10, 20, 30)
          )
        )
      )
      PathNamePeeker("a/b/c").apply(rec) should be(Some(List(10, 20, 30)))
    }
  }
}
