package org.goldenport.record.v2

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest._

/*
 * @since   Mar. 25, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ConstraintSpec extends WordSpec with Matchers {
  "CFormat" should {
    "accept valid email" in {
      val c = CFormat("email")
      c.validate(XString, "foo@example.com", Record.empty) should be(None)
    }

    "reject invalid email" in {
      val c = CFormat("email")
      c.validate(XString, "foo.example.com", Record.empty).isDefined should be(true)
    }

    "support url as uri alias" in {
      val c = CFormat("url")
      c.validate(XString, "https://example.com/a", Record.empty) should be(None)
    }
  }
}
