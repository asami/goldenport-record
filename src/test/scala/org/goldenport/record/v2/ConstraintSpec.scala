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

    "accept valid date" in {
      val c = CFormat("date")
      c.validate(XString, "2026-03-25", Record.empty) should be(None)
    }

    "reject invalid date" in {
      val c = CFormat("date")
      c.validate(XString, "2026-13-40", Record.empty).isDefined should be(true)
    }

    "accept valid time" in {
      val c = CFormat("time")
      c.validate(XString, "23:59:58", Record.empty) should be(None)
    }

    "accept valid date-time alias" in {
      val c = CFormat("date_time")
      c.validate(XString, "2026-03-25T07:15:30+09:00", Record.empty) should be(None)
    }

    "accept valid phone number" in {
      val c = CFormat("phone")
      c.validate(XString, "+819012345678", Record.empty) should be(None)
    }

    "reject unsupported format" in {
      val c = CFormat("postcode")
      c.validate(XString, "100-0001", Record.empty).isDefined should be(true)
    }
  }
}
