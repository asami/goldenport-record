package org.goldenport.record.v2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
 * @since   Apr. 30, 2026
 * @version Apr. 30, 2026
 * @author  ASAMI, Tomoharu
 */
class DataTypeSpec extends AnyWordSpec with Matchers {
  "DataType" should {
    "resolve filebundle as a builtin datatype" in {
      XFileBundle.name shouldBe "filebundle"
      DataType.get("filebundle") shouldBe Some(XFileBundle)
    }
  }
}
