package org.goldenport.record.v2.unitofwork

import java.net.URL
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scalaz.{Store => _, _}, Scalaz._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.interpreter._

/*
 * @since   Nov. 15, 2015
 *  version Dec.  8, 2015
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class StoreOperationSpec extends AnyWordSpec with Matchers {
  import StoreOperation._
  "a" should {
    "b" in {
      val interpreter = StoreOperationInterpreter.log
      val store = unitofwork.Store.printer
      val id = unitofwork.Store.StringId("1")
      val rec = Record.empty
      val program: StoreOperationFM[InsertResult] = for {
        a <- get(store, id)
        b <- insert(store, rec)
      } yield b
      // val program: StoreOperationFM[Option[Record]] = for {
      //   a <- get(store, id)
      // } yield a
      val r = StoreOperation.runFM(program)(interpreter)
      println(r)
    }
  }
}
