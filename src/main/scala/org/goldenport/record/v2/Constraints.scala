package org.goldenport.record.v2

import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.RAISE
import org.goldenport.json.JsonUtils

/*
 * @snice   Jul. 20, 2019
 * @version Jul. 20, 2019
 * @author  ASAMI, Tomoharu
 */
case class Constraints(constraints: List[Constraint]) {
}

object Constraints {
  def createOption(ps: Seq[Constraint]): Option[Constraints] =
    if (ps.isEmpty)
      None
    else
      Some(Constraints(ps.toList))

  object json {
    object Implicits {
      import org.goldenport.record.v2.Constraint.json.Implicits._

      implicit object ConstraintsFormat extends Format[Constraints] {
        def reads(p: JsValue): JsResult[Constraints] = p match {
          case JsArray(xs) => JsonUtils.toListResult(xs.map(Json.fromJson[Constraint])).map(Constraints.apply)
          case _ => JsError(s"$p")
        }
        def writes(o: Constraints): JsValue = RAISE.notImplementedYetDefect
      }
    }
  }
}

