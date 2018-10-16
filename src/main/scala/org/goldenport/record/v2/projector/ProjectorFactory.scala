package org.goldenport.record.v2.projector

import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.SchemaFactory

/*
 * @since   Sep.  3, 2018
 * @version Oct. 16, 2018
 * @author  ASAMI, Tomoharu
 */
case class ProjectorFactory(
  schemaFactory: SchemaFactory
) {
  implicit val ProjectorFormat = new Format[Projector] {
    def reads(json: JsValue): JsResult[Projector] =
      json match {
        case m: JsObject => JsSuccess(_projector(m.fields))
        case m => JsError(s"Unknown element in columns: $m") // TODO
      }
    def writes(o: Projector): JsValue = RAISE.notImplementedYetDefect

    private def _projector(ps: Seq[(String, JsValue)]) = {
      val m = ps.toMap
      val schema = m.get("schema")
      val policy = m.get("policy")
      Projector.ProjectorBuilder(
        schema.map(schemaFactory.unmarshall),
        policy.map(Projector.Policy.json.unmarshall)
      ).build(Projector.default)
    }
  }

  def marshall(p: Projector): String = Json.toJson(p).toString
  def unmarshall(p: String): Projector = unmarshall(Json.parse(p))
  def unmarshall(p: JsValue): Projector = p.as[Projector]
}
