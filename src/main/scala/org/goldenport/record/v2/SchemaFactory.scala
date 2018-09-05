package org.goldenport.record.v2

import play.api.libs.json._
import play.api.libs.functional.syntax._
import Column.Form
import org.goldenport.exception.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.json.JsonUtils.Implicits._

/*
 * @since   Sep.  3, 2018
 * @version Sep.  5, 2018
 * @author  ASAMI, Tomoharu
 */
case class SchemaFactory(
  importerFactory: ImporterFactory
) {
  implicit val DataTypeFormat = new Format[DataType] {
    def reads(json: JsValue): JsResult[DataType] =
      json match {
        case JsString(s) => JsSuccess(DataType.to(s))
        case _: JsUndefined => JsError("Undefined.")
        case m => JsError(s"Unknown element in columns: $m")
      }
    def writes(o: DataType): JsValue = JsString(o.name)
  }
  implicit val MultiplicityFormat = new Format[Multiplicity] {
    def reads(json: JsValue): JsResult[Multiplicity] =
      json match {
        case JsString(s) => JsSuccess(Multiplicity.to(s))
        case _: JsUndefined => JsError("Undefined.")
        case m => JsError(s"Unknown element in columns: $m")
      }
    def writes(o: Multiplicity): JsValue = JsString(o.mark)
  }
  implicit val FormFormat = Json.format[Form]
  implicit val ColumnFormat = new Format[Column] {
    def reads(json: JsValue): JsResult[Column] = {
      val name = (json \ "name").as[String]
      val datatype = (json \ "datatype").asOpt[DataType] getOrElse XString
      val multiplicity = (json \ "multiplicity").asOpt[Multiplicity] getOrElse MOne
      val label = (json \ "label").asOpt[String]
      val i18nLabel = (json \ "i18nLabel").asOpt[I18NString]
      val default = (json \ "default").asOpt[String] orElse (json \ "value").asOpt[String]
      val form = {
        val a = (json \ "form").asOpt[Form].getOrElse(Form.empty)
        a.withValueOption(default)
      }
      val importer = importerFactory.unmarshall(json \ "importer")
      val extension = importer.map(Column.Extension.create) getOrElse Column.Extension.empty
      JsSuccess(
        Column(name, datatype, multiplicity,
          label = label, i18nLabel = i18nLabel,
          form = form,
          extension = extension
        )
      )
    }
    def writes(o: Column): JsValue = JsObject(
      List(
        "name" -> JsString(o.name),
        "datatype" -> JsString(o.datatype.name),
        "multiplicity" -> JsString(o.multiplicity.mark)
      ) ++ List(
        o.label.map(x => "label" -> JsString(x)),
        o.i18nLabel.map(x => "i18nLabel" -> Json.toJson(x)),
        if (o.form.isEmpty) None else Some("form" -> Json.toJson(o.form)),
        o.importer.map(x => "importer" -> x.toJson)
      ).flatten
    )
  }

  implicit val SchemaFormat = new Format[Schema] {
    def reads(json: JsValue): JsResult[Schema] = {
      val columns = (json \ "columns") match {
        case JsArray(xs) => xs.map(_.as[Column])
        case m => RAISE.noReachDefect // JsError(s"Unknown element in columns: $m")
      }
      JsSuccess(Schema(columns))
    }
    def writes(o: Schema): JsValue = JsObject(List(
      "columns" -> JsArray(o.columns.map(Json.toJson(_)))
    ))
  }

  def marshall(schema: Schema): String = Json.toJson(schema).toString
  def unmarshall(p: String): Schema = Json.parse(p).as[Schema]
  def unmarshall(p: JsValue): Schema = SchemaFormat.reads(p) match {
    case JsSuccess(s, _) => s
    case m: JsError => throw new IllegalArgumentException(m.toString)
  }
}
