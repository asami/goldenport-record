package org.goldenport.record.http

import scala.xml._
import play.api.libs.json.JsValue
import org.goldenport.exception.RAISE
import org.goldenport.bag.BufferFileBag
import org.goldenport.record.v2.Record
import org.goldenport.record.v2.util.SchemaBuilder

/*
 * Unify arcadia
 * 
 * @since   Oct.  8, 2017
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def code: Int
  def mime: String
  def getString: Option[String]
  def getRecord: Option[Record]
  def getRecords: Option[List[Record]]
  def json: JsValue
}

case class StringResponse(
  code: Int,
  mime: String,
  content: String
) extends Response {
}

case class BinaryResponse(
  code: Int,
  mime: String,
  content: BufferFileBag
) extends Response {
}
