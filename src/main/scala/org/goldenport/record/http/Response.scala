package org.goldenport.record.http

import scala.xml._
import java.io.InputStream
import java.nio.charset.Charset
import play.api.libs.json.JsValue
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.bag.BufferFileBag
import org.goldenport.record.v2.Record
import org.goldenport.record.v2.util.SchemaBuilder
import org.goldenport.io.{ContentType, MimeType, IoUtils}
import org.goldenport.bag.{ChunkBag, BufferFileBag}

/*
 * Unify arcadia
 * 
 * @since   Oct.  8, 2017
 *  version Aug. 29, 2018
 * @version Sep. 18, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def code: Int
  def contentType: ContentType
  def mime: MimeType = contentType.mime
  def charset: Option[Charset] = contentType.charset
  def getString: Option[String]
  def getRecord: Option[Record]
  def getRecords: Option[List[Record]]
  def json: JsValue
  def show: String
  def isSuccess = code == 200
}

object Response {
  def parser(code: Int, header: Map[String, IndexedSeq[String]], in: InputStream): Response = {
    val contenttype = header.get("Content-Type").flatMap(_.headOption.map(ContentType.parse)).getOrElse(ContentType.octetstream)
    def text = IoUtils.toText(in, contenttype.charset)
    def binary = BufferFileBag.fromInputStream(in)
    if (contenttype.mime.isText)
      StringResponse(code, contenttype, text)
    else
      BinaryResponse(code, contenttype, binary)
  }

  def html(p: String): StringResponse = StringResponse(
    200,
    ContentType.html,
    p
  )
}

case class StringResponse(
  code: Int,
  contentType: ContentType,
  content: String
) extends Response {
  def getString = Some(content)
  def getRecord = RAISE.unsupportedOperationFault
  def getRecords = RAISE.unsupportedOperationFault
  def json = RAISE.unsupportedOperationFault
  lazy val show = s"Response(${Strings.cutstring(content, 32)})"
}

case class BinaryResponse(
  code: Int,
  contentType: ContentType,
  content: ChunkBag
) extends Response {
  def getString = None
  def getRecord = None
  def getRecords = None
  def json = RAISE.unsupportedOperationFault
  lazy val show = s"Response(Binary[${content.size}])"
}
