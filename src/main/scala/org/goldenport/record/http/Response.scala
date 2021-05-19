package org.goldenport.record.http

import scala.xml._
import java.io.InputStream
import java.nio.charset.Charset
import play.api.libs.json.JsValue
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.{StatusCode, Conclusion}
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
 *  version Sep. 18, 2018
 *  version Oct.  8, 2018
 *  version Apr. 21, 2019
 * @version Feb. 21, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def code: Int
  def contentType: ContentType
  def mime: MimeType = contentType.mime
  def charset: Option[Charset] = contentType.charset
  def getString: Option[String]
  def getBinary: Option[ChunkBag]
  def getRecord: Option[Record]
  def getRecords: Option[List[Record]]
  def json: JsValue
  def show: String
  def isSuccess = code == 200
  def isNotFound = code == 404
  def toStatusCode: StatusCode = StatusCode(code)
  def toConclusion: Conclusion = Conclusion(toStatusCode)
}

object Response {
  def parser(code: Int, header: Map[String, IndexedSeq[String]], in: InputStream): Response = {
    val contenttype = header.get("Content-Type").flatMap(_.headOption.map(ContentType.parse)).getOrElse(ContentType.octetstream)
    def text = {
      contenttype.charset.
        map(IoUtils.toText(in, _)).
        getOrElse(
          if (contenttype.mime.isHtml)
            parseHtml(contenttype, in)
          else if (contenttype.mime.isXml)
            parseXml(contenttype, in)
          else
            IoUtils.toText(in)
        )
    }
    def binary = BufferFileBag.fromInputStream(in)
    if (contenttype.mime.isText)
      StringResponse(code, contenttype, text)
    else
      BinaryResponse(code, contenttype, binary)
  }

  private val _regex_xml = """(?i)[<][?]xml[ ][^?]+(encoding[ ]*[=][ ]*["]([^"]+)["])""".r
  private val _regex_html4 = """(?i)<meta[^>]+content[ ]*=[ ]*["]([^"]+)["]""".r
  private val _regex_html5 = """(?i)<meta[^>]+charset[ ]*=[ ]*["]([^"]+)["]""".r

  def parseHtml(contenttype: ContentType, in: InputStream) = {
    val bag = BufferFileBag.fromInputStream(in)
    val firstpage = bag.toTextFirstPage
    val charset = _regex_html4.findFirstMatchIn(firstpage).
      flatMap(x =>
        ContentType.parse(x.group(1)).charset.map(_.name())
      ).orElse(
        _regex_html5.findFirstMatchIn(firstpage).
          map(_.group(1))
      ).orElse(
        _regex_xml.findFirstMatchIn(firstpage).
          map(_.group(2))
      )
    charset.map(bag.toText(_)).getOrElse(bag.toText)
  }

  def parseXml(contenttype: ContentType, in: InputStream) = {
    val bag = BufferFileBag.fromInputStream(in)
    val firstpage = bag.toTextFirstPage
    val charset = _regex_xml.findFirstMatchIn(firstpage).map(_.group(2))
    charset.map(bag.toText(_)).getOrElse(bag.toText)
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
  def getBinary = None
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
  def getBinary = Some(content)
  def getRecord = None
  def getRecords = None
  def json = RAISE.unsupportedOperationFault
  lazy val show = s"Response(Binary[${content.size}])"
}
