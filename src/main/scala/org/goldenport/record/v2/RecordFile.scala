package org.goldenport.record.v2

import java.net.URL
import org.goldenport.Strings
import com.asamioffice.goldenport.io.UURL
import com.asamioffice.goldenport.text.UPathString

/**
 * derived from Record.
 * 
 *  since   Jun.  9, 2010
 * @since   Mar. 10, 2013
 * @version Mar. 10, 2013
 * @author  ASAMI, Tomoharu
 */
trait InputFile {
  def name: String
  def key: String
  def filename: String

  def getUrl: Option[URL] = None
} 

case class UrlInputFile(
  name: String,
  key: String, 
  url: URL
) extends InputFile {
  def filename = UPathString.getLastComponent(url.toString)

  override def getUrl = Some(url)
}

object InputFile {
  def createByUrlString(
    name: String,
    key: String,
    path: String): UrlInputFile = {
    UrlInputFile(name, key, UURL.getURLFromFileOrURLName(path))
  }
}

case class UploadFile(key: String, uri: String)

/*
import play.api.mvc._
import play.api.libs.Files.TemporaryFile

case class MultipartFormDataInputFile(file: MultipartFormData.FilePart[TemporaryFile]) extends InputFile {
}
*/
