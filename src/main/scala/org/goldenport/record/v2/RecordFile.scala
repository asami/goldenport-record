package org.goldenport.record.v2

import java.io._
import java.net.URL
import org.goldenport.Strings
import com.asamioffice.goldenport.io.UURL
import com.asamioffice.goldenport.text.UPathString

/**
 * derived from Record.
 * 
 *  since   Jun.  9, 2010
 * @since   Mar. 10, 2013
 * @version Mar. 12, 2013
 * @author  ASAMI, Tomoharu
 */
trait InputFile {
  def name: String
  def key: String
  def filename: String
  def getUrl: Option[URL] = None
  def createWorkFile(): WorkFile
} 

case class UrlInputFile(
  name: String,
  key: String, 
  url: URL
) extends InputFile {
  def filename = UPathString.getLastComponent(url.toString)

  override def getUrl = Some(url)

  def createWorkFile() = new UrlWorkFile(url)
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

trait WorkFile {
  def length
  def openStream(): InputStream
  def dispose() {}
}

class UrlWorkFile(url: URL) extends WorkFile {
  private var _file: File = null

  private def _ensure() {
    if (_file == null) {
      _file = File.createTempFile("ef", "tmp")
      val in = url.openStream
      try {
        val out = new FileOutputStream(_file)
        val buf = new Array[Byte](8192)
        var n = in.read(buf)
        while (n > 0) {
          out.write(buf, 0, n)
          n = in.read(buf)
        }
        out.close
      } finally {
        in.close
      }
    }
  }

  def length = {
    _ensure()
    _file.length
  }

  def openStream() = {
    if (_file != null) {
      new FileInputStream(_file)
    } else {
      url.openStream
    }
  }

  override def dispose() {
    if (_file != null) {
      _file.delete()
      _file = null
    }
  }
}

class FileWorkFile(file: File) extends WorkFile {
  def length = file.length

  def openStream() = {
    new FileInputStream(file)
  }
}

/*
import play.api.mvc._
import play.api.libs.Files.TemporaryFile

case class MultipartFormDataInputFile(file: MultipartFormData.FilePart[TemporaryFile]) extends InputFile {
}
*/
