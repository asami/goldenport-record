package org.goldenport.record.v2

import scalaz._, Scalaz._
import java.net.URI

/*
 * @since   Dec. 28, 2012
 *  version Dec. 31, 2012
 *  version Jan.  9, 2013
 * @version Mar.  3, 2013
 * @author  ASAMI, Tomoharu
 */
case class SqlSchema(actions: Seq[SqlAction]) {
  def useId = actions.exists(_.useId)
}

object NullSqlSchema extends SqlSchema(Nil)

case class UploadFile(key: String, uri: String)

trait InputFile {
  def name: String
  def key: String
  def filename: String
}

/*
import play.api.mvc._
import play.api.libs.Files.TemporaryFile

case class MultipartFormDataInputFile(file: MultipartFormData.FilePart[TemporaryFile]) extends InputFile {
}
*/
