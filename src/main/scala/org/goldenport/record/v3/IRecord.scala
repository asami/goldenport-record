package org.goldenport.record.v3

import java.util.Date
import java.net.{URL, URI}
import java.sql.Timestamp
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.util.AnyUtils

/*
 * @since   Aug. 23, 2018
 *  version Sep. 20, 2018
 *  version Oct. 30, 2018
 *  version Nov.  7, 2018
 *  version Jan.  6, 2019
 * @version Feb. 28, 2019
 * @author  ASAMI, Tomoharu
 */
trait IRecord extends org.goldenport.record.IRecord
    with org.w3c.dom.Element with DomPart {
  def toRecord: Record
  def fields: Seq[Field]
  def isEmpty: Boolean
  def isDefined(key: Symbol): Boolean
  def isDefined(key: String): Boolean
  def get(key: Symbol): Option[Any]
  def get(key: String): Option[Any]
  def getField(key: Symbol): Option[Field] = fields.find(_.key == key)
  def getField(key: String): Option[Field] = fields.find(_.name == key)
  def getList(key: Symbol): Option[List[Any]]
  def getList(key: String): Option[List[Any]]
  def takeList(key: Symbol): List[Any] = getList(key) getOrElse Nil
  def takeList(key: String): List[Any] = getList(key) getOrElse Nil
  def getString(key: Symbol): Option[String] = get(key).map(AnyUtils.toString)
  def getString(key: String): Option[String] = get(key).map(AnyUtils.toString)
  def getStringList(key: Symbol): Option[List[String]] = getList(key).map(_.map(AnyUtils.toString))
  def getStringList(key: String): Option[List[String]] = getList(key).map(_.map(AnyUtils.toString))
  def takeStringList(key: Symbol): List[String] = takeList(key).map(AnyUtils.toString)
  def takeStringList(key: String): List[String] = takeList(key).map(AnyUtils.toString)
  def getInt(key: Symbol): Option[Int] = get(key).map(AnyUtils.toInt)
  def getInt(key: String): Option[Int] = get(key).map(AnyUtils.toInt)
  def getTimestamp(key: Symbol): Option[Timestamp] = get(key).map(AnyUtils.toTimestamp)
  def getTimestamp(key: String): Option[Timestamp] = get(key).map(AnyUtils.toTimestamp)
  def getDate(key: Symbol): Option[Date] = get(key).map(AnyUtils.toDate)
  def getDate(key: String): Option[Date] = get(key).map(AnyUtils.toDate)
  def getUrl(key: Symbol): Option[URL] = get(key).map(AnyUtils.toUrl)
  def getUrl(key: String): Option[URL] = get(key).map(AnyUtils.toUrl)
  def getUri(key: Symbol): Option[URI] = get(key).map(AnyUtils.toUri)
  def getUri(key: String): Option[URI] = get(key).map(AnyUtils.toUri)
  def getIRecord(key: Symbol): Option[IRecord] = getRecord(key)
  def getIRecord(key: String): Option[IRecord] = getRecord(key)
  def getRecord(key: Symbol): Option[Record]
  def getRecord(key: String): Option[Record]
  def takeRecordList(key: Symbol): List[Record]
  def takeRecordList(key: String): List[Record]
  def +(rhs: IRecord): IRecord // XXX semantics
  def update(rhs: IRecord): IRecord = this.+(rhs) // XXX semantics

  def asNameStringVector: Vector[(String, String)] = RAISE.notImplementedYetDefect
  def asSymbolAnyVector: Vector[(Symbol, Any)] = RAISE.notImplementedYetDefect
  def asJson: JsObject = RAISE.notImplementedYetDefect

  // compatibility
  // def getOne(key: String): Option[Any] = RAISE.unsupportedOperationFault
  // def getOne(key: Symbol): Option[Any] = RAISE.unsupportedOperationFault
  // def getFormOne(key: String): Option[Any] = RAISE.unsupportedOperationFault
  // def getFormOne(key: Symbol): Option[Any] = RAISE.unsupportedOperationFault
  // def getConcreteString(key: String): Option[String] = RAISE.unsupportedOperationFault
  // def getConcreteString(key: Symbol): Option[String] = RAISE.unsupportedOperationFault
  // def getFormString(key: String): Option[String] = RAISE.unsupportedOperationFault
  // def getFormString(key: Symbol): Option[String] = RAISE.unsupportedOperationFault
  // def getFormTimestamp(key: String): Option[Timestamp] = RAISE.unsupportedOperationFault
  // def getFormTimestamp(key: Symbol): Option[Timestamp] = RAISE.unsupportedOperationFault
  // def getFormDate(key: String): Option[Date] = RAISE.unsupportedOperationFault
  // def getFormDate(key: Symbol): Option[Date] = RAISE.unsupportedOperationFault
  // def eagerStringList(key: String): List[String] = RAISE.unsupportedOperationFault
  // def effectiveList(key: String): List[Any] = RAISE.unsupportedOperationFault
  // def toStringVector: Vector[(String, String)] = RAISE.unsupportedOperationFault
}

object IRecord {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import org.goldenport.json.JsonUtils.Implicits._
  import org.goldenport.record.v2.util.RecordUtils

  implicit object IRecordFormat extends Format[IRecord] {
    def reads(json: JsValue): JsResult[IRecord] = json match {
      case m: JsObject => JsSuccess(Record.create(m))
      case _ => JsError(s"Invalid Record($json)")
    }
    def writes(o: IRecord): JsValue = o.asJson
  }
}
