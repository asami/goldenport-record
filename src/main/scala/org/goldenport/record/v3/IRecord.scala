package org.goldenport.record.v3

import java.util.Date
import java.sql.Timestamp
import play.api.libs.json._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 23, 2018
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
trait IRecord {
  def toRecord: Record
  def fields: Seq[Field]
  def isEmpty: Boolean
  def isDefined(key: String): Boolean
  def isDefined(key: Symbol): Boolean
  def get(key: String): Option[Any]
  def get(key: Symbol): Option[Any]
  def getString(key: String): Option[String]
  def getString(key: Symbol): Option[String]
  def getInt(key: String): Option[Int]
  def getInt(key: Symbol): Option[Int]
  def getRecord(key: String): Option[Record]
  def getRecord(key: Symbol): Option[Record]
  def getRecordList(key: String): List[Record]
  def getRecordList(key: Symbol): List[Record]
  def +(rhs: IRecord): IRecord

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
