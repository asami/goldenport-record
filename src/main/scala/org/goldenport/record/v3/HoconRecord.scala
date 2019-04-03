package org.goldenport.record.v3

import scala.collection.JavaConverters._
import com.typesafe.config._
import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig

/*
 * @since   Oct. 21, 2018
 * @version Mar. 24, 2019
 * @author  ASAMI, Tomoharu
 */
case class HoconRecord(hocon: RichConfig) extends IRecord {
  lazy val toRecord: Record = Record(fields)
  lazy val keyNames: List[String] = fields.map(_.name).toList
  lazy val fields: Seq[Field] = hocon.config.entrySet.asScala.toVector.map { x =>
    val key = x.getKey
    val value = x.getValue match {
      case m: ConfigList => RAISE.notImplementedYetDefect
      case m: ConfigObject => RAISE.notImplementedYetDefect
      case m: ConfigValue => SingleValue(m.unwrapped)
    }
    Field(key, value)
  }
  def isEmpty: Boolean = fields.isEmpty
  def isDefined(key: String): Boolean = hocon.isDefined(key)
  def isDefined(key: Symbol): Boolean = isDefined(key.name)
  def get(key: String): Option[Any] = hocon.getStringOption(key)
  def get(key: Symbol): Option[Any] = get(key.name)
  def getList(key: String): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getList(key: Symbol): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getRecord(key: String): Option[Record] = RAISE.notImplementedYetDefect
  def getRecord(key: Symbol): Option[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: String): List[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: Symbol): List[Record] = RAISE.notImplementedYetDefect
  def +(rhs: IRecord): IRecord = RAISE.notImplementedYetDefect
}
