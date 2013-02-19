package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._

/*
 * @snice   Dec.  8, 2012
 *  version Dec. 12, 2012
 * @version Feb. 20, 2013
 * @author  ASAMI, Tomoharu
 */
case class Column(
  name: String,
  label: String,
  datatype: DataType = XString,
  multiplicity: Multiplicity = MOne,
  constraints: Seq[Constraint] = Nil,
  orderBy: Option[OrderBy] = None,
  visibility: Visibility = PlainVisibility //,
//  operations: Seq[Operation] = Nil,
//  sql: SqlColumn = NullSqlColumn,
//  extjs: Map[String, Any] = Map.empty,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  properties: Map[String, Any] = Map.empty,
//  comment: String = ""
) extends ColumnSlot {
  def toModelField: String = """{"name": "%s", "type": "%s"}""".format(name, datatype.extjsName)

  def toModelValidation: List[String] = {
    List(_validation_presence,
        _validation_length,
        _validation_inclusion,
        _validation_exclusion,
        _validation_format).flatten
  }

  private def _validation_presence: Option[String] = {
    multiplicity match {
      case MOne => """{"field": "%s", "type": "presence"}""".format(name).some
      case _ => None
    }
  }

  private def _validation_length: Option[String] = {
    none
  }

  private def _validation_inclusion: Option[String] = {
    none
  }

  private def _validation_exclusion: Option[String] = {
    none
  }

  private def _validation_format: Option[String] = {
    none
  }

  def isSingle = multiplicity match {
    case MOne => true
    case MZeroOne => true
    case _ => false
  }

  def isMulti = !isSingle
}
