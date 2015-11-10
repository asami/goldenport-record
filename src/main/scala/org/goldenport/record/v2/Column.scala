package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._
import org.smartdox.Description

/*
 * @snice   Dec.  8, 2012
 *  version Dec. 12, 2012
 *  version Feb. 20, 2013
 *  version Mar.  3, 2013
 *  version Oct. 23, 2013
 *  version Jan. 20, 2014
 *  version Jul. 25, 2014
 *  version Aug. 11, 2014
 *  version Oct. 27, 2015
 * @version Nov.  8, 2015
 * @author  ASAMI, Tomoharu
 */
case class Column(
  name: String,
  datatype: DataType = XString,
  multiplicity: Multiplicity = MOne,
  kind: ColumnKind = PlainKind,
  constraints: List[Constraint] = Nil,
//  orderBy: Option[SqlOrder] = None, // displayFormat
  visibility: Visibility = PlainVisibility,
  label: Option[String] = None,
  aliases: List[String] = Nil,
  sql: SqlColumn = NullSqlColumn,
  formatter: Option[Formatter] = None,
  displaySequence: Option[Int] = None, // TODO unify displayFormat
  displayFormat: Option[DisplayFormat] = None,
  desc: Description = Description.empty
//  operations: Seq[Operation] = Nil,
//  extjs: Map[String, Any] = Map.empty,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  properties: Map[String, Any] = Map.empty,
//  comment: String = ""
) extends ColumnSlot {
  def orderBy: Option[SqlOrder] = displayFormat.flatMap(_.orderBy)

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

  def isDerived = sql.isDerived

  def isAcceptColumnName(p: String): Boolean = nameCandidates.contains(p)

  lazy val nameCandidates: Vector[String] =
    Vector(name) ++ aliases ++ label.toVector

  /*
   * Formatter
   */
  def format(value: Option[List[Any]]): String = {
    def otherwise = value match {
      case None => ""
      case Some(xs) => xs.map(datatype.format).mkString(",")
    }
    formatter match {
      case Some(f) => f.format(this, value) getOrElse otherwise
      case None => otherwise
    }
  }
}

object Column {
}

sealed trait ColumnKind {
}
case object PlainKind extends ColumnKind
case object IdKind extends ColumnKind
case object NameKind extends ColumnKind
