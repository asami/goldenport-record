package org.goldenport.record.v3

import org.goldenport.values.PathName

/*
 * @since   Dec. 12, 2022
 * @version Dec. 12, 2022
 * @author  ASAMI, Tomoharu
 */
trait PathNamePart { self: Record =>
  def getField(path: PathName): Option[Field] = {
    def _go_(path: List[String], fields: List[Field]): Option[Field] = path match {
      case Nil => None
      case x :: Nil => fields.find(_.key.name == x)
      case x :: xs => fields.find(_.key.name == x).flatMap(_.getValue).flatMap {
        case m: Record => _go_(xs, m.fields.toList)
        case _ => None
      }
    }
    _go_(path.components, fields.toList)
  }

  def getFieldValue(path: PathName): Option[FieldValue] = getField(path).map(_.value)

  def getValue(path: PathName): Option[Any] = getFieldValue(path).flatMap(_.getValue)

  def getString(path: PathName): Option[String] = getFieldValue(path).map(_.asString)
}
