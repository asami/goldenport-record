package org.goldenport.record.v2

import org.goldenport.values.PathName
import org.goldenport.record.util.AnyUtils

/*
 * @since   Dec. 12, 2022
 * @version Dec. 12, 2022
 * @author  ASAMI, Tomoharu
 */
trait PathNamePart { self: Record =>
  // to_natural_value
  protected final def _get_value(field: Field): Option[Any] = field.values match {
    case Nil => None
    case List(x) => Some(x)
    case xs => Some(xs)
  }

  def getField(path: PathName): Option[Field] = {
    def _go_(path: List[String], fields: List[Field]): Option[Field] = path match {
      case Nil => None
      case x :: Nil => fields.find(_.key.name == x)
      case x :: xs => fields.find(_.key.name == x).flatMap(_get_value).flatMap {
        case m: Record => _go_(xs, m.fields)
        case _ => None
      }
    }
    _go_(path.components, fields)
  }

  def getValue(path: PathName): Option[Any] = getField(path).flatMap(_get_value)

  def getString(path: PathName): Option[String] = getValue(path).map(AnyUtils.toString)
}
