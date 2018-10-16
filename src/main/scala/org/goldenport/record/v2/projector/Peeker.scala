package org.goldenport.record.v2.projector

import play.api.libs.json._
import org.goldenport.values.PathName
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.RecordUtils

/*
 * @since   Jul. 19, 2018
 * @version Jul. 23, 2018
 * @author  ASAMI, Tomoharu
 */
trait Peeker {
  def apply(p: Record)(implicit ctx: ProjectorContext): Option[Any]

  protected final def get_value_or_list(name: String, p: Record): Option[Any] =
    p.getField(name).flatMap(_.effectiveValue)
}

object Peeker {
}

case object AttributePeeker extends Peeker {
  def apply(p: Record)(implicit ctx: ProjectorContext): Option[Any] = get_value_or_list(ctx.attributeName, p)
}

case class PathNamePeeker(pathname: PathName) extends Peeker {
  def apply(p: Record)(implicit ctx: ProjectorContext): Option[Any] = {
    val components = pathname.components
    _go(p, components)
  }

  @annotation.tailrec
  private def _go(rec: Record, components: List[String]): Option[Any] =
    components match {
      case Nil => Some(rec)
      case x :: Nil => get_value_or_list(x, rec)
      case x :: xs => get_value_or_list(x, rec) match {
        case Some(s) => s match {
          case m: List[_] => None // TODO
          case m: Record => _go(m, xs)
          case m: JsObject => _go(RecordUtils.js2record(m), xs)
          case m: JsArray => None // TODO
          case m: JsValue => None // TODO
          case m => None // TODO
        }
        case None => None
      }
    }
}
object PathNamePeeker {
  def apply(p: String): PathNamePeeker = PathNamePeeker(PathName(p))
}
