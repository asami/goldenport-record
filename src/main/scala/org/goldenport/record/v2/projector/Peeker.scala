package org.goldenport.record.v2.projector

import org.goldenport.values.PathName
import org.goldenport.record.v2._

/*
 * @since   Jul. 19, 2018
 * @version Jul. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait Peeker {
  def apply(p: Record): Option[Any]
}

object Peeker {
}

case class PathNamePeeker(pathname: PathName) extends Peeker {
  def apply(p: Record): Option[Any] = {
    val components = pathname.components
    _go(p, components)
  }

  @annotation.tailrec
  private def _go(rec: Record, components: List[String]): Option[Any] =
    components match {
      case Nil => None
      case x :: Nil => rec.getField(x).map(_.effectiveValue)
      case x :: xs => rec.getField(x).flatMap(_.effectiveValue) match {
        case Some(s) => s match {
          case m: List[_] => None // TODO
          case m: Record => _go(m, xs)
          case m => None // TODO
        }
        case None => None
      }
    }
}
object PathNamePeeker {
  def apply(p: String): PathNamePeeker = PathNamePeeker(PathName(p))
}
