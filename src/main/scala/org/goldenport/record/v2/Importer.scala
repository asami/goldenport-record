package org.goldenport.record.v2

/*
 * @since   Jan. 15, 2017
 * @version Jul. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait Importer {
  def apply(column: Column, src: Record): Option[Any]
}

trait ValueImporter extends Importer {
  def apply(column: Column, src: Record): Option[Any] = {
    src.getField(column.name).flatMap(_.effectiveValue) map {
      case xs: List[_] => applyList(xs)
      case x => apply(x)
    }
  }

  def applyList(xs: List[_]): List[_] = xs.map(apply)

  def apply(p: Any): Any
}

trait ExternalImporter extends Importer {
}

package object importer {
  case object TrimImporter extends ValueImporter {
    def apply(p: Any): Any = p match {
      case m: String => m.trim
      case _ => p
    }
  }

  case object YYYYMMDDImporter extends ValueImporter {
    def apply(p: Any): Any = converter.YYYYMMDDConverter.apply(p)
  }
}
