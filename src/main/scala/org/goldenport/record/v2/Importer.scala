package org.goldenport.record.v2

import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.json.JsonUtils
import org.goldenport.record.v2.projector.ProjectorContext

/*
 * @since   Jan. 15, 2017
 *  version Jul. 28, 2018
 *  version Sep.  5, 2018
 * @version Oct. 16, 2018
 * @author  ASAMI, Tomoharu
 */
trait Importer {
  def apply(column: Column, src: Record): Option[Any]
  def apply(ctx: ProjectorContext, column: Column, src: Record): Option[Any] = apply(column, src)
  def toJson: JsValue = RAISE.unsupportedOperationFault
  def marshall: String = RAISE.unsupportedOperationFault
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

case class ImporterFactory(
  importers: Vector[ImporterClass]
) {
  def unmarshall(p: JsLookupResult): Option[Importer] = p match {
    case JsDefined(js) => unmarshall(js)
    case m: JsUndefined => None
  }

  def unmarshall(p: JsValue): Option[Importer] = {
    // println(s"=> ImporterFactory#unmarshall(${importers}: $p")
    val r = importers.toStream.flatMap(_.unmarshall(p)).headOption
    // println(s"<= ImporterFactory#unmarshall: $r")
    r
  }
}
object ImporterFactory {
  def apply(p: ImporterClass, ps: ImporterClass*): ImporterFactory = ImporterFactory(p +: ps.toVector)
}

trait ImporterClass {
  def unmarshall(p: JsLookupResult): Option[Importer] = p match {
    case JsDefined(js) => unmarshall(js)
    case m: JsUndefined => None
  }

  def unmarshall(p: JsValue): Option[Importer] = {
    p match {
      case JsString(name) => unmarshall(name)
      case m: JsObject => JsonUtils.getString(m, "name").flatMap(unmarshall(_, m))
      case _  => None
    }
  }

  def unmarshall(name: String): Option[Importer] = unmarshall(name, JsObject(Nil))
  def unmarshall(name: String, p: JsObject): Option[Importer]
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
