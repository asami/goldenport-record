package org.goldenport.record.v3

import org.w3c.dom._
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.{Schema, Column}

/*
 * @since   Aug. 24, 2018
 *  version Sep.  4, 2018
 *  version Dec. 27, 2018
 * @version May. 27, 2019
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  meta: Table.MetaData = Table.MetaData.empty,
  head: Option[Table.Head] = None,
  foot: Option[Table.Foot] = None
) extends ITable {
  def toTable = this
  def toRecordList: List[Record] = records.toList
  def toRecordVector: Vector[Record] = records
}

object Table {
  import org.goldenport.xml.dom.RichElement.Implicits._

  val empty = Table(Vector.empty)

  case class MetaData(
    schema: Option[Schema]
  ) {
    def getKey(i: Int): Option[Symbol] = schema.
      flatMap(_.columns.lift(i).map(x => Symbol(x.name)))
  }
  object MetaData {
    val empty = MetaData(None)
  }

  case class Cell(content: String, width: Option[Int] = None)

  case class Head(names: List[Cell])

  case class Foot(data: List[Cell])

  def create(p: JsArray): Table = RAISE.notImplementedYetDefect

  def create(p: Node): Table = createOption(p).getOrElse(RAISE.invalidArgumentFault("No table content"))

  def createOption(p: Node): Option[Table] = _create(p)

  private def _create(p: Node): Option[Table] = Option(p) flatMap {
    case m: Element => _create_element(m)
    case m => None
  }

  private def _create_element(p: Element): Option[Table] =
    Option(p.getLocalName()).map(_.toLowerCase).collect {
      case "table" => _create_table(p)
    }

  private def _create_table(p: Element): Table = {
    val head = p.getElementByLocalNameIC("thead").flatMap(_create_head)
    val foot = p.getElementByLocalNameIC("tfoot").flatMap(_create_foot)
    val meta = _make_meta(head)
    val rs = p.getElementByLocalNameIC("tbody").
      map(_create_body(meta, _)).
      getOrElse(_create_body(meta, p))
    Table(rs, meta, head, foot)
  }

  private def _create_head(p: Element): Option[Head] = {
    val trs = _create_trs(p)
    trs.map(Head)
  }

  private def _create_foot(p: Element): Option[Foot] = {
    val trs = _create_trs(p)
    trs.map(Foot)
  }

  private def _create_trs(p: Element): Option[List[Cell]] = {
    val trs = p.elementsByLocalNameIC("tr")
    // TODO more complex
    trs.headOption.map { tr =>
      val ds = tr.elementsByLocalNameIC("th", "td")
      ds.map { x =>
        x.getAttributeOIC("width").
          map(w => Cell(x.getTextContent, Some(w.toInt))).
          getOrElse(Cell(x.getTextContent))
      }
    }
  }

  private def _create_body(meta: MetaData, p: Element): Vector[Record] = {
    for (tr <- p.elementsVectorByLocalNameIC("tr")) yield {
      val fs = for ((td, i) <- tr.elementsVectorByLocalNameIC("th", "td").zipWithIndex) yield {
        val key: Symbol = meta.getKey(i).getOrElse(Symbol(s"${i + 1}"))
        val data = td.getTextContent
        Field.create(key, data)
      }
      Record(fs)
    }
  }

  private def _make_meta(ps: Option[Head]): MetaData =
    ps.map(_make_meta).getOrElse(MetaData.empty)

  private def _make_meta(p: Head): MetaData = {
    val a = p.names.map(x => Column(x.content)) // TODO width
    val s = Schema(a)
    MetaData(Some(s))
  }
}
