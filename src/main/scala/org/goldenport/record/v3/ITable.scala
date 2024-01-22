package org.goldenport.record.v3

import org.w3c.dom._
import org.goldenport.exception.RAISE
import org.goldenport.matrix.IMatrix
import org.goldenport.record.v2.{Schema => Schema2, XmlSchema, Column => V2Column}
import org.goldenport.values.{NumberRange, EnumRange}

/*
 * @since   Sep.  2, 2018
 *  version Dec. 27, 2018
 *  version Feb. 11, 2019
 *  version Jun. 23, 2019
 *  version Jul. 26, 2019
 *  version Aug. 23, 2019
 *  version Sep. 16, 2019
 *  version Jan. 26, 2020
 *  version Feb. 28, 2020
 *  version Mar. 25, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
trait ITable extends org.goldenport.table.ITable {
  def toTable: Table
  def schema: Schema2
  def meta: Table.MetaData // TODO
  def head: Option[Table.Head] // TODO
  def foot: Option[Table.Foot] // TODO
  def data: Table.Data
  def width: Int = data.width
  def height: Int = data.height
  def toRecordList: List[Record]
  def toRecordVector: Vector[Record]
  def toVectorVector: Vector[Vector[Any]] = data.matrix.rowVector.map(_.map(_.content))
  def filter(f: Record => Boolean): ITable
  def select(names: Seq[String]): ITable
  def select(p: NumberRange): ITable
}

object ITable {
  private val _table_schema = {
    val columns = Vector()
    Schema2(columns, xml = XmlSchema("table"))
  }

  private val _thead_schema = {
    val columns = Vector()
    Schema2(columns, xml = XmlSchema("thead"))
  }

  private val _tbody_schema = {
    val columns = Vector()
    Schema2(columns, xml = XmlSchema("tbody"))
  }

  private val _tfoot_schema = {
    val columns = Vector()
    Schema2(columns, xml = XmlSchema("tfoot"))
  }

  private val _tr_schema = {
    val columns = Vector()
    Schema2(columns, xml = XmlSchema("tr"))
  }

  private val _th_column = V2Column("th", xml = V2Column.Xml.element)
  private val _td_column = V2Column("td", xml = V2Column.Xml.element)

  case class HtmlBuilder() {
    def dom(p: ITable): Element = record(p)

    def text(p: ITable): String = record(p).toXmlString

    def record(p: ITable): Record = {
      val xs = Vector(_thead(p), _tbody(p), _tfoot(p)).flatten
      _element(_table_schema, xs)
    }

    private def _thead(p: ITable): Option[Field] = p.head.map { h =>
      val xs = for (c <- h.names) yield {
        val d = c.text
        Field.create("th", d, _th_column)
      }
      val tr = Field.create("tr", _element(_tr_schema, xs))
      Field.create("thead", _element(_thead_schema, Vector(tr)))
    }

    private def _tbody(p: ITable): Option[Field] = {
      // data.columns
      val xs = for (row <- p.data.matrix.rowIterator) yield {
        val a = for (c <- row) yield {
          val d = c.text
          Field.create("td", d, _td_column)
        }
        Field.create("tr", _element(_tr_schema, a))
      }
      val r = Field.create("tbody", _element(_tbody_schema, xs.toVector))
      Some(r)
    }

    private def _tfoot(p: ITable): Option[Field] = p.foot.map { f =>
      val xs = for (c <- f.data) yield {
        val d = c.text
        Field.create("th", d, _th_column)
      }
      Field.create("tfoot", _element(_tbody_schema, xs.toVector))
    }

    private def _element(schema: Schema2, fields: Seq[Field]) =
      Record(fields, Record.MetaData(Some(schema)))
  }
}
