package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.util.AnyUtils
import org.goldenport.record.v2.{Schema, Column, DisplayFormat}

/*
 * @since   Jun. 16, 2019
 *  version Aug. 24, 2019
 *  version Oct.  8, 2019
 * @version Nov. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class TableVisualizer(
  lineStyle: Option[MatrixVisualizer.LineStyle] = None,
  isEmbed: Boolean = false
) {
  def plainText(p: ITable): String =
    (p.head, p.foot) match {
      case (Some(h), Some(f)) => _with_header_footer(h, f, p.data)
      case (Some(h), None) => _with_header(h, p.data)
      case (None, Some(f)) => _with_footer(f, p.data)
      case (None, None) => _simple(p.data)
    }

  private def _simple(p: Table.Data) = {
    val cdefs = _column_defs(p)
    val data = _format_data(p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisLineStyle
    val mv = MatrixVisualizer.border(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    mv.plainText(data.matrix)
  }

  private def _with_header(h: Table.Head, p: Table.Data) = {
    val cdefs = _column_defs(h, p)
    val data = _format_data(h, p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val headervis = MatrixVisualizer.header(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val bodyvis = MatrixVisualizer.bodyEnd(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val a = h.matrix appendRows data.matrix
    val columns = bodyvis.buildColumns(a)
    val rh = headervis.plainText(columns, h.matrix)
    val rb = bodyvis.plainText(columns, data.matrix)
    rh + rb
  }

  private def _with_footer(f: Table.Foot, p: Table.Data) = {
    val cdefs = _column_defs(p)
    val data = _format_data(p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val footervis = MatrixVisualizer.footer(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val bodyvis = MatrixVisualizer.bodyStart(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val a = data.matrix appendRows f.matrix
    val columns = bodyvis.buildColumns(a)
    bodyvis.plainText(columns, data.matrix)
    footervis.plainText(columns, f.matrix)
  }

  private def _with_header_footer(h: Table.Head, f: Table.Foot, p: Table.Data) = {
    val cdefs = _column_defs(h, p)
    val data = _format_data(h, p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val headervis = MatrixVisualizer.header(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val footervis = MatrixVisualizer.body(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val bodyvis = MatrixVisualizer.footer(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs)
    val datamatrix = p.matrix
    val a = h.matrix appendRows datamatrix appendRows f.matrix
    val columns = bodyvis.buildColumns(a)
    headervis.plainText(columns, p.matrix)
    bodyvis.plainText(columns, p.matrix)
    footervis.plainText(columns, p.matrix)
  }

  private def _column_defs(p: Table.Data): MatrixVisualizer.ColumnDefs =
    MatrixVisualizer.ColumnDefs(p.columns.map(x => MatrixVisualizer.ColumnDef.empty))

  private def _column_defs(h: Table.Head, p: Table.Data): MatrixVisualizer.ColumnDefs = {
    val a: List[MatrixVisualizer.ColumnDef] = h.names.map(_get_column_def).zipAll(
      p.columns.map(_get_column_def),
      None,
      None
    ).map {
      case (_, Some(y)) => y
      case (Some(x), None) => x
      case _ => MatrixVisualizer.ColumnDef.empty
    }
    MatrixVisualizer.ColumnDefs(a.toVector)
  }

  private def _get_column_def(p: Table.Cell): Option[MatrixVisualizer.ColumnDef] = None

  private def _get_column_def(c: Column): Option[MatrixVisualizer.ColumnDef] =
    c.displayFormat.flatMap(_get_column_width).map(x => MatrixVisualizer.ColumnDef(Some(x)))

  private def _get_column_width(p: DisplayFormat): Option[Int] = ???


  private def _format_data(p: Table.Data): Table.Data = p

  private def _format_data(h: Table.Head, p: Table.Data): Table.Data = p

  private def _to_string(c: MatrixVisualizer.ColumnDef, p: Table.Cell): String =
    if (isEmbed)
      AnyUtils.toEmbed(p.embed(c.width))
    else
      AnyUtils.toPrint(p.print(c.width))
}
object TableVisualizer {
  val thick = TableVisualizer(MatrixVisualizer.JisThickLineStyle)

  def apply(p: MatrixVisualizer.LineStyle): TableVisualizer = TableVisualizer(Some(p))
}
