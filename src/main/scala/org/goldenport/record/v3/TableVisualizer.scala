package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.util.AnyUtils
import org.goldenport.record.v2.{Schema, Column => Column2, DisplayFormat}

/*
 * @since   Jun. 16, 2019
 *  version Aug. 24, 2019
 *  version Oct.  8, 2019
 *  version Nov. 16, 2019
 * @version Mar. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class TableVisualizer(
  lineStyle: Option[MatrixVisualizer.LineStyle] = None,
  isCompact: Boolean = false,
  isEmbed: Boolean = false
) {
  private val _console_width = 80

  def plainText(p: ITable): String =
    (p.head, p.foot) match {
      case (Some(h), Some(f)) => _with_header_footer(h, f, p.data)
      case (Some(h), None) => _with_header(h, p.data)
      case (None, Some(f)) => _with_footer(f, p.data)
      case (None, None) => _simple(p.data)
    }

  private def _simple(pp: Table.Data) = {
    val p = if (isCompact) pp.trim else pp
    val cdefs = _column_defs(p)
    val data = _format_data(p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisLineStyle
    val mv = MatrixVisualizer.border(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val a = data.matrix
    val columns = _build_columns(mv, a)
    mv.plainText(columns, data.matrix)
  }

  private def _with_header(ph: Table.Head, pp: Table.Data) = {
    val h = if (isCompact) ph.trim else ph
    val p = if (isCompact) pp.trim else pp
    val cdefs = _column_defs(h, p)
    val data = _format_data(h, p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val headervis = MatrixVisualizer.header(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val bodyvis = MatrixVisualizer.bodyEnd(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val a = h.matrix appendRows data.matrix
    val columns = _build_columns(bodyvis, a)
    val rh = headervis.plainText(columns, h.matrix)
    val rb = bodyvis.plainText(columns, data.matrix)
    rh + rb
  }

  private def _with_footer(pf: Table.Foot, pp: Table.Data) = {
    val f = if (isCompact) pf.trim else pf
    val p = if (isCompact) pp.trim else pp
    val cdefs = _column_defs(p)
    val data = _format_data(p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val footervis = MatrixVisualizer.footer(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val bodyvis = MatrixVisualizer.bodyStart(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val a = data.matrix appendRows f.matrix
    val columns = _build_columns(bodyvis, a)
    val rb = bodyvis.plainText(columns, data.matrix)
    val rf = footervis.plainText(columns, f.matrix)
    rb + rf
  }

  private def _with_header_footer(ph: Table.Head, pf: Table.Foot, pp: Table.Data) = {
    val h = if (isCompact) ph.trim else ph
    val f = if (isCompact) pf.trim else pf
    val p = if (isCompact) pp.trim else pp
    val cdefs = _column_defs(h, p)
    val data = _format_data(h, p)
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val headervis = MatrixVisualizer.header(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val footervis = MatrixVisualizer.body(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val bodyvis = MatrixVisualizer.footer(_to_string).withLineStyle(linestyle).withColumnDefs(cdefs).withCompact(isCompact)
    val datamatrix = p.matrix
    val a = h.matrix appendRows datamatrix appendRows f.matrix
    val columns = _build_columns(bodyvis, a)
    val rh = headervis.plainText(columns, p.matrix)
    val rb = bodyvis.plainText(columns, p.matrix)
    val rf = footervis.plainText(columns, p.matrix)
    rh + rb + rf
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

  // private def _adjust(p: MatrixVisualizer.ColumnInfos) =
  //   if (isCompact)
  //     _adjust_compact(p)
  //   else
  //     p

  // private def _adjust_compact(p: MatrixVisualizer.ColumnInfos) =
  //   MatrixVisualizer.compact(_console_width, p)

  private def _get_column_def(p: Table.Cell): Option[MatrixVisualizer.ColumnDef] = None

  private def _get_column_def(c: Column2): Option[MatrixVisualizer.ColumnDef] =
    c.displayFormat.flatMap(_get_column_width).map(x => MatrixVisualizer.ColumnDef(Some(x)))

  private def _get_column_width(p: DisplayFormat): Option[Int] = ???

  private def _format_data(p: Table.Data): Table.Data =
    if (isCompact)
      p.trim
    else
      p

  private def _format_data(h: Table.Head, p: Table.Data): Table.Data =
    if (isCompact)
      p.trim
    else
      p

  private def _to_string(c: MatrixVisualizer.ColumnDef, p: Table.Cell): String = {
    val a = if (isEmbed)
      AnyUtils.toEmbed(p.embed(c.width))
    else
      AnyUtils.toPrint(p.print(c.width))
    if (isCompact)
      a
    else
      a
  }

  private def _build_columns[T](vis: MatrixVisualizer[T], p: IMatrix[T]) =
    if (isCompact)
      vis.buildColumnsCompact(_console_width, p)
    else
      vis.buildColumns(p)
}
object TableVisualizer {
  val thick = TableVisualizer(MatrixVisualizer.JisThickLineStyle)

  def apply(p: MatrixVisualizer.LineStyle): TableVisualizer = TableVisualizer(Some(p))
}
