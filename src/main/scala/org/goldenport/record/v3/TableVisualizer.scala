package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.util.AnyUtils

/*
 * @since   Jun. 16, 2019
 * @version Jun. 23, 2019
 * @author  ASAMI, Tomoharu
 */
case class TableVisualizer(
  lineStyle: Option[MatrixVisualizer.LineStyle] = None
) {
  def plainText(p: ITable): String =
    (p.head, p.foot) match {
      case (Some(h), Some(f)) => _header_footer(h, f, p.data)
      case (Some(h), None) => _header(h, p.data)
      case (None, Some(f)) => _footer(f, p.data)
      case (None, None) => _simple(p.data)
    }

  private def _simple(p: Table.Data) = {
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisLineStyle
    val mv = MatrixVisualizer.border(_to_string).withLineStyle(linestyle)
    mv.plainText(p.matrix)
  }

  private def _header(h: Table.Head, p: Table.Data) = {
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val headervis = MatrixVisualizer.header(_to_string).withLineStyle(linestyle)
    val bodyvis = MatrixVisualizer.bodyEnd(_to_string).withLineStyle(linestyle)
    val a = h.matrix appendRows p.matrix
    val columns = bodyvis.buildColumns(a)
    val rh = headervis.plainText(columns, h.matrix)
    val rb = bodyvis.plainText(columns, p.matrix)
    rh + rb
  }

  private def _footer(f: Table.Foot, p: Table.Data) = {
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val footervis = MatrixVisualizer.footer(_to_string).withLineStyle(linestyle)
    val bodyvis = MatrixVisualizer.bodyStart(_to_string).withLineStyle(linestyle)
    val a = p.matrix appendRows f.matrix
    val columns = bodyvis.buildColumns(a)
    bodyvis.plainText(columns, p.matrix)
    footervis.plainText(columns, p.matrix)
  }

  private def _header_footer(h: Table.Head, f: Table.Foot, p: Table.Data) = {
    val linestyle = lineStyle getOrElse MatrixVisualizer.JisThickLineStyle
    val headervis = MatrixVisualizer.header(_to_string).withLineStyle(linestyle)
    val footervis = MatrixVisualizer.body(_to_string).withLineStyle(linestyle)
    val bodyvis = MatrixVisualizer.footer(_to_string).withLineStyle(linestyle)
    val datamatrix = p.matrix
    val a = h.matrix appendRows datamatrix appendRows f.matrix
    val columns = bodyvis.buildColumns(a)
    headervis.plainText(columns, p.matrix)
    bodyvis.plainText(columns, p.matrix)
    footervis.plainText(columns, p.matrix)
  }

  private def _to_string(p: Table.Cell): String = AnyUtils.toString(p.content)
}
object TableVisualizer {
  def thick = TableVisualizer(MatrixVisualizer.JisThickLineStyle)

  def apply(p: MatrixVisualizer.LineStyle): TableVisualizer = TableVisualizer(Some(p))
}
