package org.goldenport.record.v2.bag

import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.concurrent.Task
import scalaz.stream._
import scalax.io._
import scala.collection.JavaConverters._
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.xssf.usermodel.{
  XSSFWorkbook, XSSFCellStyle, XSSFColor
}
import org.apache.poi.ss.usermodel._
import com.asamioffice.goldenport.io.UURL
import org.goldenport.Platform
import org.goldenport.Strings
import org.goldenport.values.FileName
import org.goldenport.bag._
import org.goldenport.record.v2._
import org.goldenport.record.v2.bag.RecordBag._
import org.goldenport.record.v2.util.{TupleUtils, RecordUtils}
import org.goldenport.util.StringUtils

/*
 * @since   Sep. 21, 2015
 *  version Oct. 28, 2015
 *  version Sep. 22, 2016
 *  version Sep. 21, 2017
 * @version Aug.  5, 2019
 * @author  ASAMI, Tomoharu
 */
class ExcelBag(
  val kind: ExcelBag.ExcelKind = ExcelBag.Xlsx,
  val bag: org.goldenport.bag.FileBag = new org.goldenport.bag.FileBag(),
  val strategy: RecordBag.Strategy = RecordBag.Strategy.plainAuto,
  override val name: String = "data",
  val sheetNumber: Int = 0,
  val sheetName: Option[String] = None, // filenameBody
  val properties: ExcelBag.Properties = ExcelBag.Properties.empty
) extends RecordBag with WorkbookFeature {
  import ExcelBag._

  type WORKBOOK = Workbook
  type SHEET = Sheet

  // def chunkBag = {
  //   _ensure_external
  //   bag
  // }
  override def filenameSuffix = Some(kind.suffix)
  override def mimetype: String = Strings.mimetype.application_excel
  override def getCodec = Some(Platform.codec.WINDOWS31J)

  def dispose() {
    workbook_dispose_state()
    bag.dispose()
  }

  protected def generate_Schema() = {
    View(strategy, chunkBag, _sheet).getSchema
  }

  protected def new_Workbook(): Workbook = {
    kind match {
      case Xls => new HSSFWorkbook()
      case Xlsx => new XSSFWorkbook()
    }
  }

  protected def save_Workbook(workbook: Workbook) {
    bag.write(workbook.write(_))
  }

  protected def load_Workbook(): Workbook = {
    WorkbookFactory.create(bag.toFile)
  }

  protected def dispose_Workbook(workbook: Workbook) {
    workbook.close()
  }


  def withSchema(s: Schema) = {
//    new ExcelBag(bag, s).asInstanceOf[this.type]
    ???
  }

  def recordsR: Process[Task, Record] = {
    View(strategy, chunkBag, _sheet).recordsR
  }

  private def _sheet: Sheet = {
    try {
      val r = to_workbook.getSheetAt(sheetNumber)
      if (r == null)
        _create_sheet
      else
        r
    } catch {
      case e: IllegalArgumentException => _create_sheet
    }
  }

  private def _create_sheet: Sheet = {
    val r = to_workbook.createSheet(sheetName getOrElse name)
    workbook_touch()
    r
  }

  private def _create_row(i: Int): Row = {
    val r = _sheet.createRow(i)
    workbook_touch()
    r
  }

  private def _complement_physical_header: Boolean = {
    if (strategy.headerPolicy.isComplementPhysicalHeader) {
      _build_header
      true
    } else {
      false
    }
  }

  private def _build_header {
    val row = _create_row(0)
    for ((label, i) <- TupleUtils.makeHeader(schema).zipWithIndex) {
      val cell = row.createCell(i)
      cell.setCellValue(label)
      _get_column(i).map(_set_header_format(_, cell))
    }
    _set_split_window
  }

  private def _get_column(i: Int): Option[Column] = {
    getSchema.flatMap(_.columns.lift(i))
  }

  private lazy val _default_header_cellstyle = {
    val style = to_workbook.createCellStyle()
    _set_border(style)
    _set_align_center(style)
    _set_header_font(style)
//    _set_background_color(style, 0x87ceeb) // skyblue
    _set_background_color(style, 0x7fffd4) // aquamarine
    style
  }

  private lazy val _default_body_cellstyle = {
    val style = to_workbook.createCellStyle()
    _set_border(style)
    style
  }

  private var _header_cellstyles = Map.empty[Int, CellStyle]
  private var _body_cellstyles = Map.empty[Int, CellStyle]

  private def _header_cellstyle_with_rgb(rgb: Int) = {
    _header_cellstyles.get(rgb) getOrElse {
      val style = to_workbook.createCellStyle()
      _set_border(style)
      _set_align_center(style)
      _set_header_font(style)
      _set_background_color(style, rgb)
      _header_cellstyles = _header_cellstyles + (rgb -> style)
      style
    }
  }

  private def _body_cellstyle_with_rgb(rgb: Int) = {
    _body_cellstyles.get(rgb) getOrElse {
      val style = to_workbook.createCellStyle()
      _set_border(style)
      _set_background_color(style, rgb)
      _body_cellstyles = _body_cellstyles + (rgb -> style)
      style
    }
  }

  private lazy val _header_font = {
    val font = to_workbook.createFont()
    font.setFontName("ＭＳ Ｐゴシック")
    font.setBold(true)
    font
  }

  private def _set_border(style: CellStyle) {
    style.setBorderBottom(CellStyle.BORDER_THIN);
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderLeft(CellStyle.BORDER_THIN)
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderRight(CellStyle.BORDER_THIN)
    style.setRightBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderTop(CellStyle.BORDER_THIN)
    style.setTopBorderColor(IndexedColors.BLACK.getIndex)
  }

  private def _set_align_center(style: CellStyle) {
    style.setAlignment(CellStyle.ALIGN_CENTER)
  }

  private def _set_header_font(style: CellStyle) {
    style.setFont(_header_font)
  }

  private def _set_background_color(style: CellStyle, rgb: Int) {
    style match {
      case m: XSSFCellStyle => _set_background_color(m, rgb)
      case _ => ()
    }
  }

  private def _set_background_color(style: XSSFCellStyle, rgb: Int) {
    val c = new java.awt.Color(rgb)
    val color = new XSSFColor(c)
    style.setFillPattern(CellStyle.SOLID_FOREGROUND)
    style.setFillForegroundColor(color)
    style.setFillBackgroundColor(color)
  }

  protected def is_ruled_line = properties.ruledLine | false
  protected def is_fixed_header = properties.fixedHeader| false

  private def _header_cellstyle(c: Column) = {
    c.displayFormat.flatMap(_.header.backgroundColorRgb) match {
      case Some(rgb) => _header_cellstyle_with_rgb(rgb)
      case None => _default_header_cellstyle
    }
  }

  private def _body_cellstyle(c: Column) = {
    c.displayFormat.flatMap(_.body.backgroundColorRgb) match {
      case Some(rgb) => _body_cellstyle_with_rgb(rgb)
      case None => _default_body_cellstyle
    }
  }

  private def _set_header_format(c: Column, cell: Cell): Cell = {
    if (is_ruled_line) {
      cell.setCellStyle(_header_cellstyle(c))
    }
    cell
  }

  private def _set_body_format(c: Column, cell: Cell): Cell = {
    if (is_ruled_line) {
      cell.setCellStyle(_body_cellstyle(c))
    }
    cell
  }

  private def _set_split_window {
    if (is_fixed_header)
      _sheet.createFreezePane(0, 1)
  }

  private def _to_celltype(c: Column) = {
    Cell.CELL_TYPE_STRING
  }

  private def _acquire = Task.delay {
    workbook_ensure_internal
    if (_complement_physical_header)
      new IntIndex(1)
    else
      new IntIndex(0)
  }
  private def _release(index: IntIndex) = Task.now(())

  def recordW: Sink[Task, Record] = {
    def execute(index: IntIndex) = Task.now((rec: Record) => Task delay {
      val row = index.apply()
      getSchema.cata(
        _write_with_schema(_, row, rec),
        _write_without_schema(row, rec))
    })
    io.resource(_acquire)(_release)(execute)
  }

  def recordsW: Sink[Task, Seq[Record]] = {
    def execute(index: IntIndex) = Task.now((rs: Seq[Record]) => Task delay {
      for (rec <- rs) {
        val row = index.apply()
        getSchema.cata(
          _write_with_schema(_, row, rec),
          _write_without_schema(row, rec))
      }
    })
    io.resource(_acquire)(_release)(execute)
  }

  private def _write_with_schema(schema: Schema, rownum: Int, rec: Record) {
    val row = _create_row(rownum)
    for ((c, i) <- schema.columns.zipWithIndex) {
      _get_data(c, rec) match {
        case Some(s) => 
          val cell = row.createCell(i)
          _set_data(cell, s)
          _set_body_format(c, cell)
        case None =>
          if (is_ruled_line) {
            val cell = row.createCell(i)
            _set_body_format(c, cell)
          }
      }
    }
  }

  private def _get_data(column: Column, rec: Record): Option[Any] = {
    val a = rec.get(column.name) orElse column.label.flatMap(rec.get)
    a.flatMap(_get_data)
  }

  private def _get_data(xs: List[Any]): Option[Any] = {
    xs.nonEmpty option {
      if (xs.length == 1)
        xs(0)
      else
        xs.mkString(",")
    }
  }

  def openAppender(): RecordBag.Appender = ???
  def write(rs: java.sql.ResultSet): Unit = ???

  def writeRecords(rs: Iterator[Record]): Unit = {
    writeRecordsWithSchema(rs)
  }

  def writeRecordsWithSchema(rs: Iterator[Record]): Unit = {
    fromIterator(rs).to(recordW).run.run
  }

  private def _set_data(cell: Cell, data: Any) {
    data match {
      case m: String => cell.setCellValue(m)
      case m: Boolean => cell.setCellValue(m)
      case m: Short => cell.setCellValue(m.toDouble)
      case m: Int => cell.setCellValue(m.toDouble)
      case m: Long => cell.setCellValue(m.toDouble)
      case m: Float => cell.setCellValue(m.toDouble)
      case m: Double => cell.setCellValue(m)
      case m => cell.setCellValue(m.toString)
    }
  }

  private def _write_without_schema(row: Int, rec: Record) {
    ???
  }

  override def getViews: List[RecordBagView] = {
    import scala.collection.JavaConverters._
    workbook_ensure_internal()
    val a = for (i <- 0 until to_workbook.getNumberOfSheets()) yield {
      val sheet = to_workbook.getSheetAt(i)
      View(strategy, this.bag, sheet)
    }
    a.toList
  }
}

object ExcelBag {
  sealed trait ExcelKind {
    def suffix: String
  }
  case object Xls extends ExcelKind {
    def suffix: String = "xls"
  }
  case object Xlsx extends ExcelKind {
    def suffix: String = "xlsx"
  }

  case class Properties(
    ruledLine: Option[Boolean],
    headerFormat: Option[Properties.HeaderFormat],
    fixedHeader: Option[Boolean]
  ) {
  }

  object Properties {
    val empty = Properties(None, None, None)

    case class HeaderFormat()
  }

  def createWithFilenameHint(
    filename: FileName, // Just hint
    bag: org.goldenport.bag.ChunkBag,
    strategy: RecordBag.Strategy,
    name: String
  ): ExcelBag = {
    val kind = _to_kind(filename.v)
    create(kind, bag, strategy, name)
  }

  private def _to_kind(filename: String) = {
    StringUtils.toSuffix(filename) match {
      case "xls" => Xls
      case "xlsx" => Xlsx
      case _ => throw new IllegalArgumentException(s"Not Excel file: $filename")
    }
  }

  private def _to_name(filename: String) = StringUtils.pathLastComponentBody(filename)

  def create(
    kind: ExcelKind,
    bag: org.goldenport.bag.ChunkBag,
    strategy: RecordBag.Strategy,
    name: String
  ): ExcelBag = {
    val file = bag.switchToFileBag
    new ExcelBag(kind, file, strategy, name = name)
  }

  def forCreate(
    filename: String,
    strategy: RecordBag.Strategy,
    properties: Properties
  ): ExcelBag = {
    val kind = _to_kind(filename)
    val name = _to_name(filename)
    val file = org.goldenport.bag.FileBag.create(filename)
    new ExcelBag(kind, file, strategy, name, properties = properties)
  }

  def load(
    uri: java.net.URI,
    strategy: Strategy = Strategy.windowsAuto,
    properties: Properties = Properties.empty
  ): ExcelBag = loadUri(uri.toString, strategy, properties)

  def loadUri(
    uri: String,
    strategy: Strategy = Strategy.windowsAuto,
    properties: Properties = Properties.empty
  ): ExcelBag = {
    val url = UURL.getURLFromFileOrURLName(uri)
    val bag = BufferFileBag.create(strategy.codec)
    for {
      in <- resource.managed(url.openStream())
    } {
      bag.write(in)
    }
    val file = bag.switchToFileBag
    val kind = _to_kind(uri)
    val name = _to_name(uri)
    new ExcelBag(kind, file, strategy, name = name, properties = properties)
  }

  case class View(
    strategy: RecordBag.Strategy,
    chunkBag: org.goldenport.bag.ChunkBag,
    sheet: Sheet
  ) extends RecordBagView {
    def name = sheet.getSheetName
    lazy val getSchema = _generate_schema()
    lazy val schema = getSchema getOrElse {
      throw new IllegalArgumentException("No schema")
    }

    def recordsR: Process[Task, Record] = {
      type RowIterator = java.util.Iterator[Row]
      def acquire = Task.delay(sheet.rowIterator)
      def release(src: RowIterator) = Task.delay {}
      def execute(src: RowIterator) = Task.delay {
        if (src.hasNext())
          _to_record(Option(src.next))
        else
          throw Cause.Terminated(Cause.End)
      }
      scalaz.stream.io.resource(acquire)(release)(execute)
    }

    private def _to_record(row: Option[Row]): Record = {
      row.cata(_to_record, Record.empty)
    }

    private def _to_record(row: Row): Record = {
      val fields = _to_fields(row)
      RecordUtils.makeRecordFlatten(schema, fields)
    }

    private def _to_fields(row: Row): Vector[Option[Any]] = {
      val a = for (i <- 0 until row.getLastCellNum) yield {
        _get_data(Option(row.getCell(i, Row.RETURN_BLANK_AS_NULL)))
      }
      a.toVector
    }

    private def _to_fields_packing(row: Row): Vector[Option[Any]] = {
      row.cellIterator().asScala.toVector.map(_get_data)
    }

    private def _get_data(cell: Option[Cell]): Option[Any] = {
      cell.flatMap(_get_data)
    }

    private def _get_data(cell: Cell): Option[Any] = {
      cell.getCellType match {
        case Cell.CELL_TYPE_BLANK => None
        case Cell.CELL_TYPE_NUMERIC => Option(cell.getNumericCellValue)
        case Cell.CELL_TYPE_STRING => Option(cell.getStringCellValue)
        case Cell.CELL_TYPE_FORMULA => Option(cell.getCellFormula)
        case Cell.CELL_TYPE_BOOLEAN => Option(cell.getBooleanCellValue)
        case Cell.CELL_TYPE_ERROR => Option(cell.getErrorCellValue)
      }
    }

    private def _generate_schema() = {
      sheet.rowIterator().asScala.take(1).toVector.headOption map { row =>
        val columns = row.cellIterator().asScala.toVector.zipWithIndex map {
          case  (cell, i) =>
            val label = _get_data(cell).getOrElse(s"column_${i + 1}").toString
            Column(label)
        }
        Schema(columns)
      }
    }
  }

  class IntIndex(var number: Int) {
    def apply(): Int = {
      val r = number
      number += 1
      r
    }
  }

  def fromIterator[T](
    iter: Iterator[T]
  ): Process[Task, T] = {
    def acquire = Task.now(iter)
    def release(src: Iterator[T]) = Task.now()
    def execute(src: Iterator[T]) = Task delay {
      if (src.hasNext)
        src.next
      else
        throw Cause.Terminated(Cause.End)
    }
    scalaz.stream.io.resource(acquire)(release)(execute)
  }

  def main(args: Array[String]) {
    object color {
      val commandHeader = 0xffd700 // gold
//      val commandBody = 0xf0e68c // khaki
      val commandBody = 0xffffe0 // lightyellow
      val imageHeader = 0xfa8072 // salmon
      val imageBody = 0xffb6c1 // lightpink
      val tagHeader = 0x87ceeb // skyblue
      val tagBody = 0xe6e6fa // lavender
    }
    val bf = DisplayFormat.HBBB(color.commandHeader, color.commandBody)
    val cf = DisplayFormat.HBBB(color.imageHeader, color.imageBody)
    val df = DisplayFormat.HBBB(color.tagHeader, color.tagBody)
    val schema = Schema(Vector(
      Column("a", XString),
      Column("b", XString, label = Some("日本語"), displayFormat = Some(bf)),
      Column("c", XString, displayFormat = Some(cf)),
      Column("d", XString, displayFormat = Some(df))))
    val strategy = RecordBag.Strategy.naturalWrite(schema)
    val properties = ExcelBag.Properties(
      Some(true), None, Some(true)
    )
    val bag = forCreate("/tmp/test.xlsx", strategy, properties)
    bag.writeRecords(Vector(
      Record.data("a" -> "A", "b" -> "B", "c" -> "CCC", "d" -> "D"),
      Record.data("a" -> "A", "b" -> "B", "c" -> "C"),
      Record.data("a" -> "A", "b" -> "日本語")))
    bag.flush()
  }
}
