package org.goldenport.record.v3

import org.w3c.dom._
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.matrix.{IMatrix, VectorRowColumnMatrixBase, VectorRowColumnMatrix}
import org.goldenport.values.NumberRange
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.util.AnyUtils

/*
 * @since   Aug. 24, 2018
 *  version Sep.  4, 2018
 *  version Dec. 27, 2018
 *  version May. 27, 2019
 *  version Jun. 23, 2019
 *  version Jul. 29, 2019
 *  version Aug. 21, 2019
 *  version Sep. 19, 2019
 *  version Oct. 16, 2019
 * @version Nov. 28, 2019
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  meta: Table.MetaData = Table.MetaData.empty,
  head: Option[Table.Head] = None,
  foot: Option[Table.Foot] = None
) extends ITable {
  lazy val schema = meta.schema getOrElse Record.makeSchema(records) // TODO unify data and head
  def toTable = this
  def toRecordList: List[Record] = records.toList
  def toRecordVector: Vector[Record] = records
  override lazy val width = meta.getWidth getOrElse data.width
  override def height = records.length
  def matrix: IMatrix[Any] = VectorRowColumnMatrix(records.map(_.fields.map(_make_value).toVector))
  override def toString() = print

  def print = {
    val tv = TableVisualizer()
    tv.plainText(this)
  }
  def display = s"Table[${width}x${height}]"
  def show = {
    val tv = TableVisualizer(isEmbed = true)
    tv.plainText(this)
  }
  def embed: String = display

  lazy val data: Table.Data = { // TODO unify schema
    case class Z(
      columns: Vector[Column] = Vector.empty,
      matrix: Vector[Vector[Table.Cell]] = Vector.empty
    ) {
      def r = Table.Data(columns, Table.DataMatrix.create(matrix))

      def +(rhs: Record) = {
        val cs: Vector[Column] = rhs.fields./:(columns) { (z, x) =>
          if (z.exists(_.name == x.key.name))
            z.map(xx =>
              if (xx.name == x.key.name)
                _to_column(xx, x)
              else
                xx
            )
          else
            z :+ _to_column(x)
        }
        val ds = cs./:(Vector.empty[Table.Cell]) { (z, x) =>
          val a = rhs.getValue(x.name).map {
            case m: SingleValue => _to_content(m)
            case m: MultipleValue => _to_content(m)
            case EmptyValue => Table.Cell.empty
          } getOrElse {
            Table.Cell.empty
          }
          z :+ a
        }
        Z(cs, matrix :+ ds)
      }

      import org.goldenport.record.v2.{Field => _, Table => _, _}

      private def _to_column(p: Field): Column = {
        p.value match {
          case SingleValue(v) => Column(p.name, _datatype(v), MOne)
          case MultipleValue(v) => Column(p.name, _datatype_multi(v), _multiplicity(v))
          case EmptyValue => RAISE.notImplementedYetDefect
        }
      }

      private def _datatype(p: Any): DataType = XString // TODO

      private def _datatype_multi(p: Seq[Any]): DataType = XString // TODO

      private def _multiplicity(p: Seq[Any]): Multiplicity =
        if (p.isEmpty)
          MZeroMore
        else
          MOneMore

      private def _to_column(c: Column, p: Field): Column = c // TODO datatype & multiplicity

      private def _to_content(p: SingleValue): Table.Cell = Table.Cell(p.value)

      private def _to_content(p: MultipleValue): Table.Cell = Table.Cell(p.values) // TODO
    }
    records./:(Z())(_+_).r
  }

  private def _make_value(p: Field): Any = schema.getColumn(p.key).
    map(c => p.value.getValue.map(c.datatype.toInstance).getOrElse(Table.Empty)).
    getOrElse(p.value.getValue.getOrElse(Table.Empty))

  def headOption: Option[Record] = records.headOption
  def tail: Table = if (records.isEmpty) this else copy(records = records.tail)
  def getRow(i: Int): Option[Record] = records.lift(i)
}

object Table {
  import org.goldenport.xml.dom.RichElement.Implicits._

  val empty = Table(Vector.empty)

  case object Empty {
  }

  case class MetaData(
    schema: Option[Schema]
  ) {
    def getWidth: Option[Int] = schema.map(_.columns.length)

    def getKey(i: Int): Option[Symbol] = schema.
      flatMap(_.columns.lift(i).map(x => Symbol(x.name)))
  }
  object MetaData {
    val empty = MetaData(None)

    def apply(schema: Schema): MetaData = MetaData(Some(schema))
  }

  case class Data(
    columns: Vector[Column],
    matrix: DataMatrix
  ) {
    def width = columns.length
    def height = matrix.height
  }

  case class Cell(
    content: Any,
    width: Option[Int] = None,
    height: Option[Int] = None
  ) {
    def text: String = AnyUtils.toString(content)
    def print(width: Option[Int]): String = width.map(print).getOrElse(print)
    def print(width: Int): String = AnyUtils.toString(content) // TODO
    def print: String = AnyUtils.toPrint(content) // TODO
    def embed(width: Option[Int]): String = width.map(embed).getOrElse(embed)
    def embed(width: Int): String = AnyUtils.toString(content) // TODO
    def embed: String = AnyUtils.toEmbed(content) // TODO
  }
  object Cell {
    val empty = Cell(Empty)
  }

  case class DataMatrix(
    matrix: Vector[Vector[Cell]],
    emptyValue: Option[Cell] = Some(Cell.empty)
  ) extends VectorRowColumnMatrixBase[Cell] {
    def toCellMatrix: IMatrix[Cell] = RAISE.notImplementedYetDefect
    def makeCellMatrix: IMatrix[Cell] = RAISE.notImplementedYetDefect
    def appendRow(ps: Seq[Cell]): DataMatrix = DataMatrix(matrix :+ ps.toVector)
    def appendRows(ps: IMatrix[Cell]): DataMatrix = DataMatrix(matrix ++ ps.rowIterator)
    def appendColumn(ps: Seq[Cell]): IMatrix[Cell] = RAISE.notImplementedYetDefect
    def appendColumns(ps: IMatrix[Cell]): IMatrix[Cell] = RAISE.notImplementedYetDefect
  }
  object DataMatrix {
    def create(pss: Seq[Seq[Cell]]): DataMatrix = DataMatrix(
      pss.toVector.map(_.toVector)
    )
  }

  case class Head(names: List[Cell]) {
    def matrix: IMatrix[Cell] = DataMatrix.create(Vector(names))
  }
  object Head {
    def apply(name: String, names: String*): Head = create(name +: names)

    def create(ps: Seq[String]): Head = Head(ps.map(Cell(_)).toList)
  }

  case class Foot(data: List[Cell]) {
    def matrix: IMatrix[Cell] = DataMatrix.create(Vector(data))
  }

  def apply(head: Table.Head, data: Seq[Record]): Table =
    Table(data.toVector, head = Some(head))

  def create(p: JsValue): Table = Record.createRecordOrSequence(p) match {
    case Left(rs) => 
      val schema = IRecord.makeSchema(rs)
      create(schema, rs.irecords)
    case Right(r) => 
      val schema = IRecord.makeSchema(r)
      create(schema, Vector(r))
  }

  def create(p: Node): Table = Record.createRecordOrSequence(p) match {
    case Left(rs) =>
      val schema = IRecord.makeSchema(rs)
      create(schema, rs.irecords)
    case Right(r) =>
      val schema = IRecord.makeSchema(r)
      create(schema, Vector(r))
  }

  def create(schema: Schema, ps: Seq[IRecord]): Table = {
    val head = _create_head(schema)
    Table(ps.map(_.toRecord).toVector, MetaData(schema), Some(head))
  }

  def create(schema: Option[Schema], ps: Seq[IRecord]): Table =
    schema.map(create(_, ps)).getOrElse(create(IRecord.makeSchema(ps), ps))

  def create(schema: Option[Schema], ps: RecordSequence): Table =
    create(schema orElse ps.schema, ps.irecords)

  def create(ps: RecordSequence): Table = create(ps.schema, ps.irecords)

  // def createSeqSeq(schema: Schema, ps: Seq[Seq[Any]]): Table =

  private def _create_head(schema: Schema): Head = {
    val xs = schema.columns.map(_to_cell)
    Head(xs.toList)
  }

  private def _to_cell(p: Column): Cell = Cell(p.name)

  def createHtml(p: Node): Table = createHtmlOption(p).getOrElse(RAISE.invalidArgumentFault("No table content"))

  def createHtmlOption(p: Node): Option[Table] = _create_html(p)

  private def _create_html(p: Node): Option[Table] = Option(p) flatMap {
    case m: Element => _create_element_html(m)
    case m => None
  }

  private def _create_element_html(p: Element): Option[Table] =
    Option(p.getLocalName()).map(_.toLowerCase).collect {
      case "table" => _create_table_html(p)
    }

  private def _create_table_html(p: Element): Table = {
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
    trs.map(Head(_))
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
    val a = p.names.map(x => Column(_to_string(x.content))) // TODO width
    val s = Schema(a)
    MetaData(Some(s))
  }

  private def _to_string(p: Any): String = AnyUtils.toString(p)
}
