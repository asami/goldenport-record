package org.goldenport.record.v3

import org.w3c.dom._
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.matrix.{IMatrix, VectorRowColumnMatrixBase, VectorRowColumnMatrix, VectorColumnRowMatrix}
import org.goldenport.xsv.{Xsv, Lxsv}
import org.goldenport.parser.LogicalToken
import org.goldenport.values.NumberRange
import org.goldenport.util.{StringUtils, AnyUtils => LAnyUtils}
import org.goldenport.record.v2.{Schema, Column, XString, XDouble}
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
 *  version Nov. 28, 2019
 *  version Dec.  8, 2019
 * @version Jan. 30, 2020
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
  def matrixDouble: IMatrix[Double] = VectorRowColumnMatrix(records.map(_.fields.map(_make_double).toVector))
  def matrixDoubleDistilled: IMatrix[Double] = VectorRowColumnMatrix(
    toRecordVector.map(_matrix_row(_matrix_columns, _))
  )
  override def toString() = print

  private def _matrix_columns =
    if (data.columns.exists(_.datatype.isNumber)) {
      data.columns
    } else {
      val a = data.columns.zip(matrix.columnVector)
      a.map {
        case (c, xs) => if (_guess_number(xs)) c.withDatatype(XDouble) else c
      }
    }

  private def _guess_number(p: Vector[Any]) = p.forall(LAnyUtils.guessNumberOrEmpty)

  private def _matrix_row(columns: Seq[Column], rec: Record): Vector[Double] = {
    case class Z(r: Vector[Double] = Vector.empty) {
      def +(rhs: Column) = {
        if (rhs.datatype.isNumber)
          copy(r = r :+ _value(rhs))
        else
          this
      }

      private def _value(rhs: Column) = rec.getDouble(rhs.name).getOrElse(0.0)
    }
    columns./:(Z())(_+_).r
  }

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

  private def _make_double(p: Field): Double = p.value.getValue.map {
    case "" => 0.0
    case m => AnyUtils.toDouble(m)
  }.getOrElse(0.0)

  def headOption: Option[Record] = records.headOption
  def tail: Table = if (records.isEmpty) this else copy(records = records.tail)
  def getRow(y: Int): Option[Record] = records.lift(y)
  def getColumn(x: Int): Option[Vector[Any]] = data.getColumn(x).map(_.map(_.content))
  def getColumn(x: String): Option[Vector[Any]] = data.getColumn(x).map(_.map(_.content))
  def row(y: Int): Record = records(y)
  def column(x: Int): Vector[Any] = data.column(x).map(_.content)
  def column(x: String): Vector[Any] = data.column(x).map(_.content)
  def get(x: Int, y: Int): Option[Any] = data.get(x, y).map(_.content)
  def get(x: String, y: Int): Option[Any] = data.get(x, y).map(_.content)
  def at(x: Int, y: Int): Any = data.at(x, y).content
  def at(x: String, y: Int): Any = data.at(x, y).content

  def filter(p: Record => Boolean): Table = copy(records = records.filter(p))

  def select(names: Seq[String]): Table = {
    val is = data.selectIndex(names)
    Table(
      records.map(_.select(names)),
      meta.select(names),
      head.map(_.select(is)),
      foot.map(_.select(is))
    )
  }
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

    def select(names: Seq[String]): MetaData = {
      MetaData(
        schema.map(_.select(names))
      )
    }
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
    def getColumn(x: Int): Option[Vector[Cell]] =
      if (width >= x)
        None
      else
        Some(column(x))
    def getColumn(x: String): Option[Vector[Cell]] =
      getIndex(x).map(column)
    def column(x: Int): Vector[Cell] = matrix.columnVector(x)
    def column(x: String): Vector[Cell] = matrix.columnVector(index(x))
    def get(x: Int, y: Int): Option[Cell] = matrix.get(x, y)
    def get(x: String, y: Int): Option[Cell] = getIndex(x).flatMap(get(_, y))
    def at(x: Int, y: Int): Cell = matrix(x, y)
    def at(x: String, y: Int): Cell = matrix(index(x), y)

    def getIndex(p: String): Option[Int] =
      columns.zipWithIndex.find(_._1.name == p).map(_._2)
    def index(p: String): Int = getIndex(p).getOrElse(RAISE.noSuchElementFault(p))

    // def selectIndex(names: Set[String]): Option[Seq[(String, Int)]] =
    //   schema.map(_.columns.zipWithIndex./:(Vector.empty[Int])((z, x) =>
    //     if (names.contains(x._1.name))
    //       z :+ (x._1.name, x._2)
    //     else
    //       z
    //   ))

    def selectIndex(names: Seq[String]): Seq[Int] = {
      case class Z(r: Vector[Int] = Vector.empty) {
        private val _xs = columns.map(_.name).zipWithIndex.toMap

        def +(rhs: String) = _xs.get(rhs).
          map(x => copy(r = r :+ x)).
          getOrElse(RAISE.noSuchElementFault(rhs))
      }
      names./:(Z())(_+_).r
    }

    def select(names: Seq[String]): Data = {
      val is = selectIndex(names)
      Data(
        is./:(Vector.empty[Column])((z, x) => z :+ columns(x)),
        DataMatrix.create(matrix.select(is))
      )
    }
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

    def create(p: IMatrix[Cell]): DataMatrix = DataMatrix(p.rowIterator.toVector)
  }

  case class Head(names: List[Cell]) {
    def matrix: IMatrix[Cell] = DataMatrix.create(Vector(names))

    def select(is: Seq[Int]) = Head(
      is./:(Vector.empty[Cell])((z, x) => z :+ names(x)).toList
    )
  }
  object Head {
    def apply(name: String, names: String*): Head = create(name +: names)

    def create(ps: Seq[String]): Head = Head(ps.map(Cell(_)).toList)
  }

  case class Foot(data: List[Cell]) {
    def matrix: IMatrix[Cell] = DataMatrix.create(Vector(data))

    def select(is: Seq[Int]) = Foot(
      is./:(Vector.empty[Cell])((z, x) => z :+ data(x)).toList
    )
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

  def create(schema: Schema, p: RecordSequence): Table = create(schema, p.irecords)

  def create(schema: Schema, ps: Seq[IRecord]): Table = {
    val head = _create_head(schema)
    Table(ps.map(_.toRecord).toVector, MetaData(schema), Some(head))
  }

  def create(schema: Option[Schema], ps: Seq[IRecord]): Table =
    schema.map(create(_, ps)).getOrElse(create(ps))

  def create(schema: Option[Schema], ps: RecordSequence): Table =
    create(schema orElse ps.schema, ps.irecords)

  def create(ps: Seq[IRecord]): Table = create(IRecord.makeSchema(ps), ps)

  def create(ps: RecordSequence): Table = create(ps.schema, ps.irecords)

  // def createSeqSeq(schema: Schema, ps: Seq[Seq[Any]]): Table =

  private def _create_head(schema: Schema): Head = {
    val xs = schema.columns.map(_to_cell)
    Head(xs.toList)
  }

  private def _to_cell(p: Column): Cell = Cell(p.name)

  def create(p: Xsv): Table = {
    val xs = p.rowIterator.toVector
    xs.headOption.map(x => _create(x, xs.tail)).getOrElse(Table.empty)
  }

  private def _create(head: Vector[LogicalToken], tail: Vector[Vector[LogicalToken]]): Table = {
    val names = head.map(_.raw)
    val columns = names.map(Column(_))
    val schema = Schema(columns)
    case class Z(
      records: Vector[Record] = Vector.empty
    ) {
      def r: Vector[Record] = records

      def +(rhs: Vector[LogicalToken]) = {
        val ns = _make_names(rhs)
        val xs = rhs.zip(ns).map {
          case (v, k) => k -> v.raw
        }
        copy(records :+ Record.create(xs))
      }

      private def _make_names(p: Vector[LogicalToken]) =
        if (p.length <= names.length)
          names
        else
          names ++ (1 to (p.length - names.length)).map(_make_name)

      private def _make_name(p: Int) = {
        @annotation.tailrec
        def go(count: Int): String = {
          val s = s"${Vector.fill(count)('_')}$p"
          if (names.contains(s))
            go(count + 1)
          else
            s
        }
        go(1)
      }
    }
    val rs = tail./:(Z())(_+_).r
    create(schema, rs)
  }

  def create(ps: Vector[Lxsv]): Table = {
    val rs = ps.map(Record.create)
    create(rs)
  }

  def createDouble(p: IMatrix[Double]): Table = {
    val cs = for (i <- 1 to p.width) yield Column(s"$i", XDouble)
    val schema = Schema(cs)
    create(schema, p.asInstanceOf[IMatrix[Any]])
  }

  def createStringAutoNumber(p: IMatrix[String]): Table = createAutoNumber(p.asInstanceOf[IMatrix[Any]])

  def createAutoNumber(p: IMatrix[Any]): Table = createAuto(x => x.forall(x => !_is_numberable(x)), p)

  def createAuto(headerp: Vector[Any] => Boolean,  p: IMatrix[Any]): Table =
    p.headOption.map(x =>
      if (headerp(x))
        createAuto(x.map(AnyUtils.toString), p.tail)
      else
        createAuto(p)
    ).getOrElse(Table.empty)

  def createAuto(names: Seq[String], p: IMatrix[Any]): Table = {
    val matrix = if (true) _distill_numberable_matrix(p) else p
    case class Z(
      ns: Vector[Column] = Vector.empty,
      xs: Vector[Vector[Any]] = Vector.empty
    ) {
      def r = create(Schema(ns), VectorColumnRowMatrix(xs))

      def +(rhs: Int) = {
        if (_is_numberable_column(matrix, rhs))
          copy(ns = ns :+ _column(rhs), xs = xs :+ p.columnVector(rhs))
        else
          this
      }

      private def _name(p: Int): String = names.lift(p).getOrElse(s"${p + 1}")

      private def _column(p: Int): Column = Column(_name(p), XDouble)
    }
    (0 until matrix.width)./:(Z())(_+_).r
  }

  private def _distill_numberable_matrix(p: IMatrix[Any]): IMatrix[Any] =
    VectorRowColumnMatrix(p.rowIterator.filter(_.forall(_is_numberable)).toVector)

  private def _is_numberable_column(p: IMatrix[Any], x: Int): Boolean =
    (0 until p.height).map(y => _is_numberable(p(x, y))).forall(identity)

  private def _is_numberable(p: Any) = p match {
    case m: String => _is_numberable_string(m)
    case m => LAnyUtils.isNumber(m)
  }

  private def _is_numberable_string(p: String) = Strings.blankp(p) || StringUtils.isNumberWide(p)

  def createAuto(p: IMatrix[Any]): Table = {
    val cs = for (i <- 1 to p.width) yield {
      val datatype = XString // TODO
      Column(s"$i", datatype)
    }
    val schema = Schema(cs)
    create(schema, p)
  }

  def create(schema: Schema, p: IMatrix[Any]): Table = {
    val head = _create_head(schema)
    val rs = for (x <- p.rowIterator.toVector) yield {
      val xs = schema.columns.zip(x).map {
        case (c, v) => c.name -> v
      }
      Record.create(xs)
    }
    Table(rs, MetaData(schema), Some(head), None)
  }

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
