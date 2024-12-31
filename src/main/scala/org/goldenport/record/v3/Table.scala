package org.goldenport.record.v3

import org.w3c.dom._
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.extension.Showable
import org.goldenport.matrix.{IMatrix, VectorRowColumnMatrixBase, VectorRowColumnMatrix, VectorColumnRowMatrix}
import org.goldenport.xsv.{Xsv, Lxsv}
import org.goldenport.parser.LogicalToken
import org.goldenport.i18n._
import org.goldenport.xml.dom.DomUtils
import org.goldenport.values.NumberRange
import org.goldenport.value._
import org.goldenport.util.{StringUtils, AnyUtils => LAnyUtils}
import org.goldenport.record.v2.{Schema => Schema2, Column => Column2, XString, XDouble}
import org.goldenport.record.v2.DataType
import org.goldenport.record.v2.XObject
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
 *  version Jan. 30, 2020
 *  version Feb. 28, 2020
 *  version Mar. 30, 2020
 *  version Mar. 25, 2021
 *  version Apr. 13, 2021
 *  version Oct. 31, 2021
 *  version Feb. 23, 2022
 *  version Mar. 19, 2022
 *  version May.  7, 2022
 *  version Jan. 22, 2023
 * @version Oct. 23, 2024
 * @author  ASAMI, Tomoharu
 */
case class Table(
  idata: Table.IData,
  meta: Table.MetaData = Table.MetaData.empty,
  head: Option[Table.Head] = None,
  side: Option[Table.Side] = None,
  foot: Option[Table.Foot] = None
) extends ITable {
  lazy val schema = meta.schema getOrElse idata.schema // TODO unify data and head
  def toTable = this
  def toRecordList: List[Record] = idata.toRecordList
  def toRecordVector: Vector[Record] = idata.toRecordVector
  override lazy val width = meta.getWidth getOrElse idata.width
  override def height = idata.height
  def matrix: IMatrix[Any] = idata.matrix
  def matrixDouble: IMatrix[Double] = idata.matrixDouble
  def matrixDoubleDistilled: IMatrix[Double] = idata.matrixDoubleDistilled
  // def matrix: IMatrix[Any] = VectorRowColumnMatrix(records.map(_.fields.map(_make_value).toVector))
  // def matrixDouble: IMatrix[Double] = VectorRowColumnMatrix(records.map(_.fields.map(_make_double).toVector))
  // def matrixDoubleDistilled: IMatrix[Double] = VectorRowColumnMatrix(
  //   toRecordVector.map(_matrix_row(_matrix_columns, _))
  // )
  override def equals(o: Any) = o match {
    case m: Table => idata.equals(m.idata) && meta.equals(m.meta) && head.equals(m.head) && side.equals(m.side) && foot.equals(m.foot)
    case m: ITable => data.equals(m.data) && meta.equals(m.meta) && head.equals(m.head) && side.equals(m.side) && foot.equals(m.foot)
    case _ => false
  }

  override def hashCode() = idata.hashCode + meta.hashCode + head.hashCode + side.hashCode + foot.hashCode

  override def toString() = print

  // private def _matrix_columns =
  //   if (data.columns.exists(_.datatype.isNumber)) {
  //     data.columns
  //   } else {
  //     val a = data.columns.zip(matrix.columnVector)
  //     a.map {
  //       case (c, xs) => if (_guess_number(xs)) c.withDatatype(XDouble) else c
  //     }
  //   }

  // private def _guess_number(p: Vector[Any]) = p.forall(LAnyUtils.guessNumberOrEmpty)

  // private def _matrix_row(columns: Seq[Column2], rec: Record): Vector[Double] = {
  //   case class Z(r: Vector[Double] = Vector.empty) {
  //     def +(rhs: Column2) = {
  //       if (rhs.datatype.isNumber)
  //         copy(r = r :+ _value(rhs))
  //       else
  //         this
  //     }

  //     private def _value(rhs: Column2) = rec.getDouble(rhs.name).getOrElse(0.0)
  //   }
  //   columns./:(Z())(_+_).r
  // }

  def print = {
    val tv = TableVisualizer()
    tv.plainText(this)
  }
  def display = s"Table[${width}x${height}]"
  def show = {
    val tv = TableVisualizer(isCompact = true, isEmbed = true)
    tv.plainText(this)
  }

  def data: Table.Data = idata.data
  // lazy val data: Table.Data = { // TODO unify schema
  //   case class Z(
  //     columns: Vector[Column2] = Vector.empty,
  //     matrix: Vector[Vector[Table.Cell]] = Vector.empty
  //   ) {
  //     def r = Table.Data(columns, Table.DataMatrix.create(matrix))

  //     def +(rhs: Record) = {
  //       val cs: Vector[Column2] = rhs.fields./:(columns) { (z, x) =>
  //         if (z.exists(_.name == x.key.name))
  //           z.map(xx =>
  //             if (xx.name == x.key.name)
  //               _to_column(xx, x)
  //             else
  //               xx
  //           )
  //         else
  //           z :+ _to_column(x)
  //       }
  //       val ds = cs./:(Vector.empty[Table.Cell]) { (z, x) =>
  //         val a = rhs.getValue(x.name).map {
  //           case m: SingleValue => _to_content(m)
  //           case m: MultipleValue => _to_content(m)
  //           case EmptyValue => Table.Cell.empty
  //         } getOrElse {
  //           Table.Cell.empty
  //         }
  //         z :+ a
  //       }
  //       Z(cs, matrix :+ ds)
  //     }

  //     import org.goldenport.record.v2.{Field => _, Table => _, _}

  //     private def _to_column(p: Field): Column2 = {
  //       p.value match {
  //         case SingleValue(v) => Column2(p.name, _datatype(v), MOne)
  //         case MultipleValue(v) => Column2(p.name, _datatype_multi(v), _multiplicity(v))
  //         case EmptyValue => RAISE.notImplementedYetDefect
  //       }
  //     }

  //     private def _datatype(p: Any): DataType = XString // TODO

  //     private def _datatype_multi(p: Seq[Any]): DataType = XString // TODO

  //     private def _multiplicity(p: Seq[Any]): Multiplicity =
  //       if (p.isEmpty)
  //         MZeroMore
  //       else
  //         MOneMore

  //     private def _to_column(c: Column2, p: Field): Column2 = c // TODO datatype & multiplicity

  //     private def _to_content(p: SingleValue): Table.Cell = Table.Cell(_to_embed(p.value))

  //     private def _to_embed(p: Any): String = p match {
  //       case m: Showable => m.embed
  //       case m => AnyUtils.toString(p)
  //     }

  //     private def _to_content(p: MultipleValue): Table.Cell = Table.Cell(_to_embeds(p.values)) // TODO

  //     private def _to_embeds(ps: Seq[Any]): String = ps match {
  //       case Seq() => ""
  //       case Seq(x) => _to_embed(x)
  //       case m: Seq[_] => Vector(_to_embed(m(0)), _to_embed(m(1))).mkString("\n")
  //     }
  //   }
  //   records./:(Z())(_+_).r
  // }

  // private def _make_value(p: Field): Any = schema.getColumn(p.key).
  //   map(c => p.value.getValue.map(c.datatype.toInstance).getOrElse(Table.Empty)).
  //   getOrElse(p.value.getValue.getOrElse(Table.Empty))

  // private def _make_double(p: Field): Double = p.value.getValue.map {
  //   case "" => 0.0
  //   case m => AnyUtils.toDouble(m)
  // }.getOrElse(0.0)

  def headRecordOption: Option[Record] = idata.headOption
  def tail: Table = copy(idata = idata.tail)
  def getRow(y: Int): Option[Record] = idata.getRow(y)
  def getColumn(x: Int): Option[Vector[Any]] = data.getColumn(x).map(_.map(_.content))
  def getColumn(x: String): Option[Vector[Any]] = data.getColumn(x).map(_.map(_.content))
  def row(y: Int): Record = idata.row(y)
  def column(x: Int): Vector[Any] = data.column(x).map(_.content)
  def column(x: String): Vector[Any] = data.column(x).map(_.content)
  def get(x: Int, y: Int): Option[Any] = data.get(x, y).map(_.content)
  def get(x: String, y: Int): Option[Any] = data.get(x, y).map(_.content)
  def at(x: Int, y: Int): Any = data.at(x, y).content
  def at(x: String, y: Int): Any = data.at(x, y).content

  def filter(p: Record => Boolean): Table = copy(idata = idata.filter(p))

  def select(names: Seq[String]): Table = {
    val is = data.selectIndex(names)
    Table(
      idata.select(names),
      meta.select(names),
      head.map(_.select(is)),
      side,
      foot.map(_.select(is))
    )
  }

  def select(range: NumberRange): Table = {
    val is = range.indexes
    val names = is./:(Vector.empty[String])((z, x) => z :+ schema.columns(x).name)
    Table(
      idata.select(names),
      meta.select(names),
      head.map(_.select(is)),
      side,
      foot.map(_.select(is))
    )
  }

  def ensureHead: Table =
    if (head.isDefined)
      this
    else
      headRecordOption.map { x =>
        val head = Table.Head.create(x.fields.map(_.asString))
        val data = idata.tail
        copy(idata = data, head = Some(head))
      }.getOrElse(this)
}

object Table {
  import org.goldenport.xml.dom.RichElement.Implicits._

  val empty = Table(RecordData.empty)

  case object Empty {
  }

  case class MetaData(
    schema: Option[Schema2]
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

    def apply(schema: Schema2): MetaData = MetaData(Some(schema))
  }

  trait IData {
    def data: Data
    def schema: Schema2
    def width: Int
    def height: Int
    def toRecordList: List[Record]
    def toRecordVector: Vector[Record]
    def matrix: IMatrix[Any]
    def matrixDouble: IMatrix[Double]
    def matrixDoubleDistilled: IMatrix[Double]
    def headOption: Option[Record]
    def tail: IData
    def getRow(y: Int): Option[Record]
    def row(y: Int): Record
    def filter(p: Record => Boolean): IData
    def select(names: Seq[String]): IData
  }

  case class RecordData(
    records: Vector[Record]
  ) extends IData {
    override def equals(o: Any) = o match {
      case m: RecordData => m equals this
      case m: Data => m equals this.data
      case m: IData => m.toRecordVector equals records
    }

    def schema = Record.makeSchema(records)
    def width: Int = data.width
    def height: Int = records.length
    def toRecordList: List[Record] = records.toList
    def toRecordVector: Vector[Record] = records
    def matrix: IMatrix[Any] = VectorRowColumnMatrix(records.map(_.fields.map(_make_value).toVector))
    def matrixDouble: IMatrix[Double] = VectorRowColumnMatrix(records.map(_.fields.map(_make_double).toVector))
    def matrixDoubleDistilled: IMatrix[Double] = VectorRowColumnMatrix(
      toRecordVector.map(_matrix_row(_matrix_columns, _))
    )

    private def _make_value(p: Field): Any = schema.getColumn(p.key).
      map(c => p.value.getValue.map(c.datatype.toInstance).getOrElse(Table.Empty)).
      getOrElse(p.value.getValue.getOrElse(Table.Empty))

    private def _make_double(p: Field): Double = p.value.getValue.map {
      case "" => 0.0
      case m => AnyUtils.toDouble(m)
    }.getOrElse(0.0)

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

    private def _matrix_row(columns: Seq[Column2], rec: Record): Vector[Double] = {
      case class Z(r: Vector[Double] = Vector.empty) {
        def +(rhs: Column2) = {
          if (rhs.datatype.isNumber)
            copy(r = r :+ _value(rhs))
          else
            this
        }

        private def _value(rhs: Column2) = rec.getDouble(rhs.name).getOrElse(0.0)
      }
      columns./:(Z())(_+_).r
    }

    def headOption: Option[Record] = records.headOption
    def tail: IData = if (records.isEmpty) this else copy(records = records.tail)
    def getRow(y: Int): Option[Record] = records.lift(y)
    def row(y: Int): Record = records(y)
    def filter(p: Record => Boolean): IData = copy(records = records.filter(p))
    def select(names: Seq[String]): IData = copy(records = records.map(_.select(names)))

    lazy val data: Table.Data = { // TODO unify schema
      case class Z(
        columns: Vector[Column2] = Vector.empty,
        matrix: Vector[Vector[Table.Cell]] = Vector.empty
      ) {
        def r = Table.Data(columns, Table.DataMatrix.create(matrix))

        def +(rhs: Record) = {
          val cs: Vector[Column2] = rhs.fields./:(columns) { (z, x) =>
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

        private def _to_column(p: Field): Column2 = {
          p.value match {
            case SingleValue(v) => Column2(p.name, _datatype(v), MOne)
            case MultipleValue(v) => Column2(p.name, _datatype_multi(v), _multiplicity(v))
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

        private def _to_column(c: Column2, p: Field): Column2 = c // TODO datatype & multiplicity

        private def _to_content(p: SingleValue): Table.Cell = Table.Cell(_to_embed(p.value))

        private def _to_embed(p: Any): String = p match {
          case m: Showable => m.embed
          case m => AnyUtils.toString(p)
        }

        private def _to_content(p: MultipleValue): Table.Cell = Table.Cell(_to_embeds(p.values)) // TODO

        private def _to_embeds(ps: Seq[Any]): String = ps match {
          case Seq() => ""
          case Seq(x) => _to_embed(x)
          case m: Seq[_] => Vector(_to_embed(m(0)), _to_embed(m(1))).mkString("\n")
        }
      }
      records./:(Z())(_+_).r
    }
  }
  object RecordData {
    val empty = RecordData(Vector.empty)
  }

  case class Data(
    columns: Vector[Column2],
    datamatrix: DataMatrix
  ) extends IData {
    override def equals(o: Any) = o match {
      case m: Data => columns.equals(m.columns) && datamatrix.equals(m.datamatrix)
      case m: IData => m.data equals this
    }

    def data = this
    lazy val records: Vector[Record] =
      for (y <- datamatrix.matrix) yield {
        val a = columns.zip(y) map {
          case (c, x) => c.name -> x
        }
        Record.create(a)
      }

    def schema = Record.makeSchema(records) // TODO optimize
    def width = columns.length
    def height = datamatrix.height
    def toRecordList: List[Record] = records.toList
    def toRecordVector: Vector[Record] = records
    def matrix: IMatrix[Any] = ??? // VectorRowColumnMatrix(records.map(_.fields.map(_make_value).toVector))
    def matrixDouble: IMatrix[Double] = ??? // VectorRowColumnMatrix(records.map(_.fields.map(_make_double).toVector))
    def matrixDoubleDistilled: IMatrix[Double] = ??? // VectorRowColumnMatrix(
    //   toRecordVector.map(_matrix_row(_matrix_columns, _))
    // )
    def headOption: Option[Record] = records.headOption
    def tail: IData =
      if (datamatrix.isEmpty)
        this
      else
        copy(datamatrix = datamatrix.tailDataMatrix)
    def getRow(y: Int): Option[Record] = records.lift(y)
    def row(y: Int): Record = records(y)
    def filter(p: Record => Boolean): IData = ??? // copy(records = records.filter(p))

    def getColumn(x: Int): Option[Vector[Cell]] =
      if (width >= x)
        None
      else
        Some(column(x))
    def getColumn(x: String): Option[Vector[Cell]] =
      getIndex(x).map(column)
    def column(x: Int): Vector[Cell] = datamatrix.columnVector(x)
    def column(x: String): Vector[Cell] = datamatrix.columnVector(index(x))
    def get(x: Int, y: Int): Option[Cell] = datamatrix.get(x, y)
    def get(x: String, y: Int): Option[Cell] = getIndex(x).flatMap(get(_, y))
    def at(x: Int, y: Int): Cell = datamatrix(x, y)
    def at(x: String, y: Int): Cell = datamatrix(index(x), y)

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
        is./:(Vector.empty[Column2])((z, x) => z :+ columns(x)),
        DataMatrix.create(datamatrix.select(is))
      )
    }

    def trim: Data = copy(datamatrix = datamatrix.trim)
  }
  object Data {
    def create(p: IMatrix[Cell]): Data = {
      val a = DataMatrix.create(p)
      val columns = p.columnVector.zipWithIndex.map {
        case (x, i) => _make_column(x.map(_.content), i)
      }
      Data(columns, a)
    }

    def create(p: IMatrix[Cell], header: Seq[String]): Data = {
      val a = DataMatrix.create(p)
      val columns = p.columnVector.zip(header).map {
        case (x, l) => _make_column(x.map(_.content), l)
      }
      Data(columns, a)
    }

    def createFromAnyMatrix(p: IMatrix[Any]): Data = {
      val a = DataMatrix.createFromAnyMatrix(p)
      val columns = p.columnVector.zipWithIndex.map {
        case (x, i) => _make_column(x, i)
      }
      Data(columns, a)
    }

    private def _make_column(p: Vector[Any], i: Int): Column2 =
      _make_column(p, s"${i + 1}")

    private def _make_column(p: Vector[Any], label: String): Column2 = {
      val dt = DataType.guessSeq(p)
      Column2(label, dt)
    }
  }

  case class Cell(
    content: Any,
    origin: Option[Any] = None,
    width: Option[Int] = None,
    height: Option[Int] = None
  ) {
    def text: String = AnyUtils.toString(content)
    def print(pwidth: Option[Int]): String = width.map(print).getOrElse(print)
    def print(pwidth: Int): String = AnyUtils.toPrint(content, _width(pwidth))
    def print: String = width.map(print).getOrElse(AnyUtils.toPrint(content))
    def embed(pwidth: Option[Int]): String = width.map(embed).getOrElse(embed)
    def embed(pwidth: Int): String = AnyUtils.toEmbed(content, _width(pwidth))
    def embed: String = width.map(embed).getOrElse(AnyUtils.toEmbed(content))
    def trim: Cell = content match {
      case m: String => copy(content = m.trim)
      case _ => this
    }

    private def _width(p: Int) = width.map(x => math.min(x, p)).getOrElse(p)
  }
  object Cell {
    val empty = Cell(Empty)

    def apply(c: Any, origin: Any): Cell = new Cell(c, Some(origin))
  }

  case class DataMatrix(
    matrix: Vector[Vector[Cell]],
    emptyValue: Option[Cell] = Some(Cell.empty)
  ) extends VectorRowColumnMatrixBase[Cell] {
    def isEmpty: Boolean = matrix.isEmpty
    def tailDataMatrix: DataMatrix = copy(matrix = matrix.tail)

    def toCellMatrix: IMatrix[Cell] = RAISE.notImplementedYetDefect
    def makeCellMatrix: IMatrix[Cell] = RAISE.notImplementedYetDefect
    def appendRow(ps: Seq[Cell]): DataMatrix = DataMatrix(matrix :+ ps.toVector)
    def appendRows(ps: IMatrix[Cell]): DataMatrix = DataMatrix(matrix ++ ps.rowIterator)
    def appendColumn(ps: Seq[Cell]): IMatrix[Cell] = RAISE.notImplementedYetDefect
    def appendColumns(ps: IMatrix[Cell]): IMatrix[Cell] = RAISE.notImplementedYetDefect
    def map[A](p: Cell => A): IMatrix[A] = RAISE.notImplementedYetDefect
    def trim: DataMatrix = {
      val r = matrix.map(x => x.map(_.trim))
      copy(matrix = r)
    }
  }
  object DataMatrix {
    def create(pss: Seq[Seq[Cell]]): DataMatrix = DataMatrix(
      pss.toVector.map(_.toVector)
    )

    def create(p: IMatrix[Cell]): DataMatrix = DataMatrix(p.rowIterator.toVector)

    def createFromAnyMatrix(p: IMatrix[Any]): DataMatrix = create(p.map(x => Cell(x)))
  }

  case class Head(names: List[Cell]) {
    def matrix: IMatrix[Cell] = DataMatrix.create(Vector(names))

    def select(is: Seq[Int]) = Head(
      is./:(Vector.empty[Cell])((z, x) => z :+ names(x)).toList
    )

    def trim: Head = copy(names = names.map(_.trim))
  }
  object Head {
    def apply(name: String, names: String*): Head = create(name +: names)

    def create(ps: Seq[String]): Head = Head(ps.map(Cell(_)).toList)
  }

  case class Side(labels: List[Cell], head: Option[Cell], foot: Option[Cell]) {
    def trim: Side = copy(labels = labels.map(_.trim), head = head.map(_.trim), foot = foot.map(_.trim))
  }

  case class Foot(data: List[Cell]) {
    def matrix: IMatrix[Cell] = DataMatrix.create(Vector(data))

    def select(is: Seq[Int]) = Foot(
      is./:(Vector.empty[Cell])((z, x) => z :+ data(x)).toList
    )

    def trim: Foot = copy(data = data.map(_.trim))
  }

  sealed trait CreateHtmlStrategy extends NamedValueInstance {
  }
  object CreateHtmlStrategy extends EnumerationClass[CreateHtmlStrategy] {
    val elements = Vector(NaturalStrategy, EnsureHeadStrategy)

    case object NaturalStrategy extends CreateHtmlStrategy {
      val name = "natural"
    }
    case object EnsureHeadStrategy extends CreateHtmlStrategy {
      val name = "ensure-head"
    }
  }

  def apply(records: Seq[Record]): Table = Table(RecordData(records.toVector))

  def apply(head: Table.Head, data: Seq[Record]): Table =
    Table(RecordData(data.toVector), head = Some(head))

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

  def create(
    i18nContext: I18NContext,
    header: Table.HeaderStrategy,
    schema: Option[Schema2],
    p: RecordSequence
  ): Table = schema.map(create(i18nContext, header, _, p)).getOrElse(create(p))

  def create(
    i18nContext: I18NContext,
    header: Table.HeaderStrategy,
    schema: Schema2,
    p: RecordSequence
  ): Table = {
    val builder = Builder(i18nContext, header, schema)
    builder.build(p)
  }

  def create(schema: Schema2, p: RecordSequence): Table = create(schema, p.irecords)

  def create(schema: Schema2, ps: Seq[IRecord]): Table = {
    val head = _create_head(schema)
    Table(RecordData(ps.map(_.toRecord).toVector), MetaData(schema), Some(head))
  }

  def create(schema: Option[Schema2], ps: Seq[IRecord]): Table =
    schema.map(create(_, ps)).getOrElse(create(ps))

  def create(schema: Option[Schema2], ps: RecordSequence): Table =
    create(schema orElse ps.schema, ps.irecords)

  def create(ps: Seq[IRecord]): Table = create(IRecord.makeSchema(ps), ps)

  def create(ps: RecordSequence): Table = create(ps.schema, ps.irecords)

  def create(p: IMatrix[Cell]): Table = Table(Data.create(p))

  def createFromAnyMatrix(p: IMatrix[Any]): Table = Table(Data.createFromAnyMatrix(p))

  // def createSeqSeq(schema: Schema, ps: Seq[Seq[Any]]): Table =

  private def _create_head(schema: Schema2): Head = {
    val xs = schema.columns.map(_to_cell)
    Head(xs.toList)
  }

  private def _to_cell(p: Column2): Cell = Cell(p.name)

  def create(p: Xsv): Table = {
    val xs = p.rowIterator.toVector
    xs.headOption.map(x => _create(x, xs.tail)).getOrElse(Table.empty)
  }

  private def _create(head: Vector[LogicalToken], tail: Vector[Vector[LogicalToken]]): Table = {
    val names = head.map(_.raw)
    val columns = names.map(Column2(_))
    val schema = Schema2(columns)
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

  def create(p: IRecord): Table = {
    val name = "Name"
    val value = "Value"
    val schema = Schema2(List(Column2(name, XObject), Column2(value, XObject)))
    val rs = p.fields.map(x => Record.data(name -> x.name, value -> x.getValue.getOrElse("")))
    create(schema, rs)
  }

  def create(p: ITable): Table = p.toTable

  def create(p: IMatrix[Cell], header: Seq[String]): Table = {
    val data = Data.create(p, header)
    val schema = data.schema
    val meta = MetaData(Some(schema))
    val head = Head.create(schema.columns.map(_.name))
    Table(data, meta, Some(head))
  }

  def create(p: IMatrix[Cell], header: Option[Head]): Table =
    header.fold(create(p, Nil))(create(p, _))

  def create(p: IMatrix[Cell], header: Head): Table = {
    val data = Data.create(p, header.names.map(_.text))
    val schema = data.schema
    val meta = MetaData(Some(schema))
    Table(data, meta, Some(header))
  }

  def createDouble(p: IMatrix[Double]): Table = {
    val cs = for (i <- 1 to p.width) yield Column2(s"$i", XDouble)
    val schema = Schema2(cs)
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
      ns: Vector[Column2] = Vector.empty,
      xs: Vector[Vector[Any]] = Vector.empty
    ) {
      def r = create(Schema2(ns), VectorColumnRowMatrix(xs))

      def +(rhs: Int) = {
        if (_is_numberable_column(matrix, rhs))
          copy(ns = ns :+ _column(rhs), xs = xs :+ p.columnVector(rhs))
        else
          this
      }

      private def _name(p: Int): String = names.lift(p).getOrElse(s"${p + 1}")

      private def _column(p: Int): Column2 = Column2(_name(p), XDouble)
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
      Column2(s"$i", datatype)
    }
    val schema = Schema2(cs)
    create(schema, p)
  }

  def create(schema: Schema2, p: IMatrix[Any]): Table = {
    val head = _create_head(schema)
    val rs = for (x <- p.rowIterator.toVector) yield {
      val xs = schema.columns.zip(x).map {
        case (c, v) => c.name -> v
      }
      Record.create(xs)
    }
    Table(RecordData(rs), MetaData(schema), Some(head), None) // TODO Data
  }

  def createHtml(
    strategy: CreateHtmlStrategy,
    p: Node
  ): Table = createHtmlOption(strategy, p).getOrElse(RAISE.invalidArgumentFault("No table content"))

  def createHtmlOption(
    strategy: CreateHtmlStrategy,
    p: Node
  ): Option[Table] = {
    import CreateHtmlStrategy._
    _create_html(p).map(x =>
      strategy match {
        case NaturalStrategy => x
        case EnsureHeadStrategy => x.ensureHead
      }
    )
  }

  private def _create_html(p: Node): Option[Table] = Option(p) flatMap {
    case m: Element => _create_element_html(m)
    case m => None
  }

  private def _create_element_html(p: Element): Option[Table] =
    Option(DomUtils.localName(p)).map(_.toLowerCase).collect {
      case "table" => _create_table_html(p)
    }

  private def _create_table_html(p: Element): Table = {
    val head = p.getElementByLocalNameIC("thead").flatMap(_create_head)
    val foot = p.getElementByLocalNameIC("tfoot").flatMap(_create_foot)
    val meta = _make_meta(head)
    val tb = p.getElementByLocalNameIC("tbody")
    tb match {
      case Some(s) => _create_table_simple(s)
      case None => _create_table_simple(p)
    }
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

  private def _create_table_simple(p: Element): Table = {
    val trs = p.elementsVectorByLocalNameIC("tr")
    trs.length match {
      case 0 => Table.empty
      case x => _get_natural_header(trs.head) match {
        case Some(s) => _create_table(s, trs.tail)
        case None => _create_table(MetaData.empty, trs)
      }
    }
  }

  private def _create_table(meta: MetaData, p: Element): Table =
    _create_table(meta, p.elementsVectorByLocalNameIC("tr"))

  private def _create_table(meta: MetaData, trs: Vector[Element]): Table = {
    val rs = _create_records(meta, trs)
    val head = meta.schema.map { s =>
      val trs = for (c <- s.columns) yield {
        Cell(c.name)
      }
      Head(trs.toList)
    }
    Table(RecordData(rs), meta, head, None) // TODO Data
  }

  private def _create_records(meta: MetaData, ps: Vector[Element]): Vector[Record] = {
    for (tr <- ps) yield {
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
    val a = p.names.map(x => Column2(_to_string(x.content))) // TODO width
    val s = Schema2(a)
    MetaData(Some(s))
  }

  private def _get_natural_header(p: Element): Option[MetaData] = {
    val xs = p.elementsVectorByLocalNameIC("th", "td")
    if (xs.exists(_is_th))
      Some(_make_meta(xs))
    else
      None
  }

  private def _is_th(p: Element) = p.localName == "th"

  private def _make_meta(ps: Seq[Element]) = {
    val xs = ps.filter(_is_th_or_td)
    val a = xs.map(x => Column2(_to_string(x.text))) // TODO width
    val s = Schema2(a)
    MetaData(Some(s))
  }

  private def _is_th_or_td(p: Element) = p.localName match {
    case "td" => true
    case "th" => true
    case _ => false
  }

  private def _to_string(p: Any): String = AnyUtils.toString(p).trim

  case class HeaderStrategy(
    label: HeaderStrategy.LabelStrategy
  )
  object HeaderStrategy {
    val name = HeaderStrategy(HeaderStrategy.NameLabel)
    val label = HeaderStrategy(HeaderStrategy.LabelLabel)

    sealed trait LabelStrategy extends NamedValueInstance {
    }
    case object NameLabel extends LabelStrategy {
      val name = "name"
    }
    case object LabelLabel extends LabelStrategy {
      val name = "label"
    }
    object LabelStrategy extends EnumerationClass[LabelStrategy] {
      val name = "label"

      val elements = Vector(NameLabel, LabelLabel)
    }
  }

  case class Builder(
    i18n: I18NContext,
    header: HeaderStrategy,
    schema: Schema2
  ) {
    def build(ps: RecordSequence): Table = build(ps.irecords)

    def build(ps: Seq[IRecord]): Table = {
      val head = _build_head
      val body = _build_body(ps)
      Table(RecordData(body), MetaData(schema), Some(head))
    }

    private def _build_head = {
      val xs = schema.columns.map(_to_cell)
      Head(xs.toList)
    }

    private def _to_cell(p: Column2): Cell = {
      val s = header.label match {
        case HeaderStrategy.NameLabel => p.name
        case HeaderStrategy.LabelLabel => p.labelI18NString(i18n.locale)
      }
      Cell(s)
    }

    private def _build_body(ps: Seq[IRecord]) = {
      ps.map(_.toRecord).map(_normalize).toVector
    }

    private def _normalize(p: Record) = {
      case class Z(xs: Vector[Field] = Vector.empty) {
        def r = p.copy(fields = xs)

        def +(rhs: Column2) = {
          val names = rhs.nameCandidates
          p.fields.find(x => names.contains(x.name)).
            map { f =>
              val label = rhs.labelI18NString(i18n.locale)
              val a = f.withKey(label)
              copy(xs = xs :+ a)
            }.getOrElse(this)
        }
      }
      schema.columns./:(Z())(_+_).r
    }
  }
}
