package org.goldenport.record.v2.bag

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import scalax.io.Codec
import scala.util.Try
import org.goldenport.RAISE
import org.goldenport.bag._
import org.goldenport.values.FileName
import org.goldenport.matrix._
import org.goldenport.table.ITable
import org.goldenport.util.{StringUtils, AnyUtils => LAnyUtils}
import org.goldenport.record.v2._
import org.goldenport.record.v2.projector.Projector
import org.goldenport.record.v3.{ITable => ITable3, Table => Table3, Record => Record3, RecordSequence}
import org.goldenport.record.util.AnyUtils

/*
 * @since   Sep. 21, 2015
 *  version Oct. 27, 2015
 *  version Aug.  1, 2016
 *  version Sep. 22, 2016
 *  version Aug. 30, 2017
 *  version Feb. 12, 2019
 *  version Aug.  3, 2019
 * @version Jan. 30, 2020
 * @author  ASAMI, Tomoharu
 */
trait RecordBag extends Bag {
  import RecordBag._

  def getChunkBag = Some(chunkBag)
  def createChunkBag = chunkBag.copyTo(new BufferFileBag())
  def chunkBag: ChunkBag
  def strategy: RecordBag.Strategy

  def headerPolicy: RecordBag.HeaderPolicy = strategy.headerPolicy
  def codec = strategy.codec
  override def getCodec = Some(codec)

  lazy val getSchema: Option[Schema] = {
    strategy.schema match {
      case NoSchema => None
      case AutoSchema =>
        if (chunkBag.isEmpty)
          throw new IllegalStateException("RecordBag#getSchema producing schema requires non empty data.")
        else
          generate_Schema()
      case SpecificSchema(s) => Some(s)
    }
  }

  protected def generate_Schema(): Option[Schema]

  def schema: Schema = getSchema getOrElse {
    throw new IllegalStateException("Missing schema")
  }

  protected def get_schema_in_init: Option[Schema] = {
    strategy.schema match {
      case NoSchema => None
      case AutoSchema => None
      case SpecificSchema(s) => Some(s)
    }
  }

  protected def is_no_schema_in_init = {
    get_schema_in_init.isEmpty
  }

  def withSchema(schema: Schema): this.type

  def dispose(): Unit

  // header + data - config
  def recordsR: Process[Task, Record]

  // header + data
  def headerDataRecordsR: Process[Task, Record] = {
    RAISE.notImplementedYetDefect
  }

  // data only
  def dataRecordsR: Process[Task, Record] =
    if (headerPolicy.physical)
      recordsR.drop(1)
    else if (headerPolicy.autoMatrixHeader)
      recordsR.zipWithIndex.collect(_auto_matrix_header)
    else
      recordsR

  private def _auto_matrix_header: PartialFunction[(Record, Int), Record] = {
    case (rec, index) if !_is_header(rec, index) => rec
  }

  private def _is_header(rec: Record, index: Int): Boolean =
    index == 0 && rec.fields.exists(x => _is_matrix_header_column(x.getOne))

  // private def _is_header_column(p: Option[String]) =
  //   p.fold(false)(x => Try(x.toDouble).toOption.isEmpty) // XXX number (rational, complex)

  def dataRecordsR(schema: Schema): Process[Task, \/[ValidationResult, Record]] = {
    dataRecordsR.map(Projector(schema).apply)
  }

  def records: IndexedSeq[Record] = recordsR.runLog.run
  def headerDataRecords: IndexedSeq[Record] = headerDataRecordsR.runLog.run
  def dataRecords: IndexedSeq[Record] = dataRecordsR.runLog.run
  def dataRecordSequence: RecordSequence = RecordSequence(dataRecords.map(Record3.create))

  def rowVectorDoubleIterator: Iterator[Vector[Double]] = toMatrixDouble.rowIterator
  def columnVectorDoubleIterator: Iterator[Vector[Double]] = toMatrixDouble.columnIterator

  def toMatrixString: IMatrix[String] = toMatrix(AnyUtils.toString, "")
  def toMatrixDouble: IMatrix[Double] = toMatrix(AnyUtils.toDouble, 0.0)
  // def toMatrixDouble: IMatrix[Double] = {
  //   val schema = getSchema getOrElse RAISE.unsupportedOperationFault("No schema")
  //   def tovector(rec: Record): Vector[Double] = {
  //     schema.columns./:(Vector.empty[Double])((z, x) =>
  //       z :+ (rec.getDouble(x.name) getOrElse 0.0)
  //     )
  //   }
  //   VectorRowColumnMatrix(dataRecords.toVector.map(tovector))
  // }

  def toMatrix[T](f: Any => T, empty: T): IMatrix[T] = {
    val xs = dataVectors.map(_.map(f))
    VectorRowColumnMatrix(xs.toVector)
  }

  def toMatrixByField[T](f: Field => T, empty: T): IMatrix[T] = {
    val g = getSchema.
      map(schema => _to_vector(schema, f, empty) _).
      getOrElse(_to_vector(f, empty) _)
    VectorRowColumnMatrix(dataRecords.toVector.map(g))
  }

  private def _to_vector[T](schema: Schema, f: Field => T, empty: T)(rec: Record): Vector[T] = {
    schema.columns./:(Vector.empty[T])((z, x) =>
      z :+ (rec.getField(x.name).map(f).getOrElse(empty)))
  }

  private def _to_vector[T](f: Field => T, empty: T)(rec: Record): Vector[T] = rec.fields.map(f).toVector

  // def dataVectorsR: Process[Task, Vector[Any]] = {
  //   val schema = getSchema getOrElse RAISE.unsupportedOperationFault("No schema")
  //   dataRecordsR.map(rec =>
  //     schema.columns.toVector./:(Vector.empty[Any])((z, x) =>
  //       z :+ (rec.getOne(x.name) getOrElse "")
  //     )
  //   )
  // }

  def vectorsR: Process[Task, Vector[Any]] = {
    val schema = getSchema getOrElse RAISE.unsupportedOperationFault("No schema")
    recordsR.map(rec =>
      schema.columns.toVector./:(Vector.empty[Any])((z, x) =>
        z :+ (rec.getOne(x.name) getOrElse "")
      )
    )
  }

  def dataVectorsR: Process[Task, Vector[Any]] =
    if (headerPolicy.physical)
      vectorsR.drop(1)
    else if (headerPolicy.autoMatrixHeader)
      vectorsR.zipWithIndex.collect(_auto_matrix_header_for_vector)
    else
      vectorsR

  private def _auto_matrix_header_for_vector: PartialFunction[(Vector[Any], Int), Vector[Any]] = {
    case (rec, index) if !_is_matrix_header(rec, index) => rec
  }

  private def _is_matrix_header(rec: Vector[Any], index: Int): Boolean =
    index == 0 && rec.exists(_is_matrix_header_column)

  private def _is_matrix_header_column(p: Any): Boolean = p match {
    case m: String => !StringUtils.isNumberWide(m)
    case m => !LAnyUtils.isNumber(m)
  }

  def dataVectors: IndexedSeq[Vector[Any]] = dataVectorsR.runLog.run

  def dataVectorsDouble: IndexedSeq[Vector[Double]] = dataVectorsR.map(_.map(AnyUtils.toDouble)).runLog.run

  // def toTable: ITable = to_record_table
  def toTable: ITable3 = to_record_table

  protected final def to_record_table: ITable3 = Table3.create(getSchema, dataRecordSequence)

// protected final def to_vector_table: ITable3 = Table3.createSeqSeq(getSchema, dataVectors)

  def recordW: Sink[Task, Record]
  def recordsW: Sink[Task, Seq[Record]]

  def write(rs: java.sql.ResultSet): Unit
  def writeRecords(rs: Seq[Record]) {
    writeRecords(rs.iterator)
  }
  def writeRecords(rs: Iterator[Record]): Unit
  def writeRecordsWithSchema(rs: Iterator[Record]): Unit
  def openAppender(): Appender

  def getViews: List[RecordBagView] = List(FlatRecordBagView(strategy, this))
}

object RecordBag {
  def create(
    filename: String,
    bag: ChunkBag,
    strategy: PartialFunction[FileName, Strategy]
  ): RecordBag = {
    RAISE.notImplementedYetDefect
  }

  def getSchema(
    bag: ChunkBag,
    strategy: Strategy,
    fieldsf: String => Seq[String]
  ): Option[Schema] = {
    strategy.schema match {
      case NoSchema => None
      case AutoSchema =>
        if (bag.isEmpty)
          throw new IllegalArgumentException("RecordBag#getSchema data has been empty yet.")
        else
          Some(_generate_schema(bag, strategy, fieldsf))
      case SpecificSchema(s) => Some(s)
    }
  }

  private def _generate_schema(
    bag: ChunkBag,
    strategy: RecordBag.Strategy,
    fieldsf: String => Seq[String]
  ): Schema = {
    val codec = strategy.codec
    val a = bag.linesR(codec).take(1).runLast.run
    a match {
      case Some(s) => 
        val b = for (x <- fieldsf(s)) yield {
          Column(x)
        }
        Schema(b, isAutoLabel = false)
      case None => Schema.empty
    }
  }

  sealed trait SchemaStrategy
  case object NoSchema extends SchemaStrategy
  case object AutoSchema extends SchemaStrategy
  case class SpecificSchema(schema: Schema) extends SchemaStrategy

  case class Strategy(
    codec: Codec,
    headerPolicy: HeaderPolicy,
    schema: SchemaStrategy,
    eliminateEmptyColumn: Boolean
  ) {
    def update(
      codec: Option[Codec],
      headerPolicy: Option[HeaderPolicy],
      schema: Option[SchemaStrategy],
      eliminateEmptyColumn: Option[Boolean]
    ): Strategy = Strategy(
      codec getOrElse this.codec,
      headerPolicy getOrElse this.headerPolicy,
      schema getOrElse this.schema,
      eliminateEmptyColumn getOrElse this.eliminateEmptyColumn
    )

    def withSchema(p: Schema) = copy(schema = SpecificSchema(p))
  }

  object Strategy {
    val UTF8 = org.goldenport.Platform.codec.UTF8
    val WINDOWS31J = org.goldenport.Platform.codec.WINDOWS31J
    def simple = Strategy(Codec.UTF8, noHeader, NoSchema, false)
    def windowsAuto = Strategy(WINDOWS31J, naturalHeader, AutoSchema, false)
    def virtualAuto = Strategy(Codec.UTF8, virtualHeader, AutoSchema, false)
    def plainAuto = Strategy(Codec.UTF8, naturalHeader, AutoSchema, false)
    def matrixAuto = Strategy(Codec.UTF8, matrixHeader, NoSchema, false)
    def naturalRead(schema: Schema) = Strategy(Codec.UTF8, naturalHeader, SpecificSchema(schema), false)
    def naturalWrite(schema: Schema) = Strategy(Codec.UTF8, complementPhysicalHeader, SpecificSchema(schema), false)

    object creator {
      import org.goldenport.util.StringUtils.isSuffix
      def apply(charset: Option[String]): PartialFunction[FileName, Strategy] =
        charset.fold(windows)(apply)

      def apply(charset: String): PartialFunction[FileName, Strategy] =
        charset.toLowerCase match {
          case "windows-31j" => windows
          case "utf-8" => utf8
        }

      def windows: PartialFunction[FileName, Strategy] = {
        case s if isSuffix(s.v, "csv") => windowsAuto
        case s if isSuffix(s.v, "json") => virtualAuto
        case s if isSuffix(s.v, Vector("xls", "xlsx")) => windowsAuto
      }

      def utf8: PartialFunction[FileName, Strategy] = {
        case s if isSuffix(s.v, "csv") => plainAuto
        case s if isSuffix(s.v, "json") => virtualAuto
        case s if isSuffix(s.v, Vector("xls", "xlsx")) => windowsAuto
      }
    }
  }

  case class HeaderPolicy(
    logical: Boolean,
    physical: Boolean,
    complementPhysicalHeader: Boolean,
    autoMatrixHeader: Boolean
  ) {
    // in read
    def isComplementDisplayHeader = logical && !physical
    // in write
    def isComplementPhysicalHeader = physical && complementPhysicalHeader
  }

  def withComplementPhysicalHeader(p: Boolean) = {
    if (p)
      complementPhysicalHeader
    else
      noHeader
  }

  val noHeader = HeaderPolicy(false, false, false, false)
  val naturalHeader = HeaderPolicy(true, true, false, false)
  val virtualHeader = HeaderPolicy(true, false, false, false)
  val complementPhysicalHeader = HeaderPolicy(true, true, true, false)
  val matrixHeader = HeaderPolicy(false, false, false, true)

  abstract class Appender {
    def append(rec: Record): Unit
    def append(values: Seq[Any]): Unit
    def appendStrings(values: Seq[String]): Unit
    def close(): Unit
  }
}
