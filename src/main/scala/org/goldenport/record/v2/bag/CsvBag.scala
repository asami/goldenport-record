package org.goldenport.record.v2.bag

import scalaz._, Scalaz._
import scalaz.stream._
import scalaz.stream.process1.lift
import scalaz.concurrent.Task
import scalax.io._
import java.io.{Writer, InputStream, File}
import java.sql.ResultSet
import java.nio.charset.Charset
import au.com.bytecode.opencsv.CSVWriter
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.matrix._
import org.goldenport.table.ITable
import org.goldenport.Platform.codec.{UTF8, WINDOWS31J}
import org.goldenport.io.ResourceHandle
import org.goldenport.csv.CsvLineMaker
import org.goldenport.bag._
import org.goldenport.record.v2.util.CsvUtils
import RecordBag._

/*
 * @since   Jun.  7, 2014
 *  version Jul. 25, 2014
 *  version Oct. 15, 2014
 *  version Nov. 13, 2014
 *  version Dec. 19, 2014
 *  version Sep. 28, 2015
 *  version Oct. 27, 2015
 *  version Feb. 29, 2016
 *  version Mar.  2, 2016
 *  version Aug. 31, 2016
 *  version Sep. 22, 2016
 *  version Aug. 30, 2017
 *  version Sep.  2, 2017
 *  version Jan. 23, 2018
 *  version Jul. 18, 2018
 *  version Feb. 12, 2019
 *  version Aug. 18, 2019
 * @version Jan. 28, 2020
 * @author  ASAMI, Tomoharu
 */
class CsvBag(
  val bag: ChunkBag = new BufferFileBag(),
//  val codec: Codec = WINDOWS31J,
//  val schema: Option[Schema] = None,
//  val headerPolicy: RecordBag.HeaderPolicy = RecordBag.noHeader,
  // val recordStrategy: RecordBag.Strategy = RecordBag.Strategy.plainAuto,
  // override val name: String = "data",
  // val lineEnd: String = "\r\n",
  // val isForceDoubleQuote: Boolean = false
  val csvStrategy: CsvBag.Strategy = CsvBag.Strategy.legacy
) extends RecordBag {
  import CsvBag._

  override def name = csvStrategy.name
  def strategy = csvStrategy.recordBagStrategy
  def lineEnd = csvStrategy.lineEnd
  def isForceDoubleQuote = csvStrategy.isForceDoubleQuote

  def chunkBag = bag
  override def filenameSuffix = Some("csv")
  override def mimetype: String = Strings.mimetype.text_csv

  protected def generate_Schema(): Option[Schema] = {
    RecordBag.getSchema(bag, strategy, CsvUtils.parseLine)
  }

  // private def _schema: Schema = {
  //   schema getOrElse {
  //     throw new IllegalArgumentException(s"The operation requires a schema.")
  //   }
  // }

  private var _count: Option[Int] = None
  def count: Int = {
    _count getOrElse {
      val c = calc_count
      _count = Some(c)
      c
    }
  }

  def dataCount: Int = {
    if (headerPolicy.physical)
      count - 1
    else
      count
  }

  private var _is_empty = true

  // Override resource, need not dispose in this point.
  def changeName(n: String): CsvBag = {
    val a = new CsvBag(bag, csvStrategy.withName(n))
    a._count = _count
    a._is_empty = _is_empty
    a
  }

  def withSchema(s: Schema) = {
//    new CsvBag(bag, codec, Some(s), headerPolicy, lineEnd).asInstanceOf[this.type]
    RAISE.notImplementedYetDefect
  }

  protected def add_count(c: Int) {
    _count match {
      case Some(s) => _count = Some(s + c)
      case None => _count = Some(c)
    }
  }

  protected def count_up() {
    add_count(1)
  }

  def isNoSchema = !_is_schema_available

  def isEmpty = bag.isEmpty
  def nonEmpty = bag.nonEmpty

  // override def toMatrixDouble: IMatrix[Double] = {
  //   val a = vectorsR.map(_.map(_.toDouble)).runLog.run
  //   VectorRowColumnMatrix(a.toVector)
  // }

  // override def dataVectorsR: Process[Task, Vector[Any]] = super.dataVectorsR

  // override def toTable: ITable = to_record_table

  def openWriter(): Writer = {
    val writer = bag.openWriter(codec)
    _complement_physical_header(writer)
    writer
  }

  private def _complement_physical_header(writer: Writer) {
    if (headerPolicy.isComplementPhysicalHeader && !isNoSchema && _is_empty) {
      assume (_count.isEmpty, "count should be None")
      CsvUtils.writeHeader(writer, schema, lineEnd, isForceDoubleQuote)
      count_up()
    }
  }

  private def _complement_physical_header_unsupported {
    throw new UnsupportedOperationException("Complement physical header feature is unsupported.")
  }

  def write(lines: String) {
    _complement_physical_header_unsupported
    bag.write(lines, codec)
    add_count(lines.length)
  }

  def write(lines: Seq[Seq[String]]) {
    val writer = openWriter
    try {
      get_schema_in_init match {
        case Some(s) => throw new UnsupportedOperationException("Not support schema ")
        case None => CsvUtils.write(writer, lineEnd, lines, isForceDoubleQuote)
      }
      add_count(lines.length)
    } finally {
      writer.close()
    }
  }

  def write(rs: ResultSet) {
    val writer = openWriter
    try {
      val n = get_schema_in_init match {
        case Some(s) => throw new UnsupportedOperationException("Not support schema ")
        case None => CsvUtils.write(writer, lineEnd, rs, isForceDoubleQuote)
      }
      add_count(n)
    } finally {
      writer.close()
    }
  }

  def writeRecords(rs: Iterator[Record]) {
    val writer = openWriter
    try {
      val n = if (is_no_schema_in_init)
        CsvUtils.write(writer, lineEnd, rs, isForceDoubleQuote)
      else
        CsvUtils.write(writer, schema, lineEnd, rs, isForceDoubleQuote)
      add_count(n)
    } finally {
      writer.close()
    }
  }

  def writeRecordsWithSchema(rs: Iterator[Record]) {
    val writer = openWriter
    try {
      val n = CsvUtils.write(writer, schema, lineEnd, rs, isForceDoubleQuote)
      add_count(n)
    } finally {
      writer.close()
    }
  }

  def openAppender() = {
    val writer = openWriter
    new AppenderImpl(writer)
  }

  private def _is_schema_available = getSchema.nonEmpty

  // Caution: compatible semantics for ChunkBag
  def linesR: Process[Task, String] = {
    def f(s: (String, Int)): Vector[String] = {
      if (s._2 == 0)
        Vector(CsvUtils.makeHeader(schema, isForceDoubleQuote), s._1)
      else
        Vector(s._1)
    }
    if (headerPolicy.isComplementDisplayHeader) {
      require (_is_schema_available, "Making header requires the schema.")
      bag.linesR(codec).zipWithIndex.map(f).flatMap(Process.emitAll)
    } else {
      bag.linesR(codec)
    }
  }

  def stringsR: Process[Task, String] = {
    def f(s: (String, Int)): Vector[String] = {
      if (s._2 == 0)
        Vector(CsvUtils.makeHeader(schema, isForceDoubleQuote), s._1)
      else
        Vector(s._1)
    }
    if (headerPolicy.isComplementDisplayHeader) {
      require (_is_schema_available, "Making header requires the schema.")
      bag.stringsR(codec).zipWithIndex.map(f).flatMap(Process.emitAll)
    } else {
      bag.stringsR(codec)
    }
  }

  def stringsEotR: Process[Task, String] = 
    stringsR ++ Process(CsvLineMaker.EOTString)

  // Single-line
  // TODO Multi-line
  override def vectorsR: Process[Task, Vector[String]] = {
    linesR.map(CsvUtils.parseLine)
  }

  // Multi-line
  def optionVectorsR: Process[Task, Vector[Option[String]]] =
    stringsEotR.pipe(CsvLineMaker.fsmInit)

  // Single-line
  // TODO Multi-line
  def tupleVectorsR: Process[Task, Vector[(String, String)]] = {
    vectorsR.map(CsvUtils.makeTupleVector(schema, _))
  }

  // Single-line
  // TODO Multi-line
  def mapsR: Process[Task, Map[String, String]] = {
    vectorsR.map(CsvUtils.makeMap(schema, _))
  }

  def recordsR: Process[Task, Record] = getSchema.
    map(s =>
      if (strategy.eliminateEmptyColumn)
        optionVectorsR.map(CsvUtils.makeRecordFlatten(s, _))
      else
        optionVectorsR.map(CsvUtils.makeRecordNullable(s, _))
    ).getOrElse(
      optionVectorsR.map(CsvUtils.makeRecord(_))
    )

  def mapW: Sink[Task, Map[String, String]] = {
    require (!isNoSchema, "mapW requires a schema")
    def acquire = Task.delay {
      val writer = openWriter
      writer
    }
    def release(writer: Writer) = Task.delay(writer.close())
    io.resource(acquire)(release) { writer =>
      Task.now((map: Map[String, String]) => Task.delay {
        CsvUtils.append(writer, schema, lineEnd, map, isForceDoubleQuote)
        add_count(map.size)
      })
    }
  }

  private def _acquire = Task.delay {
    val writer = openWriter
    writer
  }

  private def _release(writer: Writer) = Task.delay(writer.close())

  def recordW: Sink[Task, Record] = {
    io.resource(_acquire)(_release) { writer =>
      Task.now((rec: Record) => Task.delay {
        val n = if (is_no_schema_in_init)
          CsvUtils.append(writer, lineEnd, rec, isForceDoubleQuote)
        else
          CsvUtils.append(writer, schema, lineEnd, rec, isForceDoubleQuote)
        count_up()
      })
    }
  }

  def recordsW: Sink[Task, Seq[Record]] = {
    io.resource(_acquire)(_release) { writer =>
      Task.now((rs: Seq[Record]) => Task.delay {
        for (rec <- rs)
          if (is_no_schema_in_init)
            CsvUtils.append(writer, lineEnd, rec, isForceDoubleQuote)
          else
            CsvUtils.append(writer, schema, lineEnd, rec, isForceDoubleQuote)
        add_count(rs.length)
      })
    }
  }

  def toRecords: IndexedSeq[Record] = {
    recordsR.runLog.run
  }

  def selectColumn(no: Int): Seq[String] = {
    vectorsR.map(_(no)).runLog.run
  }

  def selectColumn(no: Int, p: Vector[String] => Boolean): Seq[String] = {
    vectorsR.filter(p).map(_(no)).runLog.run
  }

  def selectColumn(name: String): Seq[String] = {
    recordsR.map(_.getString(name) getOrElse "").runLog.run
  }

  def selectColumn(name: String, p: Record => Boolean): Seq[String] = {
    recordsR.filter(p).map(_.getString(name) getOrElse "").runLog.run
  }

  def size: Long = bag.size

  def dispose() {
    bag.dispose()
  }

  def getHeader: Option[Seq[String]] = {
    getSchema.map(_.header)
  }

  protected def calc_count: Int = {
    bag.linesR(codec).runFoldMap(x => 1).run
  }

  def save(file: File): Unit = {
    val out = FileBag.create(file)
    for {
      x <- resource.managed(out)
    } yield {
      bag.copyTo(x.chunkW)
    }
  }

  class AppenderImpl(writer: Writer) extends Appender {
    val out = new CSVWriter(writer, CSVWriter.DEFAULT_SEPARATOR, CSVWriter.DEFAULT_QUOTE_CHARACTER, lineEnd)

    def append(rec: Record) {
      get_schema_in_init match {
        case Some(s) => out.writeNext(CsvUtils.record2Values(rec, s, isForceDoubleQuote))
        case None => out.writeNext(CsvUtils.record2Values(rec, isForceDoubleQuote))
      }
      count_up()
    }

    def append(values: Seq[Any]) {
      get_schema_in_init match {
        case Some(s) => throw new UnsupportedOperationException("Not support schema ")
        case None => out.writeNext(values.map(CsvUtils.toCsvValue(_, isForceDoubleQuote)).toArray)
      }
      count_up()
    }

    def appendStrings(values: Seq[String]) {
      get_schema_in_init match {
        case Some(s) => throw new UnsupportedOperationException("Not support schema ")
        case None => out.writeNext(values.toArray)
      }
      count_up()
    }

    def close() {
      out.flush()
      out.close()
      writer.close()
    }
  }
}

object CsvBag {
  import RecordBag.Strategy

  val empty = new CsvBag(EmptyBag)

  case class Strategy(
    recordBagStrategy: RecordBag.Strategy,
    name: String,
    lineEnd: String,
    isForceDoubleQuote: Boolean
  ) {
    def codec = recordBagStrategy.codec
    def headerPolicy = recordBagStrategy.headerPolicy
    def schema = recordBagStrategy.schema
    def eliminateEmptyColumn = recordBagStrategy.eliminateEmptyColumn

    def withName(p: String) = copy(name = p)
    def withIsForceDoubleQuote(p: Boolean) = copy(isForceDoubleQuote = p)

    def update(
      bag: Option[RecordBag.Strategy],
      name: Option[String],
      lineEnd: Option[String],
      isForceDoubleQuote: Option[Boolean]
    ): Strategy = Strategy(
      bag getOrElse this.recordBagStrategy,
      name getOrElse this.name,
      lineEnd getOrElse this.lineEnd,
      isForceDoubleQuote getOrElse this.isForceDoubleQuote
    )

    def withSchema(p: Schema) = copy(recordBagStrategy = recordBagStrategy.withSchema(p))
  }
  object Strategy {
    val default = Strategy(
      RecordBag.Strategy.plainAuto,
      "data",
      "\n",
      false
    )
    val legacy = Strategy(
      RecordBag.Strategy.plainAuto,
      "data",
      "\r\n", // legacy
      false
    )
    val matrixAuto = Strategy(
      RecordBag.Strategy.matrixAuto,
      "data",
      "\n",
      false
    )

    def legacy(
      p: RecordBag.Strategy
    ): Strategy = legacy.copy(p)

    def legacy(
      p: RecordBag.Strategy,
      name: String
    ): Strategy = legacy.copy(p, name = name)

    def legacy(
      p: RecordBag.Strategy,
      name: Option[String],
      isForceDoubleQuote: Boolean
    ): Strategy = name.cata(
      x => legacy.copy(p, name = x, isForceDoubleQuote = isForceDoubleQuote),
      legacy.copy(p, isForceDoubleQuote = isForceDoubleQuote)
    )
  }

  def create(
    bag: ChunkBag,
    strategy: Strategy
  ): CsvBag = new CsvBag(bag, strategy)

  def create(
    bag: Option[ChunkBag],
    codec: Option[Codec],
    schema: Option[Schema],
    withHeader: Option[Boolean],
    name: Option[String],
    isforcedoublequote: Option[Boolean]
  ): CsvBag = {
    val b = bag getOrElse new BufferFileBag()
    val c = codec getOrElse RecordBag.Strategy.UTF8
    val h = withComplementPhysicalHeader(withHeader | false)
    val a = schema match {
      case Some(s) => SpecificSchema(s)
      case None => AutoSchema
    }
    val eliminate = false
    val rstrategy = RecordBag.Strategy(c, h, a, eliminate)
    val dqp = isforcedoublequote getOrElse false
    val strategy = Strategy.legacy(rstrategy, name, dqp)
    new CsvBag(b, strategy)
  }

  def create(file: File, strategy: Strategy): CsvBag = create(FileBag.create(file), strategy)

  // def create(
  //   bag: ChunkBag,
  //   codec: Codec,
  //   schema: Option[Schema],
  //   withHeader: Boolean
  // ): CsvBag = {
  //   create(bag, codec, schema, withComplementPhysicalHeader(withHeader))
  // }

  def createComplementPhysicalHeader(
    bag: ChunkBag,
    codec: Codec,
    schema: Schema
  ): CsvBag = {
    create(bag, codec, Some(schema), complementPhysicalHeader)
  }

  def createNaturalHeader(
    bag: ChunkBag,
    codec: Codec
  ): CsvBag = {
    create(bag, codec, None, naturalHeader)
  }

  def create(
    bag: ChunkBag,
    codec: Codec,
    schema: Option[Schema],
    header: HeaderPolicy
  ): CsvBag = {
    require (header.complementPhysicalHeader --> schema.isDefined,
      "Complementing a phsycal header needs a schema.")
    val a = schema match {
      case Some(s) => SpecificSchema(s)
      case None => AutoSchema
    }
    val eliminate = false
    val strategy = RecordBag.Strategy(codec, header, a, eliminate)
    create(bag, strategy)
  }

  def create(bag: ChunkBag, strategy: RecordBag.Strategy): CsvBag = {
    new CsvBag(bag, Strategy.legacy(strategy))
  }

  def create(bag: ChunkBag, strategy: RecordBag.Strategy, name: String): CsvBag = {
    new CsvBag(bag, Strategy.legacy(strategy, name))
  }

  def createNoHeader(bag: ChunkBag, schema: Schema): CsvBag = {
    create(bag, RecordBag.Strategy.UTF8, Some(schema), noHeader)
  }

  def create(schema: Schema): CsvBag = {
    create(RecordBag.Strategy.UTF8, schema) // Use UTF-8 instead of WINDOWS31J as default
  }

  // def create(schema: Schema, header: HeaderPolicy): CsvBag = {
  //   val b = new BufferFileBag()
  //   val c = Strategy.UTF8
  //   val a = SpecificSchema(schema)
  //   val strategy = Strategy(c, header, a)
  //   create(b, strategy)
  // }

  def create(codec: Codec, schema: Schema): CsvBag = {
    create(codec, Some(schema))
  }

  // def create(schema: Schema, name: String): CsvBag = {
  //   create(None, None, Some(schema), None, Some(name))
  // }

  def create(
    codec: Codec,
    schema: Option[Schema]
  ): CsvBag = {
    create(new BufferFileBag(), codec, schema, noHeader)
  }

  def createForDownload(schema: Schema): CsvBag = {
    val codec = WINDOWS31J
    create(BufferFileBag.create(codec), RecordBag.Strategy.WINDOWS31J, Some(schema), complementPhysicalHeader)
  }

  def createForDownload(schema: Schema, name: String): CsvBag = {
    val codec = WINDOWS31J
    create(Some(BufferFileBag.create(codec)), Some(RecordBag.Strategy.WINDOWS31J), Some(schema), Some(true), Some(name), None)
  }

  def createForDownload(
    schema: Schema,
    name: String,
    charset: String,
    isforcedoublequote: Boolean
  ): CsvBag = {
    val codec = Codec(Charset forName charset)
    create(Some(BufferFileBag.create(codec)), Some(codec), Some(schema), Some(true), Some(name), Some(isforcedoublequote))
  }

  // legacy
  def createForDownload(schema: Schema, header: HeaderPolicy): CsvBag = {
    val codec = WINDOWS31J
    val b = BufferFileBag.create(codec)
    val c = RecordBag.Strategy.WINDOWS31J
    val a = SpecificSchema(schema)
    val eliminate = false
    val strategy = RecordBag.Strategy(c, header, a, false)
    create(b, strategy)
  }

  def load(
    uri: java.net.URI,
    strategy: Strategy = Strategy.default
  ): CsvBag = loadUri(uri.toString, strategy)

  def loadUri(
    uri: String,
    strategy: Strategy = Strategy.default
  ): CsvBag = {
    val url = UURL.getURLFromFileOrURLName(uri)
    val bag = BufferFileBag.create(strategy.codec)
    for {
      in <- resource.managed(url.openStream())
    } {
      bag.write(in)
    }
    create(bag, strategy)
  }

  def loadResource(rh: ResourceHandle, strategy: Strategy = Strategy.default): CsvBag = {
    val bag = BufferFileBag.create(strategy.codec)
    for {
      in <- resource.managed(rh.openInputStream())
    } {
      bag.write(in)
    }
    create(bag, strategy)
  }

  // legacy because of WINDOWS31J
  def fromUri(
    uri: String,
    schema: Option[Schema] = None,
    header: HeaderPolicy = noHeader,
    codec: Codec = WINDOWS31J
  ): CsvBag = {
    val url = UURL.getURLFromFileOrURLName(uri)
    val bag = BufferFileBag.create(codec)
    for {
      in <- resource.managed(url.openStream())
    } {
      bag.write(in)
    }
    create(bag, codec, schema, header)
  }

  def fromInputStream(
    in: InputStream,
    schema: Option[Schema] = None,
    header: HeaderPolicy = noHeader,
    codec: Codec = WINDOWS31J
  ): CsvBag = {
    val bag = BufferFileBag.create(codec)
    bag.write(in)
    create(bag, codec, schema, header)
  }

  def fromRecords(
    rs: Seq[Record],
    schema: Option[Schema] = None,
    header: HeaderPolicy = noHeader,
    codec: Codec = WINDOWS31J
  ): CsvBag = {
    val bag = BufferFileBag.create(codec)
    val csv = create(bag, codec, schema, header)
    csv.writeRecords(rs)
    csv
  }

  def fromRecordsIterator(
    rs: Iterator[Record],
    schema: Option[Schema] = None,
    header: HeaderPolicy = noHeader,
    codec: Codec = WINDOWS31J
  ): CsvBag = {
    val bag = BufferFileBag.create(codec)
    val csv = create(bag, codec, schema, header)
    csv.writeRecords(rs)
    csv
  }
}
