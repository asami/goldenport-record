package org.goldenport.record.v2.util

import scalaz._, Scalaz._
import scalaz.stream._
import java.io.Writer
import java.sql.ResultSet
import au.com.bytecode.opencsv._
import org.joda.time.{DateTime, DateTimeZone}
import org.goldenport.util.{DateTimeUtils, DateUtils}
import org.goldenport.record.v2._
import org.goldenport.record.command.NullValue
import com.asamioffice.goldenport.text.UString

/*
 * @since   Jun. 10, 2014
 *  version Jul. 25, 2014
 *  version Sep. 25, 2015
 *  version Oct. 15, 2015
 *  version Nov. 26, 2015
 *  version Mar.  1, 2016
 *  version Sep. 22, 2016
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
object CsvUtils {
  private lazy val _parser = new CSVParser()

  def makeLine(xs: Seq[String]): String = {
    xs.map(makeField).mkString(",")
  }

  def makeField(x: String): String = {
    def f(s: String) = s.replaceAllLiterally("\"", "\"\"")
    if (x.contains(',') || x.contains('"') || x.contains('\n') || x.contains('\r'))
      "\"" + f(x) + "\""
    else
      x
  }

  def writeHeader(writer: Writer, schema: Schema, lineend: String) {
    writer.write(makeHeader(schema))
    writer.write(lineend)
  }

  private def _create_csv_writer(writer: Writer, lineend: String) = {
    new CSVWriter(
      writer,
      CSVWriter.DEFAULT_SEPARATOR,
      CSVWriter.DEFAULT_QUOTE_CHARACTER,
      lineend
    )
  }

  def write(writer: Writer, lineend: String, lines: Seq[Seq[String]]) {
    import scala.collection.JavaConverters._
    for (out <- resource.managed(_create_csv_writer(writer, lineend))) {
      val a = lines.map(_.toArray).asJava
      out.writeAll(a)
    }
  }

  def write(writer: Writer, lineend: String, rs: ResultSet): Int = {
    var count = 0
    for (out <- resource.managed(_create_csv_writer(writer, lineend))) {
      out.writeAll(rs, false)
      count += 1
    }
    count
  }

  def write(writer: Writer, lineend: String, rs: Iterator[Record]): Int = {
    var count = 0
    for (out <- resource.managed(_create_csv_writer(writer, lineend))) {
      for (rec <- rs) {
        out.writeNext(record2Values(rec))
        count += 1
      }
    }
    count
  }

  def write(writer: Writer, schema: Schema, lineend: String, rs: Iterator[Record]): Int = {
    var count = 0
    for (out <- resource.managed(_create_csv_writer(writer, lineend))) {
      for (rec <- rs) {
        out.writeNext(record2Values(rec, schema))
        count += 1
      }
    }
    count
  }

  def append(writer: Writer, schema: Schema, lineend: String, map: Map[String, String]) {
    val line = makeLine(map2Values(map, schema))
    writer.write(line)
    writer.write(lineend)
  }

  def append(writer: Writer, lineend: String, rec: Record) {
    val line = makeLine(record2Values(rec))
    writer.write(line)
    writer.write(lineend)
  }

  def append(writer: Writer, schema: Schema, lineend: String, rec: Record) {
    val line = makeLine(record2Values(rec, schema))
    writer.write(line)
    writer.write(lineend)
  }

  def getValue(rec: Record, key: String): String = {
    getValue(rec, Symbol(key))
  }

  def getValue(rec: Record, key: Symbol): String = {
//    rec.getOne(key).map(toCsvValue(_)) getOrElse ""
    rec.getList(key) match {
      case Nil => ""
      case x :: Nil => toCsvValue(x)
      case xs => toCsvValue(xs)
    }
  }

  def record2Values(rec: Record): Array[String] = {
    val keys = rec.fields.map(_.key)
    keys.map(x => getValue(rec, x)).toArray
  }

  def record2Values(rec: Record, schema: Schema): Array[String] = {
    val keys = schema.columns.map(_.name)
    keys.map(x => getValue(rec, x)).toArray
  }

  def map2Values(map: Map[String, String], schema: Schema): Array[String] = {
    def getvalue(key: String) = map.get(key).orZero
    val keys = schema.columns.map(_.name)
    keys.map(getvalue).toArray
  }

  def parseLine(line: String): Vector[String] = {
    _parser.parseLine(line).toVector
  }

  // def parseOptionFields(chunk: String): Vector[Option[String]] = {
  //   CsvLineMaker.parse(line)
  // }

  def makeHeader(schema: Schema): String = {
    makeLine(schema.header)
  }

  def makeTupleVector(schema: Schema, fields: Seq[String]): Vector[(String, String)] = {
    schema.columns.map(_.name).zip(fields).toVector
  }

  def makeTupleVectorOption(schema: Schema, fields: Seq[Option[String]]): Vector[(String, Option[String])] = {
    schema.columns.map(_.name).zip(fields).toVector
  }

  def makeTupleVectorFlatten(schema: Schema, fields: Seq[Option[String]]): Vector[(String, String)] = {
    schema.columns.map(_.name).zip(fields).toVector collect {
      case (k, Some(v)) => k -> v
    }
  }

  def makeTupleVectorNullable(schema: Schema, fields: Seq[Option[String]]): Vector[(String, AnyRef)] = {
    schema.columns.map(_.name).zip(fields).toVector map {
      case (k, Some(v)) => k -> v
      //      case (k, None) => k -> NullValue
      case (k, None) => k -> "" // NullValue see Projector#_normalize_form
    }
  }

  def makeMap(schema: Schema, fields: Seq[String]): Map[String, String] = {
    makeTupleVector(schema, fields).toMap
  }

  def makeRecord(schema: Schema, fields: Seq[String]): Record = {
    Record.create(makeTupleVector(schema, fields))
  }

  def makeRecordFlatten(schema: Schema, fields: Seq[Option[String]]): Record = {
    Record.create(makeTupleVectorFlatten(schema, fields))
  }

  def makeRecordNullable(schema: Schema, fields: Seq[Option[String]]): Record = {
    Record.create(makeTupleVectorNullable(schema, fields))
  }

  def toCsvValues(vs: Seq[Any], tz: DateTimeZone = DateTimeUtils.jodajst): Seq[String] = vs.map(toCsvValue(_, tz))

  def toCsvValue(v: Any, tz: DateTimeZone = DateTimeUtils.jodajst): String = {
    v match {
      case null => ""
      case None => ""
      case Some(x) => toCsvValue(x)
      case x: String => x
      case x: Boolean => x.toString
      case x: Byte => x.toString
      case x: Short => x.toString
      case x: Int => x.toString
      case x: Long => x.toString
      case x: Float => x.toString
      case x: Double => x.toString
      case x: BigInt => x.toString
      case x: BigDecimal => x.toString
      case x: java.math.BigInteger => x.toString
      case x: java.sql.Timestamp => DateTimeUtils.toIsoDateTimeString(x, tz)
      case x: java.sql.Date => DateUtils.toIsoDateString(x)
      case x: java.util.Date => DateUtils.toIsoDateString(x)
      case x: DateTime => DateTimeUtils.toIsoDateTimeString(x, tz)
//      case x: Record => toJsValue(x)
//      case x: RecordSet => toJsValue(x)
//      case xs: Seq[_] => JsArray(xs.map(anyToJsValue))
      case Nil => ""
      case x :: Nil => toCsvValue(x)
      case xs: List[_] => xs.map(toCsvValue(_)).mkString(",")
      case x => x.toString
    }
  }
}
