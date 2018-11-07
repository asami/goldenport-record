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
 *  version Aug. 30, 2017
 *  version Jul. 18, 2018
 * @version Nov.  7, 2018
 * @author  ASAMI, Tomoharu
 */
object CsvUtils {
  private lazy val _parser = new CSVParser()

  def makeLine(xs: Seq[String], isforcedoublequote: Boolean): String = {
    xs.map(makeField(_, isforcedoublequote)).mkString(",")
  }

  def makeField(x: String, isforcedoublequote: Boolean): String = {
    def f(s: String) = s.replaceAllLiterally("\"", "\"\"")
    if (x.contains(',') || x.contains('"') || x.contains('\n') || x.contains('\r') || isforcedoublequote)
      "\"" + f(x) + "\""
    else
      x
  }

  def writeHeader(
    writer: Writer,
    schema: Schema,
    lineend: String,
    isforcedoublequote: Boolean
  ) {
    writer.write(makeHeader(schema, isforcedoublequote))
    writer.write(lineend)
  }

  private def _create_csv_writer(
    writer: Writer,
    lineend: String,
    isforcedoublequote: Boolean // TODO disable quote feature in CSVWriter
  ) = {
    new CSVWriter(
      writer,
      CSVWriter.DEFAULT_SEPARATOR,
      CSVWriter.DEFAULT_QUOTE_CHARACTER,
      lineend
    )
  }

  def write(
    writer: Writer,
    lineend: String,
    lines: Seq[Seq[String]],
    isforcedoublequote: Boolean
  ) {
    import scala.collection.JavaConverters._
    for (out <- resource.managed(_create_csv_writer(writer, lineend, isforcedoublequote))) {
      val a = lines.map(_.toArray).asJava
      out.writeAll(a)
    }
  }

  def write(
    writer: Writer,
    lineend: String,
    rs: ResultSet,
    isforcedoublequote: Boolean
  ): Int = {
    var count = 0
    for (out <- resource.managed(_create_csv_writer(writer, lineend, isforcedoublequote))) {
      out.writeAll(rs, false)
      count += 1
    }
    count
  }

  def write(
    writer: Writer,
    lineend: String,
    rs: Iterator[Record],
    isforcedoublequote: Boolean
  ): Int = {
    var count = 0
    for (out <- resource.managed(_create_csv_writer(writer, lineend, isforcedoublequote))) {
      for (rec <- rs) {
        out.writeNext(record2Values(rec, isforcedoublequote))
        count += 1
      }
    }
    count
  }

  def write(
    writer: Writer,
    schema: Schema,
    lineend: String,
    rs: Iterator[Record],
    isforcedoublequote: Boolean
  ): Int = {
    var count = 0
    for (out <- resource.managed(_create_csv_writer(writer, lineend, isforcedoublequote))) {
      for (rec <- rs) {
        out.writeNext(record2Values(rec, schema, isforcedoublequote))
        count += 1
      }
    }
    count
  }

  def append(
    writer: Writer,
    schema: Schema,
    lineend: String,
    map: Map[String, String],
    isforcedoublequote: Boolean
  ) {
    val line = map2Values(map, schema, is, isforcedoublequote).mkString(",")
    writer.write(line)
    writer.write(lineend)
  }

  def append(
    writer: Writer,
    lineend: String,
    rec: Record,
    isforcedoublequote: Boolean
  ) {
    val line = record2Values(rec, isforcedoublequote).mkString(",")
    writer.write(line)
    writer.write(lineend)
  }

  def append(
    writer: Writer,
    schema: Schema,
    lineend: String,
    rec: Record,
    isforcedoublequote: Boolean
  ) {
    val line = record2Values(rec, schema, isforcedoublequote).mkString(",")
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
      case x :: Nil => toCsvValue(x, false)
      case xs => toCsvValue(xs, false)
    }
  }

  def getValue(
    rec: Record,
    key: String,
    isforcedoublequote: Boolean
  ): String = {
    getValue(rec, Symbol(key), isforcedoublequote)
  }

  def getValue(
    rec: Record,
    key: Symbol,
    isforcedoublequote: Boolean
  ): String = {
//    rec.getOne(key).map(toCsvValue(_)) getOrElse ""
    rec.getList(key) match {
      case Nil => ""
      case x :: Nil => toCsvValue(x, isforcedoublequote)
      case xs => toCsvValue(xs, isforcedoublequote)
    }
  }

  def record2Values(
    rec: Record
  ): Array[String] = record2Values(rec, false)

  def record2Values(
    rec: Record,
    isforcedoublequote: Boolean
  ): Array[String] = {
    val keys = rec.fields.map(_.key)
    keys.map(x => getValue(rec, x, isforcedoublequote)).toArray
  }

  def record2Values(
    rec: Record,
    schema: Schema
  ): Array[String] = record2Values(rec, schema, false)

  def record2Values(
    rec: Record,
    schema: Schema,
    isforcedoublequote: Boolean
  ): Array[String] = {
    val keys = schema.columns.map(_.name)
    keys.map(x => getValue(rec, x, isforcedoublequote)).toArray
  }

  def map2Values(
    map: Map[String, String],
    schema: Schema
  ): Array[String] = map2Values(map, schema, false)

  def map2Values(
    map: Map[String, String],
    schema: Schema,
    isforcedoublequote: Boolean
  ): Array[String] = {
    def getvalue(key: String) = makeField(map.get(key).orZero, isforcedoublequote)
    val keys = schema.columns.map(_.name)
    keys.map(getvalue).toArray
  }

  def parseLine(line: String): Vector[String] = {
    _parser.parseLine(line).toVector
  }

  // def parseOptionFields(chunk: String): Vector[Option[String]] = {
  //   CsvLineMaker.parse(line)
  // }

  def makeHeader(schema: Schema, isforcedoublequote: Boolean): String = {
    makeLine(schema.header, isforcedoublequote)
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

  def toCsvValues(
    vs: Seq[Any],
    isforcedoublequote: Boolean,
    tz: DateTimeZone = DateTimeUtils.jodajst
  ): Seq[String] = vs.map(toCsvValue(_, isforcedoublequote, tz))

  def toCsvValue(
    v: Any,
    isforcedoublequote: Boolean,
    tz: DateTimeZone = DateTimeUtils.jodajst
  ): String =
    makeField(_to_csv_value(v, tz), isforcedoublequote)

  private def _to_csv_value(
    v: Any,
    tz: DateTimeZone = DateTimeUtils.jodajst
  ): String = {
    v match {
      case null => ""
      case None => ""
      case Some(x) => _to_csv_value(x)
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
      case x :: Nil => _to_csv_value(x)
      case xs: List[_] => xs.map(_to_csv_value(_)).mkString(",")
      case x => x.toString
    }
  }
}
