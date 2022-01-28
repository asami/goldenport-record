package org.goldenport.record.parser

import scalaz._, Scalaz._
import java.nio.charset.Charset
import java.net.URLDecoder
import org.goldenport.RAISE
import org.goldenport.Platform
import org.goldenport.Strings.{totokens, tokeyvalue}
import org.goldenport.parser.ParseResult
import org.goldenport.record.v2.{Schema}
import org.goldenport.record.v3.{Record, Field}
import org.goldenport.record.command.{UpdateMode, NeutralMode}

/*
 * @since   Mar. 20, 2021
 *  version Mar. 21, 2021
 * @version Jan. 23, 2022
 * @author  ASAMI, Tomoharu
 */
case class RecordParser(
  config: RecordParser.Config
) {
  import RecordParser._

  def schema = config.schema
  def charset = config.charset

  private val _field_parser_config = FieldParser.Config.default.copy(
    charset = config.charset,
    updateMode = config.updateMode,
    isEagerList = config.isEagerList
  )

  def httpQuery(p: Option[String]): ParseResult[Record] =
    p.map(httpQuery).getOrElse(ParseResult.success(Record.empty))

  def httpQuery(p: String): ParseResult[Record] =
    for {
      httped <- _http_query(p)
      structured <- _key_structure(httped)
      queried <- _query(structured)
      parsed <- _parse(queried)
    } yield parsed

  private def _http_query(p: String): ParseResult[Record] =
    if (p.startsWith("?"))
      _http_query_do(p.drop(1))
    else
      _http_query_do(p)

  private def _http_query_do(p: String): ParseResult[Record] = {
    val xs = totokens(p, "&")
    for {
      fs <- xs.traverse(_http_query_field)
    } yield Record(fs)
  }

  private def _http_query_field(p: String): ParseResult[Field] = {
    val (k, v) = tokeyvalue(p, "=")
    val key = decode(k, charset)
    val value = decode(v, charset)
    val c = schema.getColumn(key).map(_field_parser_config.withColumn).getOrElse(_field_parser_config)
    val parser = FieldParser(c)
    parser.create(key, value)
  }

  private def _query(p: Record): ParseResult[Record] =
    if (config.isQuery)
      _query_do(p)
    else
      ParseResult.success(p)

  private def _query_do(p: Record): ParseResult[Record] =
    for {
      xs <- p.fields.toVector.traverse { x =>
        val c = schema.getColumn(x.key).map(_field_parser_config.withColumn).getOrElse(_field_parser_config)
        val parser = FieldParser(c)
        parser.query(x)
      }
    } yield Record(xs)

  def httpForm(p: Option[Record]): ParseResult[Record] =
    p.map(httpForm).getOrElse(ParseResult.success(Record.empty))

  def httpForm(p: Record): ParseResult[Record] =
    for {
      structured <- _key_structure(p)
      commanded <- _command(structured)
      parsed <- _parse(commanded)
    } yield parsed

  private def _key_structure(p: Record): ParseResult[Record] =
    if (config.isKeyStructure)
      _key_structure_do(p)
    else
      ParseResult.success(p)

  private def _key_structure_do(p: Record): ParseResult[Record] = {
    p.http.request.build match {
      case Right(r) => ParseResult.success(r)
      case Left(l) => RAISE.notImplementedYetDefect
    }
  }

  private def _command(p: Record): ParseResult[Record] =
    if (config.isCommand)
      _command_do(p)
    else
      ParseResult.success(p)

  private def _command_do(p: Record): ParseResult[Record] =
    for {
      xs <- p.fields.toVector.traverse { x =>
        val c = schema.getColumn(x.key).map(_field_parser_config.withColumn).getOrElse(_field_parser_config)
        val parser = FieldParser(c)
        parser.command(x)
      }
    } yield Record(xs)

  private def _parse(p: Record): ParseResult[Record] =
    for {
      xs <- p.fields.toVector.traverse { x =>
        val c = schema.getColumn(x.key).map(_field_parser_config.withColumn).getOrElse(_field_parser_config)
        val parser = FieldParser(c)
        parser.parse(x)
      }
    } yield Record(xs)


  def decode(p: String): String = decode(p, Platform.charset.UTF8)

  def decode(p: String, charset: Charset): String = _decode_urlencoding(_decode_plus(p), charset)

  private def _decode_plus(p: String) =
    if (config.isDecodePlus)
      p.replace('+', ' ')
    else
      p

  private def _decode_urlencoding(p: String, charset: Charset) = URLDecoder.decode(p, charset.name)
}

object RecordParser {
  case class Config(
    schema: Schema = Schema.empty,
    charset: Charset = Platform.charset.UTF8,
    updateMode: UpdateMode = NeutralMode,
    isEagerList: Boolean = false,
    isDecodePlus: Boolean = false,
    isKeyStructure: Boolean = false,
    isQuery: Boolean = false,
    isCommand: Boolean = false
  )
  object Config {
    val default = Config()
  }
}
