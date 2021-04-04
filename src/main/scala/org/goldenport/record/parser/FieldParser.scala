package org.goldenport.record.parser

import java.net.URLDecoder
import java.nio.charset.Charset
import org.goldenport.Platform
import org.goldenport.Strings.tokeyvalue
import org.goldenport.parser.ParseResult
import org.goldenport.record.v2.{Column}
import org.goldenport.record.v3.{Field}
import org.goldenport.record.command.{UpdateMode, NeutralMode}

/*
 * @since   Mar. 20, 2021
 * @version Mar. 21, 2021
 * @author  ASAMI, Tomoharu
 */
case class FieldParser(config: FieldParser.Config) {
  def charset = config.charset

  private val _field_value_config = FieldValueParser.Config.default.copy(
    column = config.column,
    charset = config.charset,
    isEagerList = config.isEagerList
  )

  private val _field_value_parser = FieldValueParser(_field_value_config)

  def create(key: String, v: String): ParseResult[Field] = for {
    value <- _field_value_parser.create(v)
  } yield Field(key, value)

  def query(p: Field): ParseResult[Field] = ParseResult.success(p)

  def command(p: Field): ParseResult[Field] =
    PropertyOperation.getOperation(p).map(_command(p)).getOrElse(ParseResult.success(p))

  private def _command(p: Field)(op: PropertyOperation): ParseResult[Field] = {
    val k = p.name.dropRight(op.postfix.length)
    val v = p.value.mapContent(x => op)
    ParseResult.success(Field(k, v))
  }

  def parse(p: Field): ParseResult[Field] = for {
    x <- _field_value_parser.parse(p.value)
  } yield p.withValue(x)
}

object FieldParser {
  case class Config(
    column: Option[Column] = None,
    charset: Charset = Platform.charset.UTF8,
    updateMode: UpdateMode = NeutralMode,
    isEagerList: Boolean = false
  ) {
    def withColumn(p: Column) = copy(column = Some(p))
  }
  object Config {
    val default = Config()
  }
}
