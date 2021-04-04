package org.goldenport.record.parser

import scalaz._, Scalaz._
import java.nio.charset.Charset
import org.goldenport.Platform
import org.goldenport.Strings.totokens
import org.goldenport.parser.ParseResult
import org.goldenport.record.v2.{Column}
import org.goldenport.record.v3.{FieldValue, SingleValue, MultipleValue, EmptyValue}

/*
 * @since   Mar. 20, 2021
 * @version Mar. 21, 2021
 * @author  ASAMI, Tomoharu
 */
trait FieldValueParser {
  def config: FieldValueParser.Config

  def create(p: String): ParseResult[FieldValue] = ParseResult.success(SingleValue(p))

  def parse(p: FieldValue): ParseResult[FieldValue] = 
    if (config.isEagerList)
      parse_eager_list(p)
    else
      parse_Value(p)

  protected def parse_Value(p: FieldValue): ParseResult[FieldValue]

  protected final def parse_eager_list(p: FieldValue) = ParseResult {
    p match {
      case SingleValue(v) => _eager_list(v) match {
        case Right(r) => MultipleValue(r)
        case Left(l) => p
      }
      case MultipleValue(vs) =>
        val xs = vs.flatMap(x => _eager_list(x) match {
          case Right(r) => r
          case Left(l) => List(l)
        })
        MultipleValue(xs)
      case EmptyValue => p
    }
  }

  private def _eager_list(p: Any): Either[Any, Seq[String]] = p match {
    case m: String if m.contains(',') =>
      totokens(m, ",") match {
        case Nil => Left(p)
        case x :: Nil => Left(p)
        case xs => Right(xs)
      }
    case _ => Left(p)
  }

  protected final def parse_value(column: Column, v: Any): ParseResult[Any] =
    column.parse(v)

  protected final def parse_values(column: Column, vs: Seq[Any]): ParseResult[Seq[Any]] =
    vs.toVector.traverse(parse_value(column, _))
}

case class ColumnFieldValueParser(column: Column, config: FieldValueParser.Config) extends FieldValueParser {
  protected def parse_Value(p: FieldValue): ParseResult[FieldValue] = p match {
    case SingleValue(v) => for {
      x <- parse_value(column, v)
    } yield SingleValue(x)
    case MultipleValue(vs) => for {
      xs <- parse_values(column, vs)
    } yield MultipleValue(xs)
    case EmptyValue => ParseResult.success(p)
  }

  // def parse(p: String): ParseResult[FieldValue] = {
  //   val r = if (column.isSingle)
  //     SingleValue(p)
  //   else
  //     MultipleValue(p)
  //   ParseResult.success(r)
  // }
}

case class StringFieldValueParser(config: FieldValueParser.Config) extends FieldValueParser {
  protected def parse_Value(p: FieldValue): ParseResult[FieldValue] =
    ParseResult.success(p)

  // def parse(p: String): ParseResult[FieldValue] = {
  //   ParseResult.success(SingleValue(p))
  // }
}

object FieldValueParser {
  case class Config(
    column: Option[Column] = None,
    charset: Charset = Platform.charset.UTF8,
    isEagerList: Boolean = false
  ) {
    def withColumn(p: Column) = copy(column = Some(p))
  }
  object Config {
    val default = Config()
  }

  def apply(): FieldValueParser = apply(Config.default)

  def apply(config: Config): FieldValueParser =
    config.column.map(ColumnFieldValueParser(_, config)).getOrElse(StringFieldValueParser(config))
}
