package org.goldenport.record.v3

import org.goldenport.parser._

/*
 * @since   Oct. 14, 2020
 * @version Oct. 14, 2020
 * @author  ASAMI, Tomoharu
 */
trait ParsePart { self: Record =>
  def parseStringOption(key: String): ParseResult[Option[String]] = 
    ParseResult(getString(key))

  def parseString(key: String): ParseResult[String] =
    for {
      v <- parseStringOption(key)
      r <- ParseResult.orMissing(s"Missing $key", v)
    } yield r

  def parseBigDecimalOption(key: String): ParseResult[Option[BigDecimal]] = 
    ParseResult(getBigDecimal(key))

  def parseBigDecimal(key: String): ParseResult[BigDecimal] =
    for {
      v <- parseBigDecimalOption(key)
      r <- ParseResult.orMissing(s"Missing $key", v)
    } yield r
}
