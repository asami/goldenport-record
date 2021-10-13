package org.goldenport.record.v3

import org.goldenport.parser._

/*
 * @since   Oct. 14, 2020
 * @version Oct.  6, 2021
 * @author  ASAMI, Tomoharu
 */
trait ParsePart { self: Record =>
  def parseBoolean(key: String): ParseResult[Boolean] =
    getBoolean(key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseBooleanOption(key: String): ParseResult[Option[Boolean]] = ParseResult(
    getBoolean(key)
  )

  def parseShort(key: String): ParseResult[Short] =
    getShort(key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseShortOption(key: String): ParseResult[Option[Short]] = ParseResult(
    getShort(key)
  )

  def parseInt(key: String): ParseResult[Int] =
    getInt(key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseIntOption(key: String): ParseResult[Option[Int]] = ParseResult(
    getInt(key)
  )

  def parseLong(key: String): ParseResult[Long] =
    getLong(key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseLongOption(key: String): ParseResult[Option[Long]] = ParseResult(
    getLong(key)
  )

  def parseFloat(key: String): ParseResult[Float] =
    getFloat(key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseFloatOption(key: String): ParseResult[Option[Float]] = ParseResult(
    getFloat(key)
  )

  def parseDouble(key: String): ParseResult[Double] =
    getDouble(key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseDoubleOption(key: String): ParseResult[Option[Double]] = ParseResult(
    getDouble(key)
  )

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
