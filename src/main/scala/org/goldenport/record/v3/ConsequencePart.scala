package org.goldenport.record.v3

import org.goldenport.context.Consequence

/*
 * @since   Oct.  6, 2021
 * @version Oct.  6, 2021
 * @author  ASAMI, Tomoharu
 */
trait ConsequencePart { self: Record =>
  def consequenceBoolean(key: String): Consequence[Boolean] =
    getBoolean(key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceBooleanOption(key: String): Consequence[Option[Boolean]] = Consequence(
    getBoolean(key)
  )

  def consequenceShort(key: String): Consequence[Short] =
    getShort(key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceShortOption(key: String): Consequence[Option[Short]] = Consequence(
    getShort(key)
  )

  def consequenceInt(key: String): Consequence[Int] =
    getInt(key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceIntOption(key: String): Consequence[Option[Int]] = Consequence(
    getInt(key)
  )

  def consequenceLong(key: String): Consequence[Long] =
    getLong(key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceLongOption(key: String): Consequence[Option[Long]] = Consequence(
    getLong(key)
  )

  def consequenceFloat(key: String): Consequence[Float] =
    getFloat(key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceFloatOption(key: String): Consequence[Option[Float]] = Consequence(
    getFloat(key)
  )

  def consequenceDouble(key: String): Consequence[Double] =
    getDouble(key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceDoubleOption(key: String): Consequence[Option[Double]] = Consequence(
    getDouble(key)
  )

  def consequenceStringOption(key: String): Consequence[Option[String]] = 
    Consequence(getString(key))

  def consequenceString(key: String): Consequence[String] =
    for {
      v <- consequenceStringOption(key)
      r <- Consequence.successOrMissingPropertyFault(s"Missing $key", v)
    } yield r

  def consequenceBigDecimalOption(key: String): Consequence[Option[BigDecimal]] = 
    Consequence(getBigDecimal(key))

  def consequenceBigDecimal(key: String): Consequence[BigDecimal] =
    for {
      v <- consequenceBigDecimalOption(key)
      r <- Consequence.successOrMissingPropertyFault(s"Missing $key", v)
    } yield r
}
