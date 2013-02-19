package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._

/*
 * @snice   Nov. 23, 2012
 *  version Dec.  9, 2012
 * @version Feb. 20, 2013
 * @author  ASAMI, Tomoharu
 */
trait Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult]

  protected final def validate_double(v: Option[_], msg: String): Validation[ValidationResult, Double] = {
    v match {
      case None => ValueDomainFailure(msg, "???").fail // XXX
      case Some(Nil) => ValueDomainFailure(msg, "???").fail // XXX
      case Some(x: String) => validate_double(x, msg)
      case Some((x: String) :: Nil) => validate_double(x, msg)
      case Some(xs) => ValueDomainFailure(msg, "???").fail // XXX
    }
  }

  protected final def validate_double(v: Seq[String], msg: String): Validation[ValidationResult, Double] = {
    v match {
      case Nil => ValueDomainFailure(msg, "???").fail // XXX
      case x :: Nil => validate_double(x, msg)
      case xs => ValueDomainFailure(msg, "???").fail // XXX
    }
  }

  protected final def validate_double(v: String, msg: String): Validation[ValidationResult, Double] = {
    v.parseDouble match {
      case Success(s) => Success(s)
      case Failure(e) => Failure(ValueDomainFailure("???", v)) // XXX
    }
  }

  protected final def validate_double_sum(xs: Seq[String]): Validation[ValidationResult, Double] = {
    xs.foldRight(0.0.success[ValidationResult])((x, a) => {
      a.flatMap(y => validate_double(x, "値がありません").map(_ + y))
    })
  }

  protected final def validation_equal_2(lhs: Double, rhs: Double, msg: String): ValidationResult = {
    if (equal_decimal_point_2(lhs, rhs)) {
      Valid
    } else {
      ValueDomainFailure(msg, rhs.toString) // XXX
    }
  }

  protected final def round_decimal_point_2(v: Double) = {
    (v * 100).toInt.toDouble / 100
  }

  protected final def equal_decimal_point_2(lhs: Double, rhs: Double) = {
    @inline def calc(d: Double): Int = ((d * 1000).toInt + 5) / 10
    calc(lhs) == calc(rhs)
  }
}

object Constraint {
  val MESSAGE_NO_DATA = "データがありません"
  val MESSAGE_EMPTY_DATA = "データが空です"
  val MESSAGE_NO_FIELD = "フィールドがありません"

  def validateMultiplicity(
    datatype: DataType, multiplicity: Multiplicity,
    cs: Seq[Constraint],
    value: Seq[String],
    record: Record): ValidationResult = {
    def tolist(s: String) = {
      s.split(",").toList // XXX
    }

    val a: Validation[Invalid, Seq[String]] = multiplicity match {
      case MOne => {
        value match {
          case null => MultiplicityFailure(MOne, MESSAGE_NO_DATA).fail
          case Nil => MultiplicityFailure(MOne, MESSAGE_EMPTY_DATA).fail
          case v => v.success
        }
      }
      case MZeroOne => {
        value match {
          case null => Nil.success
          case Nil => Nil.success
          case v => v.success
        }
      }
      case MOneMore => {
        value match {
          case null => MultiplicityFailure(MOne, MESSAGE_NO_DATA).fail
          case Nil => MultiplicityFailure(MOne, MESSAGE_EMPTY_DATA).fail
          case v => v.success
        }
      }
      case MZeroMore => {
        value match {
          case null => Nil.success
          case Nil => Nil.success
          case v => v.success
        }
      }
    }
    a match {
      case Success(s) => s.foldMap(x => cs.foldMap(_.validate(datatype, x, record) | Valid))
      case Failure(f) => f
    }
  }
}

case class DataTypeConstraint(datatype: DataType) extends Constraint {
  def validate(dt: DataType, value: String, record: Record): Option[ValidationResult] = {
    datatype.validate(value).some
  }
}

case class CWRange(minimum: Double, maximum: Double) extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    datatype.toDouble(value) map {
      x => {
        if (x < minimum) ValueDomainWarning("%sは${minimum}未満になっています。".replace("${minimum}", minimum.toString), value)
        else if (x > maximum) ValueDomainWarning("%sは${maximum}より大きくなっています。".replace("${maximum}",  maximum.toString), value)
        else Valid
      }
    } toOption
  }
}

case object CPositiveZero extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    datatype.toDouble(value) map {
      x => 
        if (x >= 0) Valid
        else ValueDomainFailure("%sは0未満になっています。", value)
    } toOption
  }
}

case object CWNotZero extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    datatype.toDouble(value) map {
      x =>
        if (x == 0) ValueDomainWarning("0になっています。", value)
        else Valid
    } toOption
  }
}

/**
 * Wraning constraint for range 0 to 100
 */
object CW0to100 extends CWRange(0, 100)

/**
 * Warning constraint for range -100 to 100
 */
object CWAbs100 extends CWRange(-100, 100)

/**
 * Costraint for Sum
 */
case class CSum(xs: String*) extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    val a = for {
      s <- validate_double_sum(xs)
      v <- validate_double(value, "値がありません。")
    } yield {
      validation_equal_2(s, v, "合算の計算が合っていません。")
    }
    a toOption
  }
}

/**
 * Costraint for Subtraction
 */
case class CSubtraction(lhs: String, rhs: String*) extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    val a = for {
      l <- validate_double(lhs, "値がありません。")
      r <- validate_double_sum(rhs)
      v <- validate_double(value, "値がありません。")
    } yield {
      validation_equal_2(l - r, v, "計算が合っていません。")
    }
    a toOption
  }
}

case class CSubtractions(lhs: List[String], rhs: List[String]) extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    val a = for {
      l <- validate_double_sum(lhs)
      r <- validate_double_sum(rhs)
      v <- validate_double(value, "値がありません。")
    } yield {
      validation_equal_2(l - r, v, "計算が合っていません。")
    }
    a toOption
  }
}

/**
 * Constraint for Percent 
 */
case class CPercent(amount: String, total: String) extends Constraint {
  def validate(datatype: DataType, value: String, record: Record): Option[ValidationResult] = {
    val r = for {
      a <- validate_double(record.get(amount), "パーセントの計算ができません。")
      t <- validate_double(record.get(total), "パーセントの計算ができません。")
      v <- validate_double(value, "値がありません。")
    } yield {
      validation_equal_2(a * 100 / t, v, "パーセントの計算があっていません。")
    }
    r toOption
  }
}
