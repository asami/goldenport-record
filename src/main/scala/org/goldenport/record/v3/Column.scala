package org.goldenport.record.v3

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.values.Designation
import org.goldenport.context.{ArgumentFault, InvalidArgumentFault}
import org.goldenport.record.v2.{DataType, XString}
import org.goldenport.record.v2.{Multiplicity, MOne, MZeroOne}
import org.goldenport.record.v2.{Constraint, CompoundFailure}
import org.goldenport.record.v2.{ValidationResult, Valid, Invalid, Warning}
import org.goldenport.record.v2.{MultiplicityFailure}
import org.goldenport.record.v2.Validator._

/*
 * @since   Dec.  8, 2012
 *  version Dec. 12, 2012
 *  version Feb. 20, 2013
 *  version Mar.  3, 2013
 *  version Oct. 23, 2013
 *  version Jan. 20, 2014
 *  version Jul. 25, 2014
 *  version Aug. 11, 2014
 *  version Oct. 27, 2015
 *  version Nov.  8, 2015
 *  version Feb. 26, 2016
 *  version Jan. 15, 2017
 *  version Aug.  1, 2017
 *  version Sep. 27, 2017
 *  version Oct. 22, 2017
 *  version Nov. 12, 2017
 *  version Dec. 13, 2017
 *  version Apr. 10, 2018
 *  version Jul. 28, 2018
 *  version Aug. 24, 2018
 *  version Sep.  4, 2018
 *  version Jan.  9, 2019
 *  version Jul.  7, 2019
 *  version Aug. 23, 2019
 *  version Oct.  9, 2019
 *  version Feb. 25, 2020
 *  version Mar. 30, 2020
 *  version May. 11, 2020
 *  version Jun.  1, 2020
 *  version Mar. 21, 2021
 * @version Mar. 25, 2021 restart
 * @author  ASAMI, Tomoharu
 */
case class Column(
  designation: Designation,
  datatype: DataType = XString,
  multiplicity: Multiplicity = MOne,
  constraints: List[Constraint] = Nil
) extends Designation.Holder {
  def isSingle = multiplicity match {
    case MOne => true
    case MZeroOne => true
    case _ => false
  }

  def resolve(p: Any): ValidationNel[ArgumentFault, Any] =
    _multiple_or_single(p) match {
      case Right(s) => 
        if (isSingle)
          _resolve_single(p)
        else
          _resolve_multiple(p)
      case Left(m) => 
        if (isSingle)
          _resolve_single(p)
        else
          _resolve_multiple(p)
    }

  private def _multiple_or_single(p: Any): Either[Seq[_], Any] = p match {
    case m: Seq[_] => Left(m)
    case m: Array[_] => Left(m.toVector)
    case m: Iterable[_] => Left(m.toVector)
    case m: Iterator[_] => Left(m.toVector)
    case m => Right(m)
  }

  private def _resolve_multiple(p: Any) = {
    RAISE.notImplementedYetDefect
  }

  private def _resolve_single(p: Any): ValidationNel[ArgumentFault, Any] =
    datatype.validate(p) |+| _validate_constraints(p) match {
      case Valid => Success(datatype.toInstance(p))
      case m: Warning => Success(datatype.toInstance(p)) // TODO
      case m: Invalid => m match {
        case CompoundFailure(fs, w) =>
          val a = fs.map(_argument_fault)
          Failure(NonEmptyList.nel(a.head, a.tail.toList))
        case mm => Failure(NonEmptyList(InvalidArgumentFault(mm.i18nMessage)))
      }
    }

  private def _validate_constraints(p: Any): ValidationResult =
    constraints.foldMap(_.validate(datatype, p))

  private def _argument_fault(p: Invalid): ArgumentFault = p match {
    case m: MultiplicityFailure => InvalidArgumentFault(m.i18nMessage) // TODO
    case m => InvalidArgumentFault(m.i18nMessage)
  }
}

object Column {
  def apply(name: String, datatype: DataType, multiplicity: Multiplicity): Column = {
    Column(Designation(name), datatype, multiplicity)
  }
}
