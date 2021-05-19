package org.goldenport.record.v3

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.context.{ValueDomainFault, ValueDomainMultiplicityFault}
import org.goldenport.record.v2.{DataType, XString}
import org.goldenport.record.v2.{Multiplicity, MOne, MZeroOne}
import org.goldenport.record.v2.{Constraint, CompoundFailure}
import org.goldenport.record.v2.{ValidationResult, Valid, Invalid, Warning}
import org.goldenport.record.v2.MultiplicityFailure
import org.goldenport.record.v2.Validator._

/*
 * @since   Apr. 29, 2021
 * @version May. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class ValueDomain(
  datatype: DataType = XString,
  multiplicity: Multiplicity = MOne,
  constraints: List[Constraint] = Nil
) {
  def isSingle = multiplicity match {
    case MOne => true
    case MZeroOne => true
    case _ => false
  }

  def resolve(p: Any): ValidationNel[ValueDomainFault, Any] =
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

  private def _resolve_single(p: Any): ValidationNel[ValueDomainFault, Any] =
    datatype.validate(p) |+| _validate_constraints(p) match {
      case Valid => Success(datatype.toInstance(p))
      case m: Warning => Success(datatype.toInstance(p)) // TODO
      case m: Invalid => m match {
        case CompoundFailure(fs, w) =>
          val a = fs.map(_value_domain_fault)
          Failure(NonEmptyList.nel(a.head, IList.fromList(a.tail.toList)))
        case mm => Failure(NonEmptyList(ValueDomainFault(mm.i18nMessage)))
      }
    }

  private def _validate_constraints(p: Any): ValidationResult =
    constraints.foldMap(_.validate(datatype, p))

  private def _value_domain_fault(p: Invalid): ValueDomainFault = p match {
    case m: MultiplicityFailure => ValueDomainMultiplicityFault(m.i18nMessage) // TODO
    case m => ValueDomainFault(m.i18nMessage)
  }
}
