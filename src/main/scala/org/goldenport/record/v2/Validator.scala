package org.goldenport.record.v2

import scalaz._, Scalaz._
import play.api.libs.json._
import org.goldenport.RAISE

/*
 * @since   Apr. 28, 2019
 * @version May.  1, 2019
 * @author  ASAMI, Tomoharu
 */
trait Validator {
  def validate(record: Record): ValidationResult = validateRecord(record)
  def validateField(field: Field): ValidationResult // legacy
  def validateRecord(record: Record): ValidationResult // legacy
  def validateRecords(records: Seq[Record]): Seq[Record] // legacy
  def toJson: JsValue = RAISE.notImplementedYetDefect
}

trait ValidatorClass {
  def fromJsonOption(p: JsValue): Option[Validator] = RAISE.notImplementedYetDefect
}

trait Validations {
//  implicit def ValidationResultZero: Zero[ValidationResult] = zero(Valid)
//  implicit def ValidationResultSemigroup: Semigroup[ValidationResult] = semigroup((a, b) => a + b)
  implicit object ValidationResultMonoid extends Monoid[ValidationResult] {
    def append(f1: ValidationResult, f2: => ValidationResult) = f1 + f2
    def zero: ValidationResult = Valid
  }
}

case object Validator extends Validations {
  object json {
    import play.api.libs.functional.syntax._
    import org.goldenport.json.JsonUtils.Implicits._

    val validators: Vector[ValidatorClass] = Vector(
      FieldsMatchValidator,
      FieldMatchValidator,
      FieldContainsValidator
    )

    object Implicits {
      implicit val ValidatorFormat = new Format[Validator] {
        def reads(json: JsValue): JsResult[Validator] =
          validators.toStream.flatMap(_.fromJsonOption(json)).headOption.
            map(x => JsSuccess(x)).
            getOrElse(JsError(s"No validator: $json"))
        def writes(o: Validator): JsValue = o.toJson
      }
    }
  }
}

trait FieldValidator extends Validator {
  def validateRecord(record: Record): ValidationResult = Valid
  def validateRecords(records: Seq[Record]): Seq[Record] = records
}

trait RecordValidator extends Validator {
  def validateField(field: Field): ValidationResult = Valid
  def validateRecords(records: Seq[Record]): Seq[Record] = records
}

trait RecordsValidator extends Validator {
  def validateField(field: Field): ValidationResult = Valid
  def validateRecord(record: Record): ValidationResult = Valid
}

class FieldsMatchValidator(keys: Seq[String],
                           predicate: Seq[Any] => Boolean,
                           failure: Boolean = true,
                           message: Option[String] = None) extends FieldValidator {
  def validateField(field: Field) = {
    val k = field.key
    val v = field.values
    if (keys.contains(k)) {
      if (predicate(v)) Valid
      else {
        if (failure) IllegalFieldFailure(k.name, v, none, message)
        else IllegalFieldWarning(k.name, v, none, message)
      }
    } else Valid
  }
}
object FieldsMatchValidator extends ValidatorClass {
}

case object FieldMatchValidator extends ValidatorClass {
  def apply(key: String, value: String) = {
    new FieldsMatchValidator(List(key), (x: Seq[Any]) => {
      x match {
        case Nil => false
        case x :: Nil if x == value => true
        case _ => false
      }
    })
  }

  def apply(key: String, values: Seq[String]) = {
    new FieldsMatchValidator(List(key), (x: Seq[Any]) => {
      x match {
        case Nil => false
        case x :: Nil if values.contains(x) => true
        case _ => false
      }
    })
  }
}

case object FieldContainsValidator extends ValidatorClass {
  def apply(key: String, values: Seq[String], failure: Boolean = true) = {
    new FieldsMatchValidator(List(key), (x: Seq[Any]) => {
      values.contains(x.contains _)
//      x.flatMap(a => values.find(a.contains)) ? false | true
    }, failure, (values.mkString("(", ",", ")") + "を含んでいます。").some)
  }
}  

/*
case class DuplicateIdValidator(key: String, label: String = "") extends RecordsValidator {
  val effectivelabel = if (label != "") label else key

  def validateRecords(records: Seq[Record]): Seq[Record] = {
    val (_, dups) = records.filter(!_.isReferenceData).foldRight((Set[String](), List[String]())) {
      (x, a) => val (keys, dups) = a
        x.getOne(key) match {
          case Some(v) => {
            if (keys.contains(v)) (keys, v :: dups)
            else (keys + v, dups)
          }
          case None => a
        }
    }
    for (a <- records) yield {
      a.getOne(key) match {
        case Some(v) if dups.contains(v) && !a.isReferenceData => a.enwarning(
          DuplicateWarning("%sが重複しています。".format(effectivelabel), v))
        case _ => a
      }
    }
  }
}

case class DuplicateCompositeIdValidator(ids: Seq[String], labels: Seq[String] = Nil) extends RecordsValidator {
  val effectivelabels = ids.zipAll(labels, "", "").foldRight(nil[String])((x, a) => {
    (if (x._2 != "") x._2 else x._1) :: a
  })

  def validateRecords(records: Seq[Record]): Seq[Record] = {
    val (_, dups) = records.filter(!_.isReferenceData).foldRight((Set[Seq[Option[String]]](), Seq[Seq[Option[String]]]())) {
      (x, a) => {
        val (keys, dups) = a
        val vs = ids.map(x.getOne)
        if (keys.contains(vs)) (keys, vs +: dups)
        else (keys + vs, dups)
      }
    }
    for (b <- records) yield {
      val vs = ids.map(b.get)
      if (dups.contains(vs) && !b.isReferenceData) {
        b.enwarning(
          DuplicateWarning("%sの組が重複しています。".format(effectivelabels.mkString("(", ",", ")")), vs.toString))
      } else b
    }
  }
}
*/
