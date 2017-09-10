package org.goldenport.record.v2.projector

import scalaz._, Scalaz._
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.record.command.NullValue
import org.goldenport.record.v2.util.RecordAux

/*
 * TODO FieldCommand integration.
 *
 * @since   Sep. 23, 2015
 *  version Oct.  9, 2015
 *  version Nov. 11, 2015
 *  version Dec. 10, 2015
 *  version Mar. 11, 2016
 *  version Aug. 31, 2016
 *  version Sep. 23, 2016
 *  version Jan. 21, 2017
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
case class Projector(
  schema: Schema,
  policy: Projector.Policy = Projector.Policy.rigid
) {
  val multiplicityRegex = """^(.*)_(\d+)$""".r

  def apply(rec: Record): \/[ValidationResult, Record] = {
    val a = _normalize(rec)
    schema.validate(a) match {
      case Valid => _project(a).right
      case x: Warning => _project(a).right
      case x: Invalid => x.left
    }
  }

  private def _normalize(rec: Record): Record = {
    val a: Map[String, List[Any]] = rec.fields.foldMap { x =>
      val name = x.key.name
      multiplicityRegex.findFirstMatchIn(name) match {
        case Some(s) =>
          val k = s.group(1)
          val i = s.group(2)
          if (_can_multiple(k))
            Map(k -> x.values)
          else
            Map(name -> x.values)
        case None => Map(name -> x.values)
      }
    }
    val b = {
      val labelnamemap = schema.columns.flatMap(c =>
        c.label.map(_ -> c.name).toList ++ c.aliases.map(_ -> c.name)
      ).toMap
      a.toList.foldMap {
        case (k, v) =>
          labelnamemap.get(k).cata(s => Map(s -> v), Map(k -> v))
      }
    }
    val c: List[Field] = b.map {
      case (k, v) => Field(Symbol(k), v)
    }.toList
    val d = _normalize_form(c)
    val e = _normalize_datatype(d)
    rec.copy(fields = e)
  }

  private def _can_multiple(key: String) = {
    schema.getColumn(Symbol(key)) match {
      case Some(s) => s.isMulti
      case None => false
    }
  }

  private def _normalize_form(p: List[Field]) = {
    if (policy.severe.form)
      p.filterNot(RecordAux.isEmptyForm(schema))
    else
      p.map { f =>
        if (RecordAux.isEmptyForm(schema)(f))
          f.copy(values = Nil) // NullValue see CsvUtils#makeTupleVectorNullable
        else
          f
      }
  }

  private def _normalize_datatype(p: List[Field]) = p map { f =>
    schema.getColumn(f.key.name).fold(f)(_ match {
      case m => f
    })
  }

  // unused because _normalize updates key
  private def _get_field(column: Column, rec: Record): Option[Field] = {
    val names: Stream[String] = column.name #:: column.aliases.toStream #::: column.label.toStream
    names.flatMap(rec.getField(_)).headOption
  }

  private def _project(rec: Record): Record = {
    val a: List[Field] = schema.columns.toList flatMap { c =>
      val field = rec.getField(c.name)
      c.multiplicity match {
        case MOne => _get_one(c.datatype, field)
        case MZeroOne => _get_zero_one(c.datatype, field)
        case MOneMore => _get_one_more(c.datatype, field)
        case MZeroMore => _get_zero_more(c.datatype, field)
        case m: MRange => _get_range(c.datatype, m, field)
        case m: MRanges => _get_ranges(c.datatype, m, field)
      }
    }
    if (policy.severe.redundancyField)
      rec.copy(fields = a)
    else
      rec.copy(fields = a ++ rec.fields).normalize
  }

  private def _get_one(datatype: DataType, field: Option[Field]): Option[Field] = {
    _normalize_datatype(datatype, field)
  }

  private def _get_zero_one(datatype: DataType, field: Option[Field]): Option[Field] = {
    _normalize_datatype(datatype, field)
  }

  private def _get_one_more(datatype: DataType, field: Option[Field]): Option[Field] = {
    _normalize_datatype(datatype, field)
  }

  private def _get_zero_more(datatype: DataType, field: Option[Field]): Option[Field] = {
    _normalize_datatype(datatype, field)
  }

  private def _get_range(datatype: DataType, range: MRange, field: Option[Field]): Option[Field] = {
    _normalize_datatype(datatype, field)
  }

  private def _get_ranges(datatype: DataType, range: MRanges, field: Option[Field]): Option[Field] = {
    _normalize_datatype(datatype, field)
  }

  private def _normalize_datatype(datatype: DataType, field: Option[Field]): Option[Field] =
    field.map(_normalize_datatype(datatype, _))

  private def _normalize_datatype(datatype: DataType, field: Field): Field = {
    datatype match {
      case XPowertype(p) => _normalize_powertype(p, field)
      case _ => field
    }
  }

  private def _normalize_powertype(p: PowertypeClass, field: Field): Field =
    field.copy(values = _normalize_powertype(p, field.values))

  private def _normalize_powertype(p: PowertypeClass, values: List[Any]): List[Any] = {
    def toinstance(x: Any) = x match {
      case s: String => Strings.totokens(s).map(p.toInstance)
      case _ => List(x)
    }
    values flatMap {
      case xs: Seq[_] => _normalize_powertype(p, xs.toList)
      case Some(x) => toinstance(x)
      case None => Nil
      case x => toinstance(x)
    }
  }
}

object Projector {
  case class Policy(severe: Severe)

  object Policy {
    val rigid = Policy(Severe.rigid)
    val update = Policy(Severe.update)
    val updateForm = Policy(Severe.updateForm)
    val loose = Policy(Severe.loose)
  }

  case class Severe(
    missingField: Boolean,
    redundancyField: Boolean,
    multiplicity: Boolean,
    datatype: Boolean,
    form: Boolean
  )

  object Severe {
    val rigid = Severe(true, true, true, true, false)
    val update = Severe(false, false, false, true, false)
    val updateForm = Severe(false, false, false, true, true)
    val loose = Severe(false, false, false, false, true)
  }
}
