package org.goldenport.record.v2.projector

import scalaz._, Scalaz._
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.record.command.NullValue
import org.goldenport.record.v2.util.RecordAux
import org.goldenport.values.PathName

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
 *  version Aug. 30, 2017
 *  version May. 16, 2018
 * @version Jul. 22, 2018
 * @author  ASAMI, Tomoharu
 */
case class Projector(
  schema: Schema,
  policy: Projector.Policy = Projector.Policy.rigid,
  builder: Option[Builder] = None
) {
  import Projector._
  val multiplicityRegex = """^(.*)_(\d+)$""".r

  def apply(rec: Record): \/[ValidationResult, Record] = apply(rec, rec)

  def apply(src: Record, sink: Record): \/[ValidationResult, Record] = {
    implicit val ctx: ProjectorContext = ProjectorContext.default
    val a = builder.fold(src)(_.apply(src, sink))
    val b = schema.importIn(a)
    val c = _normalize(b)
    val r = schema.complement(c)
    schema.validate(r, policy) match {
      case Valid => _project(r).right
      case x: Warning => _project(r).right
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
    val bb: Map[String, List[Any]] = {
      val paths = schema.columns.
        flatMap(c => c.aliases.map(PathSlot(c.name, _))).
        filter(_.isAvailable)
      // println(s"paths: $paths")
      case class Z(xs: Map[String, List[Any]], resolved: Map[String, List[Any]] = Map.empty) {
        def r = xs ++ resolved
        def +(rhs: PathSlot) = {
          @annotation.tailrec
          def go(p: Any, path: List[String]): Option[Any] = {
            p match {
              case rec: Record => path match {
                case Nil => None
                case x :: xs => rec.get(x) match {
                  case None => None
                  case Some(s) => if (xs.isEmpty)
                    Some(s)
                  else
                    go(s, xs)
                }
              }
              case _ => None
            }
          }
          // println(s"z: ${xs.get(rhs.path.firstComponent)}")
          xs.get(rhs.path.firstComponent) match {
            case None => this
            case Some(s) => copy(resolved = resolved + (rhs.name -> _flatten(s.flatMap { x =>
              go(x, rhs.path.components.tail)
            })))
          }
        }
      }
      paths./:(Z(b))(_+_).r
    }
    // println("bb: " + bb)
    val c: List[Field] = bb.map {
      case (k, v) => Field(Symbol(k), v)
    }.toList
    val d = _normalize_form(c)
    val e = _normalize_datatype(d)
    rec.copy(fields = e)
  }

  private def _flatten(ps: List[Any]): List[Any] =
    ps.flatMap {
      case xs: List[_] => _flatten(xs)
      case x => List(x)
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
    val export = Policy(Severe.export)
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
    val export = Severe(false, true, false, true, false)
    val loose = Severe(false, false, false, false, true)
  }

  case class PathSlot(name: String, path: PathName) {
    def isAvailable = path.components.length > 1
  }
  object PathSlot {
    def apply(name: String, path: String): PathSlot = PathSlot(name, PathName(path))
  }
}
