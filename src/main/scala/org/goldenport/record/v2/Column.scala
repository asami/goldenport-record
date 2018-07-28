package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._
import java.util.Locale
import com.asamioffice.goldenport.text.UString
import org.smartdox.Description
import org.goldenport.i18n.I18NString
import org.goldenport.record.v2.projector.ProjectorContext

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
 * @version Jul. 28, 2018
 * @author  ASAMI, Tomoharu
 */
case class Column(
  name: String,
  datatype: DataType = XString,
  multiplicity: Multiplicity = MOne,
  kind: ColumnKind = PlainKind,
  constraints: List[Constraint] = Nil,
//  orderBy: Option[SqlOrder] = None, // displayFormat
  visibility: Visibility = PlainVisibility,
  label: Option[String] = None, // C locale and last resort
  i18nLabel: Option[I18NString] = None,
  aliases: List[String] = Nil,
  sql: SqlColumn = NullSqlColumn,
  // formatter: Option[Formatter] = None, // unify displayFormat
  displaySequence: Option[Int] = None, // compatibility, unify displayFormat
  displayFormat: Option[DisplayFormat] = None,
  desc: Description = Description.empty,
  layout: Column.Layout = Column.Layout.empty,
  form: Column.Form = Column.Form.empty,
  extension: Column.Extension = Column.Extension.empty
//  operations: Seq[Operation] = Nil,
//  extjs: Map[String, Any] = Map.empty,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  properties: Map[String, Any] = Map.empty,
//  comment: String = ""
) extends ColumnSlot {
//  def displayFormat = extension.displayFormat
  override def toString() = s"Column(${show})"

  def show: String = s"$name,${datatype.name},${multiplicity.mark}"

  def withDatatype(p: DataType) = copy(datatype = p)
  def withPlaceholder(p: String) = copy(form = form.withPlaceholder(p))

  def converter = extension.converter
  def importer = extension.importer
  def exporter = extension.exporter
  def orderBy: Option[SqlOrder] = displayFormat.flatMap(_.orderBy)

  def validate(f: Field): ValidationResult = {
    _validate_datatype(f) |+| _validate_constraints(f)
  }

  private def _validate_datatype(f: Field): ValidationResult = {
    val a: Seq[ValidationResult] = f.values.map(
      _ match {
        case xs: Seq[_] => sys.error("???")
        case x => datatype.validate(x).enkey(f.key.name)
      }
    )
    a.toVector.suml
  }

  private def _validate_constraints(f: Field): ValidationResult = {
    // constraints
    Valid
  }

  def toModelValidation: List[String] = {
    List(_validation_presence,
        _validation_length,
        _validation_inclusion,
        _validation_exclusion,
        _validation_format).flatten
  }

  private def _validation_presence: Option[String] = {
    multiplicity match {
      case MOne => """{"field": "%s", "type": "presence"}""".format(name).some
      case _ => None
    }
  }

  private def _validation_length: Option[String] = {
    none
  }

  private def _validation_inclusion: Option[String] = {
    none
  }

  private def _validation_exclusion: Option[String] = {
    none
  }

  private def _validation_format: Option[String] = {
    none
  }

  def isSingle = multiplicity match {
    case MOne => true
    case MZeroOne => true
    case _ => false
  }

  def isMulti = !isSingle

  def isDerived = sql.isDerived

  def isAcceptColumnName(p: String): Boolean = nameCandidates.contains(p)

  lazy val nameCandidates: Vector[String] =
    Vector(name) ++ aliases ++ label.toVector

  def label(locale: Locale): String =
    i18nLabel.flatMap(_.get(locale)) orElse label getOrElse UString.capitalize(name)

  def isRequired: Boolean = multiplicity == MOne || multiplicity == MOneMore
  def getPlaceholder(locale: Locale): Option[String] = form.placeholder.flatMap(_.get(locale))

  /*
   * Format for display
   */
  def format(value: Option[List[Any]]): String = {
    def otherwise = value match {
      case None => ""
      case Some(xs) => xs.map(datatype.format).mkString(",")
    }
    displayFormat.flatMap(_.formatter) match {
      case Some(f) => f.format(this, value) getOrElse otherwise
      case None => otherwise
    }
  }
  // def format(value: Option[List[Any]]): String = {
  //   def otherwise = value match {
  //     case None => ""
  //     case Some(xs) => xs.map(datatype.format).mkString(",")
  //   }
  //   formatter match {
  //     case Some(f) => f.format(this, value) getOrElse otherwise
  //     case None => otherwise
  //   }
  // }

  /*
   * Import/Export and Convert
   */
  def importIn(rec: Record): Record = importIn(rec, rec)
  def importIn(src: Record, sink: Record): Record = {
    importer.fold(sink)(_.apply(this, src).
      fold(sink)(x => sink.update(name -> x))
    )
  }
  def importIn(ctx: ProjectorContext, rec: Record): Record = importIn(ctx, rec, rec)
  def importIn(ctx: ProjectorContext, src: Record, sink: Record): Record = {
    importer.fold(sink)(_.apply(ctx, this, src).
      fold(sink)(x => sink.update(name -> x))
    )
  }
  def exportOut(rec: Record): Record = exporter.fold(rec)(x => _transform(x.apply, rec))
  def convertIn(rec: Record): Record = converter.fold(rec)(x => _transform(x.convertIn, rec))
  def convertOut(rec: Record): Record = converter.fold(rec)(x => _transform(x.convertOut, rec))
  private def _transform(f: Any => Any, rec: Record): Record =
    rec.get(name).fold(rec)(x => rec.update(name -> x))
}

object Column {
  case class Grid(fraction: Int, denominator: Int)

  case class Layout(
    optional: Boolean = false,
    grid: Option[Grid] = None
  )
  object Layout {
    val empty = Layout()
  }

  case class Form(
    placeholder: Option[I18NString] = None,
    value: Option[String] = None,
    hidden: Boolean = false,
    readonly: Boolean = false
    // invisible: Boolean = false : Visibility
  ) {
    def withPlaceholder(p: String) = copy(placeholder = Some(I18NString(p)))
    def isEmpty = this == Form.empty
  }
  object Form {
    val empty = Form()
  }

  case class Extension(
//    displayFormat: Option[DisplayFormat],
    converter: Option[Converter], // convert to/from internal datatype
    importer: Option[Importer], // import data to represent datattype
    exporter: Option[Exporter] // export data from represent datatype
  )

  object Extension {
    val empty = Extension(None, None, None)

    def create(importer: Importer): Extension = Extension(None, Some(importer), None)

    //    def apply(p: DisplayFormat): Extension = Extension(Some(p), None, None, None)

  }
}

sealed trait ColumnKind {
}
case object PlainKind extends ColumnKind
case object IdKind extends ColumnKind
case object NameKind extends ColumnKind
