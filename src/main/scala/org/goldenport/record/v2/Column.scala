package org.goldenport.record.v2

import scalaz._, Scalaz._
import Validator._
import java.util.Locale
import com.asamioffice.goldenport.text.UString
import org.goldenport.extension.Description
import org.goldenport.i18n.I18NString
import org.goldenport.record.query.QueryExpression
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
 *  version Jul. 28, 2018
 *  version Aug. 24, 2018
 *  version Sep.  4, 2018
 *  version Jan.  9, 2019
 *  version Jul.  7, 2019
 *  version Aug. 23, 2019
 *  version Oct.  9, 2019
 *  version Feb. 25, 2020
 *  version Mar. 30, 2020
 * @version May. 11, 2020
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
  xml: Column.Xml = Column.Xml.default,
  extension: Column.Extension = Column.Extension.empty
//  operations: Seq[Operation] = Nil,
//  extjs: Map[String, Any] = Map.empty,
//  isAvailableMode: ExecutionMode => Boolean = _ => true,
//  properties: Map[String, Any] = Map.empty,
//  comment: String = ""
) extends ColumnSlot {
//  def displayFormat = extension.displayFormat
  override def toString() = if (Schema.isDebug)
    s"Column(${showlong})"
  else
    s"Column(${show})"

  lazy val key: Symbol = Symbol(name)
  def show: String = s"$name,${datatype.name},${multiplicity.mark}"
  def showlong: String = s"${show},${form},${extension}"

  def withDatatype(p: DataType) = copy(datatype = p)
  def withPlaceholder(p: String) = copy(form = form.withPlaceholder(p))
  def withDefault(p: Any) = copy(extension = extension.withDefault(p))

  def converter = extension.converter
  def importer = extension.importer
  def exporter = extension.exporter
  def default = extension.default
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

  lazy val nameCandidates: Vector[String] = {
    val a = Vector(name) ++ aliases ++ label.toVector ++ i18nLabel.map(_.values).getOrElse(Nil)
    a.distinct
  }

  def label(locale: Locale): String =
    i18nLabel.flatMap(_.get(locale)) orElse label getOrElse UString.capitalize(name)

  def labelI18NString: I18NString =
    i18nLabel orElse label.map(I18NString(_)) getOrElse I18NString(UString.capitalize(name))

  def isRequired: Boolean = multiplicity == MOne || multiplicity == MOneMore

  def getPlaceholder(locale: Locale): Option[String] = form.placeholder.flatMap(_.get(locale))

  def sqlColumnName: String = sql.getColumnName getOrElse name

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

  def toQueryExpression(ctx: QueryExpression.Context, p: Any): QueryExpression =
    datatype.toQueryExpression(ctx, p)

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
  val PROP_NAME = "name"
  val PROP_DATATYPE = "datatype"
  val PROP_MULTIPLICITY = "multiplicity"
  val PROP_LABEL = "label"
  val PROP_I18N_LABEL = "labelI18N"
  val PROP_FROM = "form"

  def comp(lhs: Column, rhs: Column): List[String] =
    List(
      lhs.name != rhs.name option s"name(${lhs.name}, ${rhs.name})",
      lhs.datatype != rhs.datatype option s"datatype(${lhs.datatype}, ${rhs.datatype}",
      lhs.label != rhs.label option s"label(${lhs.label}, ${rhs.label}",
      lhs.form != rhs.form option s"form(${lhs.form}, ${rhs.form}",
      lhs.extension != rhs.extension option s"extension(${lhs.extension}, ${rhs.extension}"
    ).flatten

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
    def withValueOption(p: Option[String]) = p.fold(this)(x => copy(value = Some(x)))
    def isEmpty = this == Form.empty
  }
  object Form {
    val empty = Form()
  }

  case class Xml(
    isAttribute: Option[Boolean] = None,
    namespaceUri: Option[String] = None,
    prefix: Option[String] = None,
    name: Option[String] = None
  )
  object Xml {
    val default = Xml()
    val attribute = Xml(Some(true))
    val element = Xml(Some(false))
  }

  case class Extension(
//    displayFormat: Option[DisplayFormat],
    converter: Option[Converter], // convert to/from internal datatype
    importer: Option[Importer], // import data to represent datatype
    exporter: Option[Exporter], // export data from represent datatype
    default: Option[Any]
  ) {
    def withDefault(p: Any) = copy(default = Some(p))
  }

  object Extension {
    val empty = Extension(None, None, None, None)

    def create(importer: Importer): Extension = Extension(None, Some(importer), None, None)

    //    def apply(p: DisplayFormat): Extension = Extension(Some(p), None, None, None)
  }

  object record {
    def unmarshall(p: Record): Column = {
      val name = p.asString(PROP_NAME)
      val datatype = p.getConcreteString(PROP_NAME).map(DataType.to) getOrElse XString
      val multiplicity = p.getConcreteString(PROP_MULTIPLICITY).map(Multiplicity.to) getOrElse MOne
      val label = p.getConcreteString(PROP_LABEL)
      val i18nLabel = None // TODO
      val form = Form.empty // TODO
      Column(name, datatype, multiplicity,
        label = label,
        i18nLabel = i18nLabel,
        form = form
      )
    }
  }
}

sealed trait ColumnKind {
}
case object PlainKind extends ColumnKind
case object IdKind extends ColumnKind
case object NameKind extends ColumnKind
