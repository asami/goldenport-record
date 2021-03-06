package org.goldenport.record.v2.util

import scalaz._, Scalaz._
import org.goldenport.i18n.I18NString
import org.goldenport.record.util.AnyUtils
import org.goldenport.record.v2._

/*
 * @since   Sep. 28, 2015
 *  version Oct. 27, 2015
 *  version Nov.  7, 2015
 *  version Feb.  3, 2016
 *  version Apr. 22, 2016
 *  version Jan. 19, 2017
 *  version Aug.  3, 2017
 *  version Sep. 27, 2017
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
case class SchemaBuilder(columns: Vector[Column] = Vector.empty) {
  import SchemaBuilder._

  def build: Schema = Schema(columns)

  def append(p: CBase, ps: CBase*): SchemaBuilder =
    copy(columns = columns ++ (p +: ps).map(_.toColumn))

  def update(name: String, datatype: DataType): SchemaBuilder =
    copy(columns = columns.map(x =>
      if (x.name === name)
        x.copy(datatype = datatype)
      else
        x
    ))
}

object SchemaBuilder {
  def apply(schema: Schema): SchemaBuilder = SchemaBuilder(schema.columns.toVector)

  def create(columns: CBase*): Schema = {
    Schema(columns.map(_.toColumn))
  }

  trait CBase {
    def toColumn: Column
  }

  case class CL(name: String, label: String) extends CBase {
    def toColumn = Column(
      name, XString, MZeroOne, label = Some(label))
  }

  case class CLej(name: String, en: String, ja: String) extends CBase {
    def toColumn = Column(
      name, XString, MZeroOne, i18nLabel = Some(I18NString(en, ja)))
  }

  case class CLT(name: String, label: String, datatype: DataType) extends CBase {
    def toColumn = Column(
      name, datatype, MZeroOne, label = Some(label))
  }

  case class CLejT(name: String, en: String, ja: String, datatype: DataType) extends CBase {
    def toColumn = Column(
      name, datatype, MZeroOne, i18nLabel = Some(I18NString(en, ja)))
  }

  case class CLP(name: String, label: String, powertype: PowertypeClass) extends CBase {
    def toColumn = Column(
      name, XPowertype(powertype), MZeroOne, label = Some(label))
  }

  case class CLPM(name: String, label: String, powertype: PowertypeClass, multiplicity: Multiplicity) extends CBase {
    def toColumn = Column(
      name, XPowertype(powertype), multiplicity, label = Some(label))
  }

  case class CLA(name: String, label: String, aliases: Seq[String]) extends CBase {
    def toColumn = Column(
      name, XString, MZeroOne, label = Some(label), aliases = aliases.toList)
  }

  case class CLAP(name: String, label: String, aliases: Seq[String], powertype: PowertypeClass) extends CBase {
    def toColumn = Column(
      name, XPowertype(powertype), MZeroOne, label = Some(label), aliases = aliases.toList)
  }

  case class CLD(name: String, label: String, displayformat: DisplayFormat) extends CBase {
    def toColumn = Column(
      name, XString, MZeroOne, label = Some(label), displayFormat = Some(displayformat))
  }

  case class CLV(name: String, label: String, value: Any) extends CBase {
    def toColumn = Column(
      name, XString, label = Some(label), form = Column.Form(
        value = Some(AnyUtils.toString(value))
      )
    )
  }

  case class CLTV(name: String, label: String, datatype: DataType, value: Any) extends CBase {
    def toColumn = Column(
      name, datatype, label = Some(label), form = Column.Form(
        value = Some(AnyUtils.toString(value))
      )
    )
  }

  case class CLI(name: String, label: String, importer: Importer) extends CBase {
    def toColumn = Column(
      name, label = Some(label), extension = Column.Extension.create(importer)
    )
  }

  case class CLTI(name: String, label: String, datatype: DataType, importer: Importer) extends CBase {
    def toColumn = Column(
      name, datatype, label = Some(label), extension = Column.Extension.create(importer)
    )
  }

  case class CT(name: String, datatype: DataType) extends CBase {
    def toColumn = Column(name, datatype, MZeroOne)
  }

  case class CT1(name: String, datatype: DataType) extends CBase {
    def toColumn = Column(name, datatype, MOne)
  }

  // case class CTC(name: String, datatype: DataType, constraint: Constraint) extends CBase {
  //   def toColumn = Column(name, datatype, MZeroOne, constraints = List(constraint))
  // }

  // case class CTC1(name: String, datatype: DataType, constraint: Constraint) extends CBase {
  //   def toColumn = Column(name, datatype, MOne, constraints = List(constraint))
  // }

  case class CTC(name: String, datatype: DataType, constraints: List[Constraint]) extends CBase {
    def toColumn = Column(name, datatype, MZeroOne, constraints = constraints)
  }

  case class CTC1(name: String, datatype: DataType, constraints: List[Constraint]) extends CBase {
    def toColumn = Column(name, datatype, MOne, constraints = constraints)
  }

  case class CO(name: String) extends CBase {
    def toColumn = Column(name, XString, MZeroOne, layout = Column.Layout(optional = true))
  }

  def mergeColumns(base: Seq[Column], adding: Seq[Column]): Seq[Column] = {
    case class Z(result: Vector[Column] = Vector.empty) {
      def +(rhs: Column) =
        if (base.exists(_.name == rhs.name))
          this
        else
          Z(result :+ rhs)
    }
    adding.foldLeft(Z(base.toVector))(_+_).result
  }
}
