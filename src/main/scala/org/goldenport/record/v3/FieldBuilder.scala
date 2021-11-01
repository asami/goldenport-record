package org.goldenport.record.v3

import java.sql.ResultSet

/*
 * @since   Oct. 31, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
class FieldBuilder(
  column: Column,
  ignoreCase: Boolean = true
) {
  def name = column.name
  def datatype = column.datatype

  private val _is_match: Field => Boolean =
    if (ignoreCase)
      (p: Field) => p.name.equalsIgnoreCase(column.name)
    else
      (p: Field) => p.name == column.name

  def build(p: IRecord): Option[Field] =
    for {
      s <- p.fields.find(_is_match)
      x <- s.getValue
    } yield {
      val v = datatype.toInstance(x)
      Field.create(name, v)
    }

  def build(rs: ResultSet): Option[Field] = {
    (column.name +: column.aliases).toStream.flatMap(x =>
      rs.getObject(x) match {
        case null => None
        case m =>
          if (rs.wasNull)
            Some(Field.createEmpty(column.name))
          else
            Some(Field.create(column.name, m))
      }
    ).headOption
  }
}

object FieldBuilder {
  def apply(column: Column): FieldBuilder = new FieldBuilder(column)
}

    // private def _apply_schema(s: Schema, p: IRecord): Record = {
    //   // TODO default value
    //   case class Z(xs: Vector[Field] = Vector.empty) {
    //     def r = Record(xs)

    //     def +(rhs: Column) = {
    //       p.fields.find(_.name.equalsIgnoreCase(rhs.name)) match {
    //         case Some(s) => s.getValue.map { x =>
    //           val v = rhs.datatype.toInstance(x)
    //           val a = Field.create(rhs.name, v)
    //           copy(xs = xs :+ a)
    //         }.getOrElse(this)
    //         case None => this
    //       }
    //     }
    //   }
    //   s.columns./:(Z())(_+_).r
    // }
