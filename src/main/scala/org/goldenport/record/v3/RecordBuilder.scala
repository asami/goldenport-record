package org.goldenport.record.v3

import java.sql.ResultSet
import org.goldenport.collection.VectorMap

/*
 * @since   Oct. 31, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
class RecordBuilder(
  schema: Schema,
  fields: VectorMap[String, FieldBuilder]
) {
  private def _fields = fields.valueVector

  def build(p: ResultSet): Record = {
    case class Z(xs: Vector[Field] = Vector.empty) {
      def r = Record(xs)

      def +(rhs: FieldBuilder) = rhs.build(p).
        map(x => copy(xs :+ x)).
        getOrElse(this)
    }
    _fields./:(Z())(_+_).r
    // (column.name +: column.aliases).toStream.flatMap(x =>
    //   rs.getObject(x) match {
    //     case null => None
    //     case m =>
    //       if (rs.wasNull)
    //         Some(Field.createEmpty(column.name))
    //       else
    //         Some(Field.create(column.name, m))
    //   }
    // ).headOption
  }

  def normalize(p: IRecord): Record = {
    // TODO default value
    case class Z(xs: Vector[Field] = Vector.empty) {
      def r = Record(xs)

      def +(rhs: FieldBuilder) = rhs.build(p).
        map(x => copy(xs :+ x)).
        getOrElse(this)
    }
    _fields./:(Z())(_+_).r
  }
}

object RecordBuilder {
  def apply(schema: Schema): RecordBuilder = {
    val fs = VectorMap(schema.columns.map(x => x.name -> FieldBuilder(x)))
    new RecordBuilder(schema, fs)
  }
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

  // def resultSetToRecord(schema: Schema, rs: ResultSet): Record = {
  //   val a = schema.columns.flatMap(getField(_, rs))
  //   Record(a)
  // }

  // def getField(column: Column, rs: ResultSet): Option[Field] =
  //   (column.name +: column.aliases).toStream.flatMap(x =>
  //     rs.getObject(x) match {
  //       case null => None
  //       case m =>
  //         if (rs.wasNull)
  //           Some(Field.createEmpty(column.name))
  //         else
  //           Some(Field.create(column.name, m))
  //     }
  //   ).headOption
