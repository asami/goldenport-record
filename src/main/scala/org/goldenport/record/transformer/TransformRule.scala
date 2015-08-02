package org.goldenport.record.transformer

import org.goldenport.record.v3._
import org.goldenport.record.v2
import org.goldenport.record.v2.{
  Schema, Column,
  MOne, MZeroOne, MOneMore, MZeroMore
}
import org.goldenport.record.v2.{MissingFieldFailure}

/*
 * @since   Jun. 21, 2015
 * @version Jul.  6, 2015
 * @author  ASAMI, Tomoharu
 */
sealed trait TransformRule {
  def isRequired: Boolean
  def transform(in: TransformerContext): TransformerContext

  protected def transform_field_required(
    from: Symbol,
    to: Symbol,
    in: TransformerContext
  ): TransformerContext = {
    in.inValue(from) match {
      case Some(s) => in.transformValue(from, to, s)
      case None => in.withValidation(MissingFieldFailure(from.name))
    }
  }

  protected def transform_field_option(
    from: Symbol,
    to: Symbol,
    in: TransformerContext
  ): TransformerContext = {
    in.inValue(from) match {
      case Some(x) => in.transformValue(from, to, x)
      case None => in
    }
  }

  protected def transform_column(
    from: Column,
    to: Column,
    in: TransformerContext
  ): TransformerContext = {
    (from.datatype, from.multiplicity, in.inValue(from.name)) match {
      case (d, MOne, None) => ???
      case (d, MOne, Some(EmptyValue)) => ???
      case (d, MOne, Some(SingleValue(v))) =>
        transform_column_one_single(to, d, v, in)
      case (d, MOne, Some(MultipleValue(v))) => ???
      case (d, MZeroOne, None) => ???
      case (d, MZeroOne, Some(EmptyValue)) => ???
      case (d, MZeroOne, Some(SingleValue(v))) => ???
      case (d, MZeroOne, Some(MultipleValue(v))) => ???
      case (d, MOneMore, None) => ???
      case (d, MOneMore, Some(EmptyValue)) => ???
      case (d, MOneMore, Some(SingleValue(v))) => ???
      case (d, MOneMore, Some(MultipleValue(v))) => ???
      case (d, MZeroMore, None) => ???
      case (d, MZeroMore, Some(EmptyValue)) => ???
      case (d, MZeroMore, Some(SingleValue(v))) => ???
      case (d, MZeroMore, Some(MultipleValue(v))) => ???
    }
  }

  protected def transform_column_one_single(
    to: Column,
    datatype: v2.DataType,
    value: Any,
    in: TransformerContext
  ): TransformerContext = {
    val (validation, v) = datatype.toInstanceV3V(value)
    in.outValueOne(to.name, v, validation)
  }

  protected def transform_column(
    c: Column,
    in: TransformerContext
  ): TransformerContext = {
    (c.datatype, c.multiplicity, in.inValue(c.name)) match {
      case (d, MOne, None) => ???
      case (d, MOne, Some(EmptyValue)) => ???
      case (d, MOne, Some(SingleValue(v))) =>
        transform_column_one(c, d, v, in)
      case (d, MOne, Some(MultipleValue(v))) => ???
      case (d, MZeroOne, None) => ???
      case (d, MZeroOne, Some(EmptyValue)) => ???
      case (d, MZeroOne, Some(SingleValue(v))) => ???
      case (d, MZeroOne, Some(MultipleValue(v))) => ???
      case (d, MOneMore, None) => ???
      case (d, MOneMore, Some(EmptyValue)) => ???
      case (d, MOneMore, Some(SingleValue(v))) => ???
      case (d, MOneMore, Some(MultipleValue(v))) => ???
      case (d, MZeroMore, None) => ???
      case (d, MZeroMore, Some(EmptyValue)) => ???
      case (d, MZeroMore, Some(SingleValue(v))) => ???
      case (d, MZeroMore, Some(MultipleValue(v))) => ???
    }
  }

  protected def transform_column_one(
    c: Column,
    datatype: v2.DataType,
    value: Any,
    in: TransformerContext
  ): TransformerContext = {
    val (validation, v) = datatype.toInstanceV3V(value)
    in.outValueOne(c.name, v, validation)
  }
}

case class SchemaRule(
  schema: Schema,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    schema.columns.foldLeft(in) { (z, x) =>
      transform_column(x, x, z) // TODO optimize
    }
  }
}

case class FieldRule1(
  field: Symbol,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    if (isRequired)
      transform_field_required(field, field, in)
    else
      transform_field_option(field, field, in)
  }
}

case class FieldRule1to1(
  from: Symbol,
  to: Symbol,
  convertf: Field => Field = identity,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}

case class FieldRule2to1(
  from1: Symbol, from2: Symbol,
  to: Symbol,
  convertf: (FieldValue, FieldValue) => Any,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    (in.inValue(from1), in.inValue(from2)) match {
      case (Some(v1), Some(v2)) =>
        val r = FieldValue.create(convertf(v1, v2))
        in.transformValue(Vector(from1, from2), to, r)
      case _ => in
    }
  }
}

case class FieldRuleNto1(
  froms: Seq[Symbol],
  to: Symbol,
  convertf: Seq[Field] => Field,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}

case class FieldRule1toN(
  from: Symbol,
  tos: Seq[Symbol],
  convertf: Field => Seq[Field],
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}

case class FieldRuleNtoN(
  froms: Seq[Symbol],
  tos: Seq[Symbol],
  convertf: Seq[Field] => Seq[Field],
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}

case class ColumnRule1(
  column: Column,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    transform_column(column, in)
  }
}

case class ColumnRule1to1(
  from: Column,
  to: Column,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    transform_column(from, to, in)
  }
}

case class ColumnRuleNto1(
  froms: Seq[Column],
  to: Column,
  convertf: Seq[Field] => Field,
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}

case class ColumnRule1toN(
  from: Column,
  tos: Seq[Column],
  convertf: Field => Seq[Field],
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}

case class ColumnRuleNtoN(
  froms: Seq[Column],
  tos: Seq[Column],
  convertf: Seq[Field] => Seq[Field],
  isRequired: Boolean = false
) extends TransformRule {
  def transform(in: TransformerContext) = {
    ???
  }
}
