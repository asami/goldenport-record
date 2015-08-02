package org.goldenport.record.transformer

import org.goldenport.record.v3._
import org.goldenport.record.v2.{Schema, Column, ValidationResult}

/*
 * @since   Jul.  6, 2015
 * @version Aug.  2, 2015
 * @author  ASAMI, Tomoharu
 */
case class TransformerContext(
  in: Record,
  done: Set[Symbol],
  out: Record
) {
  def asis: TransformerContext = {
    copy(out = in)
  }

  def withException(e: Throwable): TransformerContext = {
    copy(out = out.withException(e, in))
  }

  def withValidation(v: ValidationResult): TransformerContext = {
    copy(out = out.withValidation(v))
  }

  def transform(from: Symbol, to: Symbol, value: Any): TransformerContext = {
    copy(done = done + from, out = out.update(to, value))
  }

  def transformValue(from: Symbol, to: Symbol, value: FieldValue): TransformerContext = {
    copy(done = done + from, out = out.updateValue(to, value))
  }

  def transformValue(froms: Seq[Symbol], to: Symbol, value: FieldValue): TransformerContext = {
    copy(done = done ++ froms, out = out.updateValue(to, value))
  }

  def inValue(key: Symbol): Option[FieldValue] = {
    in.getValue(key)
  }

  def inValue(key: String): Option[FieldValue] = {
    in.getValue(key)
  }

  def outValueOne(
    key: String,
    value: Any,
    validation: ValidationResult
  ): TransformerContext = {
    copy(out = out.updateValueOne(key, value, validation))
  }

  def remainder: Record = in.removeFields(done)
}

object TransformerContext {
  def create(rec: Record) = TransformerContext(rec, Set.empty, Record.empty)
}
