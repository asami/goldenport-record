package org.goldenport.record.v2.util

import scalaz._, Scalaz._
import org.goldenport.util.StringUtils
import org.goldenport.record.v2.Schema

/*
 * @since   Sep. 26, 2015
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
object TupleUtils {
  def makeHeader(schema: Schema): Vector[String] = {
    schema.columns.map(c => StringUtils.label(c.label, c.name)).toVector
  }

  def makeTupleVector[T](schema: Schema, fields: Seq[T]): Vector[(String, T)] = {
    schema.columns.map(_.name).zip(fields).toVector
  }

  def makeTupleVectorOption[T](schema: Schema, fields: Seq[Option[T]]): Vector[(String, Option[T])] = {
    schema.columns.map(_.name).zip(fields).toVector
  }

  def makeTupleVectorFlatten[T](schema: Schema, fields: Seq[Option[T]]): Vector[(String, T)] = {
    schema.columns.map(_.name).zip(fields).toVector collect {
      case (k, Some(v)) => k -> v
    }
  }

  def makeMap[T](schema: Schema, fields: Seq[T]): Map[String, T] = {
    makeTupleVector(schema, fields).toMap
  }

  def makeMapFlatten[T](schema: Schema, fields: Seq[Option[T]]): Map[String, T] = {
    makeTupleVectorFlatten(schema, fields).toMap
  }
}

