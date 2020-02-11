package org.goldenport.record.store

import org.goldenport.RAISE
import org.goldenport.record.query.QueryExpression
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.v2.Schema

/*
 * @since   Mar. 30, 2019
 *  version Apr. 20, 2019
 *  version May.  8, 2019
 *  version Jul. 29, 2019
 * @version Nov. 15, 2019
 * @author  ASAMI, Tomoharu
 */
case class Query(
  stringExpression: Option[String],
  expressions: Option[Query.AndExpression]
) {
  def where(implicit ctx: SqlContext): String =
    List(stringExpression, expressions.map(_.where)).flatten match {
      case Nil => "1 = 1"
      case x :: Nil => x
      case xs => xs.mkString("(", " AND ", ")")
    }

  def where(schema: Option[Schema])(implicit ctx: SqlContext): String =
    schema.map(where(_)).getOrElse(where)

  def where(schema: Schema)(implicit ctx: SqlContext): String =
    List(stringExpression, expressions.map(_.where(schema))).flatten match {
      case Nil => "1 = 1"
      case x :: Nil => x
      case xs => xs.mkString("(", " AND ", ")")
    }
}

object Query {
  val all = Query(None, None)

  sealed trait Expression {
    def where(implicit ctx: SqlContext): String
    def where(schema: Schema)(implicit ctx: SqlContext): String
  }

  sealed trait ControlExpression extends Expression {
  }

  case class QueryExpressionExpression(
    fieldName: String,
    expression: QueryExpression
  ) extends Expression {
    def where(implicit ctx: SqlContext): String = expression.expression(fieldName)
    def where(schema: Schema)(implicit ctx: SqlContext): String = expression.where(schema, fieldName)
  }

  case class AndExpression(
    expressions: Vector[Expression]
  ) extends ControlExpression {
    def where(implicit ctx: SqlContext): String = expressions match {
      case Vector() => "1 = 1"
      case Vector(x) => x.where
      case xs => xs.map(_.where).mkString("(", " AND ", ")")
    }
    def where(schema: Schema)(implicit ctx: SqlContext): String = expressions match {
      case Vector() => "1 = 1"
      case Vector(x) => x.where(schema)
      case xs => xs.map(_.where(schema)).mkString("(", " AND ", ")")
    }
  }
  object AndExpression {
    def apply(ps: Seq[Expression]): AndExpression = AndExpression(ps.toVector)
  }

  case class OrExpression(
    expressions: Vector[Expression]
  ) extends ControlExpression {
    def where(implicit ctx: SqlContext): String = expressions match {
      case Vector() => "1 = 0"
      case Vector(x) => x.where
      case xs => xs.map(_.where).mkString("(", " OR ", ")")
    }
    def where(schema: Schema)(implicit ctx: SqlContext): String = expressions match {
      case Vector() => "1 = 0"
      case Vector(x) => x.where(schema)
      case xs => xs.map(_.where(schema)).mkString("(", " OR ", ")")
    }
  }
  object OrExpression {
    def apply(ps: Seq[Expression]): OrExpression = OrExpression(ps.toVector)
  }

  case class NotExpression(
    expression: Expression
  ) extends ControlExpression {
    def where(implicit ctx: SqlContext): String = s"(NOT ${expression.where})"
    def where(schema: Schema)(implicit ctx: SqlContext): String = s"(NOT ${expression.where(schema)})"
  }
  object NotExpression {
    def apply(ps: Seq[Expression]): NotExpression = ps match {
      case Seq() => RAISE.syntaxErrorFault("No expression in NOT.")
      case Seq(x) => NotExpression(x)
      case xs => NotExpression(OrExpression(xs))
    }
  }

  def apply(ps: Seq[Expression]): Query = {
    val a = if (ps.isEmpty) None else Some(AndExpression(ps.toVector))
    Query(None, a)
  }

  def create(
    p: String
  )(implicit context: QueryExpression.Context): Query = create(Record.fromLxsv(p)) // XXX : more flexible

  def create(
    p: IRecord
  )(implicit context: QueryExpression.Context): Query = create(p.toRecord)

  def create(
    p: Record
  )(implicit context: QueryExpression.Context): Query = {
    val xs: Seq[Expression] = p.fields.map { x =>
      val (key, expr) = QueryExpression.parse(x)
      QueryExpressionExpression(key.path, expr)
    }
    apply(xs)
  }
}
