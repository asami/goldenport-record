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
 *  version Nov. 15, 2019
 * @version Sep. 30, 2023
 * @author  ASAMI, Tomoharu
 */
case class Query(
  selection: Query.Selection = Query.Selection.all,
  projection: Query.Projection = Query.Projection.all,
  transfer: Query.Transfer = Query.Transfer.default
) {
  import Query._

  def withExpression(ps: Seq[Expression]) = copy(selection = selection.withExpression(ps))
  def withOffsetLimit(offset: Int, limit: Transfer.Limit) = copy(transfer = transfer.withOffsetLimit(offset, limit))
  def withProjectionColumns(cs: Seq[String]): Query = copy(projection = Projection.columns(cs))

  def withProjectionColumns(cs: Option[Seq[String]]): Query =
    cs.fold(this)(withProjectionColumns)

  def offset: Int = transfer.offset
  def limitClause: String = transfer.limit match {
    case Transfer.Limit.All => ""
    case Transfer.Limit.Value(n) => s" LIMIT $n "
  }
  def columnsClause: String = projection.columnsCluase

  def where(implicit ctx: SqlContext): String = selection.where

  def where(schema: Option[Schema])(implicit ctx: SqlContext): String =
    schema.map(where(_)).getOrElse(where)

  def where(schema: Schema)(implicit ctx: SqlContext): String = selection.where(schema)
}

object Query {
  val default = Query(Selection.all, Projection.all, Transfer.default)
  val all = Query(Selection.all, Projection.all, Transfer.all)

  case class Context(
    default: Context.Default = Context.Default.default,
    expression: QueryExpression.Context
  ) {
  }
  object Context {
    case class Default(
      transferLimit: Transfer.Limit = Transfer.Limit.default
    )
    object Default {
      val default = Default()
    }
    def now(): Context = Context(expression = QueryExpression.Context.now())
  }

  case class Selection(
    stringExpression: Option[String],
    expressions: Option[Query.AndExpression]
  ) {
    def withExpression(ps: Seq[Expression]) = {
      val a = if (ps.isEmpty) None else Some(AndExpression(ps.toVector))
      copy(expressions = a)
    }

    def where(implicit ctx: SqlContext): String =
      List(stringExpression, expressions.map(_.where)).flatten match {
        case Nil => "1 = 1"
        case x :: Nil => x
        case xs => xs.mkString("(", " AND ", ")")
      }

    def where(schema: Schema)(implicit ctx: SqlContext): String =
      List(stringExpression, expressions.map(_.where(schema))).flatten match {
        case Nil => "1 = 1"
        case x :: Nil => x
        case xs => xs.mkString("(", " AND ", ")")
      }
  }
  object Selection {
    val all = Selection(None, None)
  }

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

  sealed trait Projection {
    def columnsCluase: String
  }
  object Projection {
    val all: Projection = All

    case object All extends Projection {
      def columnsCluase = "*"
    }
    case class Columns(columns: List[String]) extends Projection {
      // def columnsCluase = columns.map(x => s"'$x'").mkString(", ")
      def columnsCluase = columns.map(x => column_literal(x)).mkString(", ")

      protected def column_literal(p: String) = p // H2
    }

    def columns(ps: Seq[String]): Projection = Columns(ps.toList)
  }

  case class Transfer(offset: Int, limit: Transfer.Limit) {
    def withOffsetLimit(offset: Int, limit: Transfer.Limit) =
      copy(offset = offset, limit = limit)
  }
  object Transfer {
    val default = Transfer(0, Limit.default)
    val all = Transfer(0, Limit.All)

    sealed trait Limit
    object Limit {
      case object All extends Limit
      case class Value(limit: Int) extends Limit

      val default = Value(1000)

      def apply(p: Int): Limit = Value(p)
    }
  }

  def apply(ps: Seq[Expression]): Query = Query.default.withExpression(ps)

  def create(
    p: String
  )(implicit context: Query.Context): Query = create(Record.fromLxsv(p)) // XXX : more flexible

  def create(
    p: IRecord
  )(implicit context: Query.Context): Query = create(p.toRecord)

  def create(
    p: Record
  )(implicit context: Query.Context): Query = {
    val xs: Seq[Expression] = p.fields.map { x =>
      val (key, expr) = QueryExpression.parse(x)(context.expression)
      QueryExpressionExpression(key.path, expr)
    }
    apply(xs)
  }
}
