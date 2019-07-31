package org.goldenport.record.store

import org.goldenport.collection.NonEmptyVector
import org.goldenport.record.query.QueryExpression

/*
 * @since   Mar. 30, 2019
 *  version Apr. 20, 2019
 *  version May.  8, 2019
 * @version Jul. 29, 2019
 * @author  ASAMI, Tomoharu
 */
case class Query(
  stringExpression: Option[String],
  expressions: Option[NonEmptyVector[Query.Expression]]
) {
  def where: String = "1 = 1" // TODO
}

object Query {
  val all = Query(None, None)

  sealed trait Expression {
  }

  sealed trait ControlExpression extends Expression {
  }

  case class QueryExpressionExpression(
    fieldName: String,
    expression: QueryExpression
  ) extends Expression {
  }

  case class AndExpression(
    expressions: Vector[Expression]
  ) extends ControlExpression {
  }
  object AndExpression {
    def apply(ps: Seq[Expression]): AndExpression = AndExpression(ps.toVector)
  }

  case class OrExpression(
    expressions: Vector[Expression]
  ) extends ControlExpression {
  }
  object OrExpression {
    def apply(ps: Seq[Expression]): OrExpression = OrExpression(ps.toVector)
  }

  case class NotExpression(
  ) extends ControlExpression {
  }
  object NotExpression {
    def apply(ps: Seq[Expression]): NotExpression = NotExpression(ps.toVector)
  }

  def apply(ps: Seq[Expression]): Query = Query(None, NonEmptyVector.createOption(ps))
}
