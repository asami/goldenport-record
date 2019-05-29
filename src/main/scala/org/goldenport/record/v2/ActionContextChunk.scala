package org.goldenport.record.v2

import scala.util.control.NonFatal

/*
 * @since   May. 28, 2019
 * @version May. 28, 2019
 * @author  ASAMI, Tomoharu
 */
case class ActionContextChunk(
  slots: Vector[ActionContextChunk.Slot],
  connection: Option[java.sql.Connection]
) {
  import ActionContextChunk._

  def toActionContexts: Vector[ActionContext] =
    connection.map(c => slots.map(_.ac.withConnection(c))).getOrElse(slots.map(_.ac))

  def withActionContexts(ps: Seq[ActionContext]): ActionContextChunk =
    copy(slots = ps.toVector.map(Slot(_)))

  def withException(e: Throwable): ActionContextChunk =
    copy(slots = slots.map(_.withException(e)))

  def run(body: Seq[ActionContext] => Seq[ActionContext]): ActionContextChunk = try {
    withActionContexts(body(toActionContexts))
  } catch {
    case NonFatal(e) => withException(e)
  }
}

object ActionContextChunk {
  case class Slot(ac: ActionContext, exception: Option[Throwable] = None) {
    def withException(e: Throwable): Slot = copy(exception = Some(e))
  }

  def apply(ps: Seq[ActionContext]): ActionContextChunk =
    ActionContextChunk(ps.toVector.map(Slot(_)), None)

  def apply(ps: Seq[ActionContext], conn: Option[java.sql.Connection]): ActionContextChunk =
    ActionContextChunk(ps.toVector.map(Slot(_)), conn)

  def run(acs: Seq[ActionContext])(body: ActionContextChunk => ActionContextChunk): Seq[ActionContext] =
    body(ActionContextChunk(acs)).toActionContexts
}
