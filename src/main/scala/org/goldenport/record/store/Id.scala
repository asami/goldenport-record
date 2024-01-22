package org.goldenport.record.store

import org.goldenport.extension.Showable

/*
 * @since   Mar. 30, 2019
 *  version Apr.  6, 2019
 *  version May.  9, 2019
 *  version Oct. 31, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
trait Id extends Showable {
  def literal: Any = string
  def string: String
  def show: String
  def print: String = show
  def display: String = show
}

object Id {
  case class StringId(id: String) extends Id {
    def string = id
    def show = string
  }
  case class IntId(id: Int) extends Id {
    override def literal = id
    def string = id.toString
    def show = string
  }
  case class LongId(id: Long) extends Id {
    override def literal = id
    def string = id.toString
    def show = string
  }

  def apply(p: String): Id = StringId(p)
  def apply(p: Int): Id = IntId(p)
  def apply(p: Long): Id = LongId(p)

  def create(p: Any): Id = p match {
    case m: String => StringId(m)
    case m: Long => LongId(m)
    case m: Int => IntId(m)
    case m => StringId(m.toString)
  }
}
