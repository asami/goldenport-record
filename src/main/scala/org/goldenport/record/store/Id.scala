package org.goldenport.record.store

/*
 * @since   Mar. 30, 2019
 * @version Apr.  6, 2019
 * @author  ASAMI, Tomoharu
 */
trait Id {
  def string: String
  def show: String
}

object Id {
  case class StringId(id: String) extends Id {
    def string = id
    def show = string
  }
  case class IntId(id: Int) extends Id {
    def string = id.toString
    def show = string
  }
  case class LongId(id: Long) extends Id {
    def string = id.toString
    def show = string
  }

  def apply(p: String): Id = StringId(p)

  def create(p: Any): Id = p match {
    case m: String => StringId(m)
    case m: Long => LongId(m)
    case m: Int => IntId(m)
    case m => StringId(m.toString)
  }
}
