package org.goldenport.record.unitofwork

/*
 * @since   Nov. 15, 2015
 * @version Nov. 25, 2015
 * @author  ASAMI, Tomoharu
 */
trait Store {
}

object Store {
  val printer = new Store {
  }

  case class IdStore(id: Id) extends Store {
  }

  def apply(id: String) = IdStore(StringId(id))

  sealed trait Id {
    def s: String = toStringRepresentation
    def toStringRepresentation: String
    override def toString() = toStringRepresentation
  }

  case class StringId(v: String) extends Id {
    def toStringRepresentation: String = v
  }
}
