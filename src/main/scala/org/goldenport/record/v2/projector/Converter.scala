package org.goldenport.record.v2.projector

import scala.util.Try
import org.joda.time._
import org.joda.time.format.DateTimeFormat
import org.goldenport.util.{DateTimeUtils, DateUtils}
import org.goldenport.exception.RAISE

/*
 * @since   Jul. 19, 2018
 * @version Jul. 23, 2018
 * @author  ASAMI, Tomoharu
 */
trait Converter {
  def apply(p: Any)(implicit ctx: ProjectorContext) = p match {
    case xs: List[_] => apply_list(xs)
    case xs: Seq[_] => apply_seq(xs)
    case xs: Array[_] => apply_array(xs)
    case x => apply_one(x)
  }

  protected def apply_list(ps: List[_])(implicit ctx: ProjectorContext): Any = apply_List(ps)
  protected def apply_seq(ps: Seq[_])(implicit ctx: ProjectorContext): Any = apply_Seq(ps)
  protected def apply_array(ps: Array[_])(implicit ctx: ProjectorContext): Any = apply_Array(ps)
  protected def apply_one(p: Any)(implicit ctx: ProjectorContext): Any = apply_One(p)

  protected def apply_List(ps: List[_])(implicit ctx: ProjectorContext): Any = ps.map(apply_one)
  protected def apply_Seq(ps: Seq[_])(implicit ctx: ProjectorContext): Any = ps.map(apply_one)
  protected def apply_Array(ps: Array[_])(implicit ctx: ProjectorContext): Any = ps.map(apply_one)
  protected def apply_One(p: Any)(implicit ctx: ProjectorContext): Any = p
}

case object NoneConverter extends Converter {
  override def apply(p: Any)(implicit ctx: ProjectorContext) = p
}

case class PartialFunctionConverter(pf: PartialFunction[Any, Any]) extends Converter {
  override def apply(p: Any)(implicit ctx: ProjectorContext) = pf.lift(p).getOrElse(p)
}

case object YYYYMMDDConverter extends Converter {
  // migrate to goldenport
  val yyyymmddFormatter = DateTimeFormat.forPattern("yyyyMMdd").withZone(DateTimeUtils.jodajst)

  override protected def apply_One(p: Any)(implicit ctx: ProjectorContext) = p match {
    case m: String =>
      val a = Try(
        DateUtils.parse2(m)
      ).toOption.getOrElse(
        DateTimeUtils.parseIsoDateTimeJst(m)
      )
      apply(a)
    case m: DateTime => yyyymmddFormatter.print(m)
    case m: LocalDateTime => yyyymmddFormatter.print(m)
    case m: java.util.Date => yyyymmddFormatter.print(m.getTime)
    case m: LocalDate => yyyymmddFormatter.print(m)
    case m => apply(p.toString)
  }
}

object Converter {

}
