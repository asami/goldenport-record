package org.goldenport.record.v2

/*
 * @since   Sep. 25, 2015
 *  version Sep. 25, 2015
 * @version Mar.  8, 2019
 * @author  ASAMI, Tomoharu
 */
case class NumberPowertype(value: Int) extends Powertype {
  def name = value.toString
}

object NumberPowertype extends PowertypeClass {
  type T = NumberPowertype

  val elements = Vector(
    NumberPowertype(0),
    NumberPowertype(1),
    NumberPowertype(2)
  )

  override def get(v: Int) = Some(new NumberPowertype(v)) // elements.lift(v) orElse Some(NumberPowertype(v))
  override def get(v: String) = get(v.toInt)
}
