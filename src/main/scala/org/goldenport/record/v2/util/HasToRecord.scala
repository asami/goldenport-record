package org.goldenport.record.v2.util

import org.goldenport.record.v2._

/*
 * @since   May. 21, 2020
 * @version May. 21, 2020
 * @author  ASAMI, Tomoharu
 */
object HasToRecord {
  import scala.language.reflectiveCalls
  def unapply(o: AnyRef): Option[{def toRecord: Record}] = {
    if (BeanUtils.containsMethod(o, "toRecord", Array()))
      Some(o.asInstanceOf[{def toRecord: Record}])
    else
      None
  }
}
