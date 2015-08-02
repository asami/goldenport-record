package org.goldenport.record.v3

import com.github.nscala_time.time.Imports._

/*
 * @since   Nov. 23, 2012
 *  version Dec. 18, 2012
 *  version Jan. 29, 2013
 *  version Feb. 20, 2013
 *  version Mar. 12, 2013
 *  version Dec. 31, 2013
 *  version Jan. 29, 2014
 *  version Feb.  6, 2014
 *  version May. 15, 2014
 *  version Jul. 27, 2014
 * @version Jun. 21, 2015
 * @author  ASAMI, Tomoharu
 */
sealed trait DataType {
  type InstanceType

  def toInstance(v: Any): InstanceType
}

case object XDateTime extends DataType {
  type InstanceType = DateTime

  def toInstance(v: Any) = ???
}

object DataType {
}
