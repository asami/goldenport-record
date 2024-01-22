package org.goldenport.record.v3

import org.goldenport.record.v2.{Schema => V2Schema, Column => V2Column}

/*
 * @since   Nov. 23, 2012
 *  version Dec. 28, 2012
 *  version Jan. 30, 2013
 *  version Mar. 12, 2013
 *  version Apr. 26, 2013
 *  version Jun. 24, 2013
 *  version Oct. 23, 2013
 *  version Feb.  6, 2014
 *  version Jun.  9, 2014
 *  version Jul. 25, 2014
 *  version Aug.  6, 2014
 *  version Sep. 25, 2015
 *  version Oct. 15, 2015
 *  version May. 26, 2016
 *  version Sep.  8, 2016
 *  version Jan. 21, 2017
 *  version May. 25, 2017
 *  version Aug.  1, 2017
 *  version Sep. 21, 2017
 *  version Oct. 25, 2017
 *  version Nov. 23, 2017
 *  version Dec. 13, 2017
 *  version Jan. 22, 2018
 *  version May. 16, 2018
 *  version Jul. 28, 2018
 *  version Aug. 29, 2018
 *  version Sep.  5, 2018
 *  version Jan.  1, 2019
 *  version Feb. 12, 2019
 *  version Apr. 29, 2019
 *  version May.  1, 2019
 *  version Aug. 20, 2019
 *  version Oct.  1, 2019
 *  version Dec. 30, 2019
 *  version Jan. 12, 2020
 *  version Apr. 17, 2020
 *  version Jun.  8, 2020
 *  version Mar. 17, 2021
 * @version Oct. 31, 2021 restart
 * @author  ASAMI, Tomoharu
 */
case class Schema(
  columns: Vector[Column]
) {
}

object Schema {
  def from(s: V2Schema): Schema = {
    val cs = s.columns.toVector.map(Column.from)
    new Schema(cs)
  }
}
