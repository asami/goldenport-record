package org.goldenport.record.v3

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.values.Designation
import org.goldenport.context.ValueDomainFault
import org.goldenport.record.v2.{DataType, XString}
import org.goldenport.record.v2.{Multiplicity, MOne, MZeroOne}

/*
 * @since   Dec.  8, 2012
 *  version Dec. 12, 2012
 *  version Feb. 20, 2013
 *  version Mar.  3, 2013
 *  version Oct. 23, 2013
 *  version Jan. 20, 2014
 *  version Jul. 25, 2014
 *  version Aug. 11, 2014
 *  version Oct. 27, 2015
 *  version Nov.  8, 2015
 *  version Feb. 26, 2016
 *  version Jan. 15, 2017
 *  version Aug.  1, 2017
 *  version Sep. 27, 2017
 *  version Oct. 22, 2017
 *  version Nov. 12, 2017
 *  version Dec. 13, 2017
 *  version Apr. 10, 2018
 *  version Jul. 28, 2018
 *  version Aug. 24, 2018
 *  version Sep.  4, 2018
 *  version Jan.  9, 2019
 *  version Jul.  7, 2019
 *  version Aug. 23, 2019
 *  version Oct.  9, 2019
 *  version Feb. 25, 2020
 *  version Mar. 30, 2020
 *  version May. 11, 2020
 *  version Jun.  1, 2020
 *  version Mar. 21, 2021
 *  version Mar. 25, 2021 restart
 * @version Apr. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class Column(
  designation: Designation,
  domain: ValueDomain
) extends Designation.Holder {
  def datatype = domain.datatype
  def multiplicity = domain.multiplicity
  def constraints = domain.constraints
  def isSingle = domain.isSingle

  def resolve(p: Any): ValidationNel[ValueDomainFault, Any] = domain.resolve(p)
}

object Column {
  trait Holder extends Designation.Holder {
    def column: Column
    def designation = column.designation
  }

  def apply(name: String, datatype: DataType): Column = apply(name, datatype, MOne)

  def apply(name: String, datatype: DataType, multiplicity: Multiplicity): Column = {
    Column(Designation(name), ValueDomain(datatype, multiplicity))
  }
}
