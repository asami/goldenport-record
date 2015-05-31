package org.goldenport.record

/*
 * @since   May. 31, 2015
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
trait TCRecordSet[T] {
//  def exportTo(rs: T, file: String): TCRecordSet[T]
  def toV2(rs: T): v2.RecordSet
//  def toV3(rs: T): v3.RecordSet
  def foreach(rs: T, op: TCRecord[T] => Unit): Unit
  def foldLeft[A](rs: T, z: A)(op: (A, TCRecord[T]) => A): A
}
