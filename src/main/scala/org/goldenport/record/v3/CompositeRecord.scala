package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Apr. 20, 2019
 * @version Apr. 20, 2019
 * @author  ASAMI, Tomoharu
 */
case class CompositeRecord(records: NonEmptyVector[IRecord]) extends IRecord {
  private def _records = records.vector
  def keyNames: List[String] = _records.flatMap(_.keyNames).distinct.toList
  val print: String = toRecord.print
  val show: String = toRecord.show
  lazy val toRecord: Record = {
    case class Z(xs: Vector[Field] = Vector.empty) {
      def r = Record(xs)
      def +(rhs: IRecord) = {
        val a = rhs.fields./:(xs)((z, x) =>
          if (xs.exists(_.key == x.key))
            z
          else
            z :+ x
        )
        Z(a)
      }
    }
    _records./:(Z())(_+_).r
  }
  def fields: Seq[Field] = toRecord.fields
  def isEmpty: Boolean = _records.forall(_.isEmpty)
  def isDefined(key: String): Boolean = _records.exists(_.isDefined(key))
  def isDefined(key: Symbol): Boolean = _records.exists(_.isDefined(key))
  def get(key: String): Option[Any] = _records.toStream.flatMap(_.get(key)).headOption
  def get(key: Symbol): Option[Any] = _records.toStream.flatMap(_.get(key)).headOption
  def getList(key: String): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getList(key: Symbol): Option[List[Any]] = RAISE.notImplementedYetDefect
  def getRecord(key: String): Option[Record] = RAISE.notImplementedYetDefect
  def getRecord(key: Symbol): Option[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: String): List[Record] = RAISE.notImplementedYetDefect
  def takeRecordList(key: Symbol): List[Record] = RAISE.notImplementedYetDefect
  def update(rhs: IRecord): IRecord = CompositeRecord(rhs +: records)
  def complement(rhs: IRecord): IRecord = CompositeRecord(records :+ rhs)
}

object CompositeRecord {
  def apply(p: IRecord, ps: IRecord*): CompositeRecord = CompositeRecord(NonEmptyVector(p, ps.toVector))
}
