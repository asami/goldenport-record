package org.goldenport.record.v2.bag

import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import org.goldenport.bag.ChunkBag
import org.goldenport.record.v2._

/*
 * @since   Oct.  9, 2015
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
trait RecordBagMutator {
}

case class FlatRecordBagMutator(
  strategy: RecordBag.Strategy,
  bag: RecordBag
) extends RecordBagMutator {
}
