package org.goldenport.record.v2.bag

import scalaz.stream._
import scalaz.concurrent.Task

/*
 * @since   Jun. 13, 2014
 *  version Jul. 25, 2014
 *  version Oct. 15, 2014
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
class TokensBag(private val csv: CsvBag) {
  def isEmpty = csv.isEmpty

  def toIterator: Iterator[String] = {
    ???
  }

  def toIndexSeq: IndexedSeq[String] = {
    tokensR.runLog.run
  }

  def tokensR: Process[Task, String] = csv.vectorsR.map(_.head)
}

object TokensBag {
  def create(tokens: Iterable[String]): TokensBag = {
    create(tokens.iterator)
  }

  def create(tokens: Iterator[String]): TokensBag = {
    val csv = new CsvBag()
    for (out <- resource.managed(csv.openAppender)) {
      while (tokens.hasNext) {
        val token = tokens.next
        out.appendStrings(Vector(token))
      }
    }
    new TokensBag(csv)
  }

  val empty = new TokensBag(CsvBag.empty)
}
