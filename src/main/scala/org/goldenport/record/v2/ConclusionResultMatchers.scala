package org.goldenport.record.v2

import org.scalatest.matchers._

/*
 * @since   Sep. 29, 2020
 * @version Sep. 29, 2020
 * @author  ASAMI, Tomoharu
 */
trait ConclusionResultMatchers {
  import ConclusionResultMatchers._

  def conclusion_object(p: AnyRef) = ObjectMatcher(p)
}

object ConclusionResultMatchers {
  case class ObjectMatcher(o: AnyRef) extends Matcher[AnyRef] {
    def apply(p: AnyRef) = p match {
      case m: SuccessConclusionResult[_] => MatchResult(m.result == o, s"${m.result} was not equal to $o", s"$p was equal to $o")
      case m: ErrorConclusionResult[_] => MatchResult(false, s"Conclusion failure: $m", s"$p was equal to $o")
    }
  }
}
