package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Aug. 23, 2018
 *  version Sep.  4, 2018
 * @version Dec. 29, 2018
 * @author  ASAMI, Tomoharu
 */
trait HttpPart { self: Record =>
  object http {
    object request {
      def normalize: Record = copy(fields = fields.map(_.normalizeHttp))
      def build: Either[NonEmptyVector[Record], Record] = Record.build(self)
    }
    object response {
    }
  }
}

object HttpPart {
}
