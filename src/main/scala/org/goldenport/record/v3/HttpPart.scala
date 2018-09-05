package org.goldenport.record.v3

import org.goldenport.exception.RAISE

/*
 * @since   Aug. 23, 2018
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
trait HttpPart { self: Record =>
  object http {
    object request {
      def normalize: Record = copy(fields = fields.map(_.normalizeHttp))
    }
    object response {
    }
  }
}

object HttpRequestPart {
}
