package org.goldenport.record.v3

import org.goldenport.context.Consequence
import org.goldenport.context.ConsequenceProperties

/*
 * @since   Oct.  6, 2021
 * @version Jan. 30, 2022
 * @author  ASAMI, Tomoharu
 */
trait ConsequencePart extends ConsequenceProperties { self: Record =>
  protected def get_Value(key: String) = get(key)
}
