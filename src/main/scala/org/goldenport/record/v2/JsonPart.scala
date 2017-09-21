package org.goldenport.record.v2

import org.goldenport.json.IJsonStringable
import org.goldenport.record.v2.util.RecordAux

/*
 * @since   Sep.  2, 2017
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
trait JsonPart extends IJsonStringable { self: Record =>
  def toJsonString: String = RecordAux.toJsonString(this)
  def toJsonString(buf: StringBuilder): Unit = RecordAux.buildJsonString(this, buf)
}
