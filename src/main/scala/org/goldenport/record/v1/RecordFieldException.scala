package org.goldenport.record.v1

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jan.  9, 2011
 *  version Jun. 27, 2011
 *  version Feb. 14, 2012
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
abstract class RecordFieldException(cause: Exception = null) extends RuntimeException {
  if (cause != null) {
    initCause(cause)
  }

  def toMessage(ctx: String): String
}

class MissingFieldRecordFieldException extends RecordFieldException() {
  def toMessage(ctx: String) = {
    "Missing field" // XXX
  }
}

class IllegalFormatRecordFieldException(cause: Exception) extends RecordFieldException(cause) {
  def toMessage(ctx: String) = {
    "Illegal format: " + cause.getMessage // XXX
  }
}

class InvalidValueRecordFieldException(message: String) extends RecordFieldException() {
  def toMessage(ctx: String) = {
    "Invalid value: " + message // XXX
  }
}

class MultiplicityRecordFieldException(message: String) extends RecordFieldException() {
  def toMessage(ctx: String) = {
    "Invalid value: " + message // XXX
  }
}
