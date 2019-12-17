package org.goldenport.record.v3

import scala.util.control.NonFatal
import org.goldenport.record.v2
import org.goldenport.record.v2.{ValidationResult, Valid, Warning, Invalid}
import org.goldenport.record.v2.{DataTypeFailure}

/*
 * @since   Jun. 20, 2015
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
object `package` {
  // type Schema = org.goldenport.record.v2.Schema
  // type Column = org.goldenport.record.v2.Column

  implicit class V2DataTypeWrapper(val datatype: v2.DataType) extends AnyVal {
    def toInstanceV3(v: Any): Any = {
      datatype match {
        case v2.XDateTime => v2.XDateTime.toInstance(v)
        case _ => datatype.toInstance(v)
      }
    }

    def toInstanceV3V(v: Any): (ValidationResult, Any) = {
      try {
        datatype.validate(v) match {
          case Valid => (Valid, toInstanceV3(v))
          case w: Warning => (w, toInstanceV3(v))
          case e: Invalid => (e, v)
        }
      } catch {
        case NonFatal(e) => (DataTypeFailure(datatype, Vector(v.toString)), v)
      }
    }
  }

  sealed trait RecordFailure {
  }

  sealed trait ValidationFailure extends RecordFailure {

  }

  sealed trait ValidationOutcome
}

