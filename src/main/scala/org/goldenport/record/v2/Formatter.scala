package org.goldenport.record.v2

/*
 * @snice   Jul. 25, 2014
 * @version Jul. 25, 2014
 * @author  ASAMI, Tomoharu
 */
trait Formatter {
  def format(column: Column, value: Option[List[Any]]): Option[String] = {
    if (is_Accept(column, value))
      Some(format_value(column, value))
    else
      None
  }

  protected def format_value(column: Column, value: Option[List[Any]]): String = {
    value match {
      case None => format_Empty
      case Some(xs) =>
        if (column.isMulti) {
          if (xs.isEmpty)
            format_Empty_Multi
          else
            format_Seq(column, xs.map(format_Value(column)))
        } else {
          if (xs.isEmpty)
            format_Empty
          else
            format_Value(column)(xs.head)
        }
    }
  }

  protected def is_Accept(column: Column, value: Option[List[Any]]): Boolean = {
    is_Accept(column.datatype)
  }

  protected def is_Accept(datatype: DataType): Boolean = ???

  protected def format_Empty: String = ""
  protected def format_Empty_Multi: String = format_Empty

  protected def format_Seq(column: Column, values: Seq[Any]): String = {
    values.map(format_Value(column)).mkString(",")
  }

  protected def format_Value(column: Column)(value: Any): String = {
    format_Value(column.datatype, value)
  }

  protected def format_Value(datatype: DataType, value: Any): String = ???
}
