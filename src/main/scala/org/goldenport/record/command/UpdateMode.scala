package org.goldenport.record.command

import org.goldenport.value._

/*
 * @since   Mar. 27, 2020
 * @version Mar. 28, 2020
 * @author  ASAMI, Tomoharu
 */
sealed trait UpdateMode extends NamedValueInstance {
  def emptyValueMode: UpdateMode.EmptyMode
  def emptyStringMode: UpdateMode.EmptyMode
}

case object NeutralMode extends UpdateMode {
  val name = "neutral"
  val emptyValueMode = UpdateMode.VoidEmptyMode
  val emptyStringMode = UpdateMode.StringEmptyMode
}

case object FormMode extends UpdateMode {
  val name = "form"
  val emptyValueMode = UpdateMode.VoidEmptyMode
  val emptyStringMode = UpdateMode.VoidEmptyMode
}

case object NullifyMode extends UpdateMode {
  val name = "nullify"
  val emptyValueMode = UpdateMode.NullEmptyMode
  val emptyStringMode = UpdateMode.NullEmptyMode
}

object UpdateMode extends EnumerationClass[UpdateMode] {
  val elements = Vector(NeutralMode, FormMode, NullifyMode)

  sealed trait EmptyMode extends NamedValueInstance {
  }

  case object StringEmptyMode extends EmptyMode {
    val name = "string"
  }

  case object VoidEmptyMode extends EmptyMode {
    val name = "void"
  }

  case object NullEmptyMode extends EmptyMode {
    val name = "null"
  }

  object EmptyMode extends EnumerationClass[EmptyMode] {
    val elements = Vector(StringEmptyMode, VoidEmptyMode, NullEmptyMode)
  }
}
