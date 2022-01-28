package org.goldenport.record.v3

import com.asamioffice.goldenport.text.{TextBuilder, StringTextBuilder, UJavaString}
import org.goldenport.values.PathName
import org.goldenport.util.AnyUtils

/*
 * @since   Jan. 25, 2022
 * @version Jan. 28, 2022
 * @author  ASAMI, Tomoharu
 */
trait HoconPart { self: Record =>
  def toHoconString: String = HoconPart.toHoconString(this)
}

object HoconPart {
  case class PathValue(path: PathName, value: HoconValue) {
    def marshall(p: TextBuilder): Unit = {
      p.print(s"${path.v}=")
      value.marshall(p)
      p.println()
    }
  }

  sealed trait HoconValue {
    def marshall(p: TextBuilder): Unit
  }
  case class StringValue(string: String) extends HoconValue {
    def marshall(p: TextBuilder) = p.print("\"" + UJavaString.escapeJavaText(string) + "\"")
  }
  case class NumberValue(number: Number) extends HoconValue {
    def marshall(p: TextBuilder) = p.print(AnyUtils.toString(number))
  }
  case class BooleanValue(boolean: Boolean) extends HoconValue {
    def marshall(p: TextBuilder) = p.print(AnyUtils.toString(boolean))
  }
  case class RecordValue(record: Record) extends HoconValue {
    def marshall(p: TextBuilder) = {
      p.println("{")
      p.indentUp()
      for (x <- record.fields.flatMap(_to_hocon)) {
        x.marshall(p)
      }
      p.indentDown()
      p.print("}")
    }
  }
  case class ArrayValue(array: Seq[HoconValue]) extends HoconValue {
    def marshall(p: TextBuilder) = {
      p.print("[")
      for (x <- array) {
        x.marshall(p)
        print(",")
      }
      p.indentDown()
      p.print("]")
    }
  }
  case object Empty extends HoconValue {
    def marshall(p: TextBuilder) = p.print("")
  }

  def toHoconString(p: Record): String = {
    val xs = p.fields.flatMap(_to_hocon)
    new Builder().build(xs)
  }

  private def _to_hocon(p: Field): Seq[PathValue] = {
    def _pathname_ = PathName(p.name, ".")
    p.value match {
      case EmptyValue => Vector(PathValue(_pathname_, Empty))
      case SingleValue(v) => v match {
        case m: Record => _path_values(_pathname_, m)
        case m => Vector(PathValue(_pathname_, _value(m)))
      }
      case MultipleValue(vs) => Vector(PathValue(_pathname_, ArrayValue(vs.map(_value))))
    }
  }

  private def _path_values(pn: PathName, p: Record): Seq[PathValue] =
    p.fields.flatMap(x => _to_hocon(pn, x))

  private def _to_hocon(parent: PathName, p: Field): Seq[PathValue] = {
    def _pathname_ = parent + p.name
    p.value match {
      case EmptyValue => Vector(PathValue(_pathname_, Empty))
      case SingleValue(v) => Vector(PathValue(_pathname_, _value(v)))
      case MultipleValue(vs) => vs.map(x => PathValue(_pathname_, _value(x)))
    }
  }

  private def _value(p: Any): HoconValue = p match {
    case m: String => if (m.isEmpty) Empty else StringValue(m)
    case m: Number => NumberValue(m)
    case m: Boolean => BooleanValue(m)
    case m: Record => RecordValue(m)
    case m => StringValue(AnyUtils.toString(m))
  }

  class Builder() extends StringTextBuilder {
    def build(ps: Seq[PathValue]): String = {
      for (x <- ps)
        x.marshall(this)
      toString
    }
  }
}
