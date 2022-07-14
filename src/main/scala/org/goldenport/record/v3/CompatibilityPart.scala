package org.goldenport.record.v3

import org.goldenport.RAISE
import org.goldenport.record.v2.{Record => Record2, Field => Field2}
import org.goldenport.record.v2.InputFile

/*
 * @since   Aug. 23, 2018
 *  version May. 10, 2021
 *  version Nov.  1, 2021
 * @version Jul.  8, 2022
 * @author  ASAMI, Tomoharu
 */
trait CompatibilityPart { self: Record =>
  def toRecord2: Record2 = extra.v2.
    map(_.copy(fields.map(_.toField2).toList)).
    getOrElse(_to_record2)

  private def _to_record2 = {
    case class Z(
      fs: Vector[Field2] = Vector.empty,
      is: Vector[InputFile] = Vector.empty
    ) {
      def r = Record2(fs.toList, inputFiles = is.toList)

      def +(rhs: Field) = rhs.value match {
        case SingleValue(v) => v match {
          case m: InputFile => copy(is = is :+ m) // .withKey('file)) : Not image record
          case m: Record =>
            val rec = _record2(m)
            val f = rhs.withValue(SingleValue(rec))
            copy(fs = fs :+ f.toField2)
          case _ => copy(fs = fs :+ rhs.toField2)
        }
        case MultipleValue(vs) =>
          val xs = vs.flatMap {
            case m: Record => Some(_record2(m))
            case m: InputFile => None
            case m => Some(m)
          }
          val xis = vs.collect {
            case m: InputFile => m // .withKey('file) : Not image record
          }
          val f = rhs.withValue(MultipleValue(xs))
          copy(fs = fs :+ f.toField2, is = is ++ xis)
        case EmptyValue => this
      }

      // for Image record
      private def _record2(p: Record): Record2 = {
        // println(s"_record2: $p")
        case class ZZ(
          fs2: Vector[Field2] = Vector.empty,
          is2: Vector[InputFile] = Vector.empty
        ) {
          def r = Record2(fs2.toList, inputFiles = is2.toList)

          def +(rhs: Field): ZZ = {
            // println(s"_record2: $rhs")
            rhs.value match {
              case SingleValue(v) => v match {
                case m: Record =>
                  val f = rhs.withValue(SingleValue(_record2(m)))
                  copy(fs2 = fs2 :+ f.toField2)
                case m: InputFile => copy(is2 = is2 :+ m.withKey('file))
                case _ => copy(fs2 = fs2 :+ rhs.toField2)
              }
              case MultipleValue(vs) =>
                val xs = vs.flatMap {
                  case m: Record => Some(_record2(m))
                  case m: InputFile => None
                  case m => Some(m)
                }
                val is = vs.collect {
                  case m: InputFile => m.withKey('file)
                }
                // println(s"_record2 xs: $xs")
                // println(s"_record2 is: $is")
                val f = rhs.withValue(MultipleValue(xs))
                copy(fs2 = fs2 :+ f.toField2, is2 = is2 ++ is)
              case EmptyValue => this
            }
          }
        }
        p.fields./:(ZZ())(_+_).r
      }
    }
    fields./:(Z())(_+_).r
  }

  // Joda
}

object CompatibilityPart {
}
