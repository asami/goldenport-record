package org.goldenport.record.util

import scala.collection.mutable
import scala.collection.JavaConverters._
import com.typesafe.config._
import org.goldenport.Strings
import org.goldenport.collection.VectorMap
import org.goldenport.hocon.{HoconUtils => LibHoconUtils}
import org.goldenport.record.v3.{Record, Field}
import org.goldenport.record.v3.{FieldValue, SingleValue, MultipleValue, EmptyValue}

/*
 * @since   Jun.  9, 2020
 *  version Jun.  9, 2020
 *  version Jun. 29, 2022
 *  version Sep.  7, 2022
 *  version Oct. 10, 2022
 * @version Dec. 16, 2022
 * @author  ASAMI, Tomoharu
 */
object HoconUtils {
  def getRecord(config: Config, key: String): Option[Record] =
    LibHoconUtils.getConfig(config, key).map(toRecord)

  def toRecord(p: Config): Record = {
    val a: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = p.entrySet.asScala
    val xs: Seq[Field] = a.toVector.map(x => x.getValue.unwrapped match {
        case m: Config => _create_field(x.getKey, toRecord(m)) // Field.create(x.getKey, toRecord(m))
        case m => _create_field(x.getKey, m) // Field.create(x.getKey, m)
    })
    val b = _unify(xs)
    Record(b)
  }

  private def _create_field(key: String, v: Any): Field = {
    def _go_(k: String, ks: List[String]): Record = {
      val a = ks match {
        case Nil => Field.create(k, v)
        case x :: xs => Field.create(k, _go_(x, xs))
      }
      Record(Vector(a))
    }

    Strings.totokens(key, ".") match {
      case Nil => Field.create(key, v)
      case x :: Nil => Field.create(x, v)
      case x :: x2 :: xs => Field.create(x, _go_(x2, xs))
    }
  }

  private def _unify(ps: Seq[Field]): Vector[Field] = {
    import scalaz._, Scalaz._

    case class Z(m: VectorMap[Symbol, Vector[FieldValue]] = VectorMap.empty) {
      def r = {
        // println(s"_unify Z: ${m}")
        val r = m.toVector.map {
          case (k, vs) => Field(k, _concat(vs))
        }
        // println(s"_unify Zr: ${m} => $r")
        r
      }

      def +(rhs: Field) = copy(m = m |+| VectorMap(rhs.key -> Vector(rhs.value)))
    }
    ps./:(Z())(_+_).r
  }

  private def _concat(ps: Vector[FieldValue]): FieldValue = {
    import scalaz._, Scalaz._

    // println(s"_concat: ${ps}")
    case class Z(
      xs: Map[Symbol, Vector[FieldValue]] = Map.empty,
      vs: Vector[FieldValue] = Vector.empty
    ) {
      def r: FieldValue = {
        // println(s"_concat Z: ${xs}")
        val a = xs.toVector.foldMap {
          case (k, vs) => Record(Vector(Field(k, _value(vs))))
        }
        // println(s"_concat Z => ${a.show}")
        val b = if (a.isEmpty) vs else vs :+ SingleValue(a)
        b.toList match {
          case Nil => EmptyValue
          case x :: Nil => x
          case xs => MultipleValue(xs.flatMap(_.asList))
        }
      }

      private def _value(vs: Vector[FieldValue]) = vs.toList match {
        case Nil => EmptyValue
        case x :: Nil => x
        case xs => _concat(xs.toVector)
      }

      def +(rhs: FieldValue) = {
        rhs match {
          case SingleValue(v) => v match {
            case m: Record =>
//              for (f <- m.fields)
                // println(s"_concat +: ${f.key} -> ${f.value}")
              val a = m.fields.toVector.foldMap(f => Map(f.key -> Vector(f.value)))
              // println(s"_concat + => ${a}")
              copy(xs = xs |+| a)
            case _ => copy(vs = vs :+ rhs)
          }
          case MultipleValue(vs) => copy(vs = this.vs ++ vs.toVector.map(SingleValue(_)))
          case EmptyValue => this
        }
      }
    }
    ps./:(Z())(_+_).r
  }

  def toJsonString(p: Config): String = toRecord(p).toJsonString
}
