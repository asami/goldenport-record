package org.goldenport.record.util

import scala.collection.mutable
import scala.collection.JavaConverters._
import com.typesafe.config._
import org.goldenport.Strings
import org.goldenport.hocon.{HoconUtils => LibHoconUtils}
import org.goldenport.record.v3.{Record, Field}

/*
 * @since   Jun.  9, 2020
 *  version Jun.  9, 2020
 *  version Jun. 29, 2022
 *  version Sep.  7, 2022
 * @version Oct. 10, 2022
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
    Record(xs)
  }

  private def _create_field(key: String, v: Any): Field = {
    def _go_(k: String, ks: List[String]): Record = {
      val a = ks match {
        case Nil => Field.create(k, v)
        case x :: Nil => Field.create(x, v)
        case x :: x2 :: xs => Field.create(x, _go_(x2, xs))
      }
      Record(Vector(a))
    }

    Strings.totokens(key, ".") match {
      case Nil => Field.create(key, v)
      case x :: Nil => Field.create(x, v)
      case x :: x2 :: xs => Field.create(x, _go_(x2, xs))
    }
  }

  def toJsonString(p: Config): String = toRecord(p).toJsonString
}
