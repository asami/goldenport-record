package org.goldenport.record.util

import scala.collection.mutable
import scala.collection.JavaConverters._
import com.typesafe.config._
import org.goldenport.hocon.{HoconUtils => LibHoconUtils}
import org.goldenport.record.v3.{Record, Field}

/*
 * @since   Jun.  9, 2020
 * @version Jun.  9, 2020
 * @author  ASAMI, Tomoharu
 */
object HoconUtils {
  def getRecord(config: Config, key: String): Option[Record] =
    LibHoconUtils.getConfig(config, key).map(toRecord)

  def toRecord(p: Config): Record = {
    val a: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = p.entrySet.asScala
    val xs: Seq[Field] = a.toVector.map(x => x.getValue.unwrapped match {
        case m: Config => Field.create(x.getKey, toRecord(m))
        case m => Field.create(x.getKey, m)
    })
    Record(xs)
  }
}
