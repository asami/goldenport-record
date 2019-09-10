package org.goldenport.record.v2.util

import scala.util.control.NonFatal
import java.lang.reflect.{Array => _, _}
import java.util.Date
import java.sql.Timestamp
import org.goldenport.util.AnyRefUtils
import org.goldenport.record.v2._

/*
 * @since   Feb. 16, 2014
 *  version Mar.  4, 2014
 *  version May. 15, 2014
 *  version Jun. 16, 2014
 *  version Aug. 12, 2014
 *  version Sep. 27, 2014
 *  version Apr. 14, 2015
 * @version Sep.  9, 2019
 * @author  ASAMI, Tomoharu
 */
object BeanUtils {
  val javaObjectMethodNames = Vector("wait", "equals", "toString", "hashCode", "getClass", "notify", "notifyAll")

  def getProperty(o: AnyRef, name: String): Option[AnyRef] = {
    o.getClass.getMethods.find(m =>
      name == m.getName && m.getParameterTypes.length == 0
    ).map(_.invoke(o))
    // TODO attribute variables
  }

  def getProperties(o: AnyRef): Record = {
    val ps = o.getClass.getMethods.flatMap { m =>
      val name = m.getName
      if (javaObjectMethodNames.contains(name) || m.getParameterTypes.length != 0) {
        None
      } else {
        val p = m.getName -> m.invoke(o)
        Some(p)
      }
    }
    // TODO attribute variables
    Record.createApp(ps)
  }

  def setProperties(o: AnyRef, rec: Record) {
    // TODO
  }

  def findConstructor(clazz: Class[_]): Option[Constructor[_]] = {
    val cs = clazz.getConstructors
    if (cs.isEmpty) None
    else Some(cs.maxBy(_.getParameterTypes.length))
  }

  def create[T](clazz: Class[_], param: Seq[Any], columns: Seq[Column] = null): T = {
    val cs = Option(columns)
    findConstructor(clazz) map { c =>
      val types = c.getParameterTypes()
      require (param.length == types.length, s"Not same parameters. ${clazz.getSimpleName}: ${param.length} / ${types.length} / ${cs.map(_.length)}")
      val xs: Seq[AnyRef] = types.zipWithIndex.map { x =>
        _buildData(x._1, param(x._2), cs.map(_(x._2)))
      }
      val xs2 = checkTypes(types, xs, cs)
      val r = c.newInstance(xs2: _*)
      r.asInstanceOf[T]
    }
  } getOrElse {
    throw new IllegalArgumentException("No constructor")
  }

  private def _buildData(p: Class[_], a: Any, c: Option[Column]): AnyRef = {
    try {
      val r = if (p == classOf[Option[_]]) {
        if (c.isEmpty) {
          a.asInstanceOf[AnyRef]
        } else {
          a.asInstanceOf[AnyRef] // TODO
        }
      } else {
        buildData(p, a, c)
      }
      r
    } catch {
      case NonFatal(e) => 
        throw new IllegalArgumentException("Undefined data type %s : %s = %s".format(p, c, e.getMessage), e)
    }
  }

  def buildData(k: Class[_], a: Any, c: Option[Column]): AnyRef = {
    c match {
      case Some(s) => buildData(s, a)
      case None => buildData(k, a)
    }
  }

  private val _mapping = Vector[(Class[_], Any => AnyRef)](
    classOf[String] -> AnyRefUtils.toString _,
    classOf[Boolean] -> AnyRefUtils.toBoolean _,
    classOf[Byte] -> AnyRefUtils.toByte _,
    classOf[Short] -> AnyRefUtils.toShort _,
    classOf[Int] -> AnyRefUtils.toInt _,
    classOf[Long] -> AnyRefUtils.toLong _,
    classOf[Float] -> AnyRefUtils.toFloat _,
    classOf[Double] -> AnyRefUtils.toDouble _,
    classOf[BigInt] -> AnyRefUtils.toBigInt _,
    classOf[BigDecimal] -> AnyRefUtils.toBigDecimal _,
    classOf[Timestamp] -> AnyRefUtils.toTimestamp _
  )

  def buildData(c: Class[_], a: Any): AnyRef = {
    _mapping.find(_._1 == c).map(_._2(a)) getOrElse {
      throw new IllegalArgumentException(s"Invalid bean data $c/$a")
    }
  }

  def buildData(c: Column, a: Any): AnyRef = {
    def one = {
      val x = a match {
        case Some(s) => s
        case None => throw new IllegalArgumentException(s"Illgal value $a / $c")
        case Nil => throw new IllegalArgumentException(s"Illgal value $a / $c")
        case xs: Seq[_] => xs.head
        case _ => a
      }
      buildData(c.datatype, x)
    }
    def zeroone = {
      val x = a match {
        case Some(s) => Some(s)
        case None => None
        case Nil => None
        case xs: Seq[_] => Some(xs.head)
        case _ => Some(a)
      }
      x.map(buildData(c.datatype, _))
    }
    def zeromore = {
      val x = a match {
        case Some(s) => Vector(s)
        case None => Nil
        case Nil => Nil
        case xs: Seq[_] => xs
        case _ => Vector(a)
      }
      x.map(buildData(c.datatype, _))
    }
    c.multiplicity match {
      case MOne => one
      case MZeroOne => zeroone
      case MOneMore => zeromore
      case MZeroMore => zeromore
      case m: MRange => zeromore
      case m: MRanges => zeromore
    }
  }

  protected def normalize_datum_one(a: Any): Any = {
    a match {
      case Some(x) => x
      case None => None
      case Nil => Nil
      case xs: Seq[_] => xs.head
      case _ => a
    }
  }

  def buildData(dt: DataType, a: Any): AnyRef = {
    RecordUtils.reconstituteAnyRef(dt, a)
  }

  def checkTypes(
    types: Seq[Class[_]],
    objects: Seq[AnyRef],
    columns: Option[Seq[Column]]
  ): Seq[AnyRef] = {
    columns match {
      case Some(s) => checkTypes(types, objects, s)
      case None => checkTypes(types, objects)
    }
  }

  def checkTypes(types: Seq[Class[_]], objects: Seq[AnyRef]): Seq[AnyRef] = {
    assert (types.length == objects.length, s"${types.length}, ${objects.length}")
    types.zip(objects) map {
      case (t, o) => checkType(t, o)
    }
  }

  def checkTypes(
    types: Seq[Class[_]],
    objects: Seq[AnyRef],
    columns: Seq[Column]
  ): Seq[AnyRef] = {
    assert (types.length == objects.length && objects.length == columns.length, s"${types.length}, ${objects.length}, ${columns.length}")
    val ts = types.zipWithIndex
    for ((t, i) <- ts; o = objects(i); c = columns(i)) yield {
      checkType(t, o, c)
    }
  }

  def checkType(
    t: Class[_],
    o: AnyRef,
    c: Column
  ): AnyRef = {
    assert (_check_type(t, o), s"$o (${o.getClass}) is incompatible for $t, $c")
    o
  }

  def checkType(t: Class[_], o: AnyRef): AnyRef = {
    assert (_check_type(t, o), s"$o (${o.getClass}) is incompatible for $t")
    if (t == classOf[Option[_]]) 
      if (o.isInstanceOf[Option[_]]) o else Some(o)
    else
      o
  }

  private def _check_type(t: Class[_], o: AnyRef) = {
    t.isInstance(o) ||
    (classOf[Boolean] == t && o.isInstanceOf[java.lang.Boolean]) ||
    (classOf[Byte] == t && o.isInstanceOf[java.lang.Byte]) ||
    (classOf[Short] == t && o.isInstanceOf[java.lang.Short]) ||
    (classOf[Int] == t && o.isInstanceOf[java.lang.Integer]) ||
    (classOf[Long] == t && o.isInstanceOf[java.lang.Long]) ||
    (classOf[Float] == t && o.isInstanceOf[java.lang.Float]) ||
    (classOf[Double] == t && o.isInstanceOf[java.lang.Double]) ||
    (classOf[Option[_]] == t)
  }

  def containsMethod(x: AnyRef, name: String, params: Array[java.lang.Class[_]]) : Boolean = {
    try {
      x.getClass.getMethod(name, params: _*)
      true
    } catch {
      case NonFatal(_) => false
    }
  }
}
