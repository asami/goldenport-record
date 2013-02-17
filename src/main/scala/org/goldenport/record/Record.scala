package org.goldenport.record

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Set
import scala.collection.JavaConversions.asScalaIterator
import scala.xml.Node
import java.io.InputStream
import java.sql.ResultSet
import java.util.{TimeZone, Calendar, GregorianCalendar}
import org.json.simple.{JSONValue, JSONArray}
import com.asamioffice.goldenport.util.NameCounter
import com.asamioffice.goldenport.text.UJson
import sql.SqlU.{value2literal => to_literal}
import org.goldenport.util._
import org.goldenport.atom._
import scala.collection.mutable.LinkedHashMap

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jun.  9, 2010
 *  version Jul.  3, 2011
 *  version Nov. 29, 2011
 *  version Feb. 16, 2012
 * @version Feb. 17, 2013
 * @author  ASAMI, Tomoharu
 */
class Record(data: Traversable[(String, AnyRef)]) extends mutable.Map[String, AnyRef] {
  val fields = new ArrayBuffer[(String, AnyRef)]
  // XXX to optional methods using context
  var datastoreKey: Option[AnyRef] = None // AppEngine Key
  var datastoreId: Option[AnyRef] = None // AppEngine ID/Name
  var g3Version: Option[String] = None
  var g3SchemaUri: Option[String] = None
  var atomId: Option[String] = None
  var atomTitle: Option[String] = None
  var atomUpdated: Option[VDateTime] = None
  var atomPublished: Option[VDateTime] = None
  var atomCategories: List[String] = Nil
  var atomAuthors: List[String] = Nil
  var atomContributers: List[String] = Nil
  var atomRights: Option[String] = None
  var atomSource: Option[String] = None
  var atomSummary: Option[String] = None
  var atomContent: Option[String] = None
  var atomLinks: List[String] = Nil
  var atomExtensionElements: List[Node] = Nil
  var schema: Option[RecordSchema] = None
  require (!data.exists(_._2 == null))
  fields ++= data

  def this() = this(Nil)

  def setFrom(src: Record) {
    require (idOption == src.idOption)
    for ((key, value) <- src.fields) {
      +=((key, value))
    }
  }

  def nth(idx: Int) = {
    fields(idx)._2
  }

  def isValid: Boolean = {
    println("Record fields = " + fields)
    println("isValid = " + !fields.exists(_._2.isInstanceOf[RecordFieldException]))
    if (schema.isEmpty) true
    else !fields.exists(_._2.isInstanceOf[RecordFieldException])
  }

  def asString(key: String) = {
    get(key).get.toString
  }

  def asInt(key: String, defaultvalue: Int): Int = {
    get(key) match {
      case Some(v) => v.toString.toInt
      case None => defaultvalue
    }
  }

  // Map
  def get(key: String): Option[AnyRef] = {
    fields.find(kv => is_key_match(kv._1, key) && kv._2 != null).map(_._2)
  }

  def iterator: Iterator[(String, AnyRef)] = {
    fields.iterator
  }

  override def +=(kv: (String, AnyRef)): this.type = {
    val idx = fields.indexWhere(_._1 == kv._1)
    if (idx != -1) {
      fields(idx) = kv
    } else {
      fields += kv
    }
    this
  }

  override def -=(key: String): this.type = {
    val idx = fields.indexWhere(_._1 == key)
    if (idx != -1) {
      fields.remove(idx)
    }
    this
  }

  override def empty: Record = {
    new Record()
  }

  override def foreach[C](f: ((String, AnyRef)) => C) {
    for (kv <- fields) f(kv)
  }

  override def size = {
    fields.size
  }

  // Object
  override def toString() = toJson

/*
  override def toString() = {
    fields.map {
      case (key, value) => key.name + ":" + to_literal(value)
    }.mkString("{", ", ", "}")
  }
*/

  //
  def idField: String = {
    schema match {
      case Some(s) => s.idName
      case None => "id"
    }
  }

  def id: AnyRef = get(idField)

  def idString: String = id.toString

  def idOption: Option[AnyRef] = get(idField)

  private lazy val normalized_fields: Seq[(String, AnyRef)] = {
    schema match {
      case Some(sc) => {
        fields.map {
          case (k, v) => sc.findDataType(k) match {
            case Some(dt) => (k, dt.normalize(v).right.get)
            case None => (k, v)
          }
        }
      }
      case None => fields
    }
  }

  def fieldNames: List[String] = normalized_fields.map(kv => to_name(kv._1)).toList

  def valueLiterals: List[String] = normalized_fields.map(kv => to_literal(kv._2)).toList

  def fieldNameValueLiteralPairs = {
    for ((key, value) <- normalized_fields.toList) yield {
      (to_name(key), to_literal(value))
    }
  }

  private def to_name(n: String) = n

  def fieldNameValueLiteralPairs(includes: List[String]) = {
    for ((key, value) <- normalized_fields.toList if includes.contains(key)) yield {
      (to_name(key), to_literal(value))
    }
  }

  def fieldNameValueLiteralPairsExcludes(excludes: List[String]) = {
    for ((key, value) <- normalized_fields.toList if !excludes.contains(key)) yield {
      (to_name(key), to_literal(value))
    }
  }

  def openInputStream(): InputStream = sys.error("not supported yet")

  // sql
  def sqlFieldsClause = fieldNames.mkString("(", ", ", ")")

  def sqlValuesClause = valueLiterals.mkString("(", ", ", ")")
    
  def sqlSetClause = {
    fieldNameValueLiteralPairsExcludes(sqlWhereFieldKeys)
      .map(kv => kv._1 + "=" + kv._2).mkString(", ")
  }

  def sqlWhereClause: String = sqlWhereClause(sqlWhereFieldKeys)

  def sqlWhereClause(keys: List[String]): String = {
    (for ((key, value) <- normalized_fields if keys.contains(key)) yield {
      value match {
//        case expr: Expression => Some(expr.sqlLiteral(key))
        case cvalues: XEnumeration => {
          cvalues.values match {
            case Nil => None
            case values if values.length == 1 => Some(
              to_name(key) + "=" + to_literal(values(0))
            )
            case values => Some(
              values.map(to_name(key) + "=" + to_literal(_)).mkString("(", " OR ", ")")
            )
          }
        }
//        case cderive: CDerivation => None
        case _ => Some(to_name(key) + "=" + to_literal(value))
      }
    }).flatten.mkString(" AND ") 
  }

  def sqlWhereExampleClause = sqlWhereClause

  def sqlWhereFieldKeys = {
    def is_include(kv: (String, AnyRef)): Boolean = {
      val (key, value) = kv
      if (is_key_match(key, idField)) return true
//      if (value.isInstanceOf[Expression]) return true
      return false
    }

    normalized_fields.filter(is_include).map(_._1).toList
  }

  private def is_key_match(lhs: String, rhs: String) = {
    lhs.toLowerCase == rhs.toLowerCase
  }

  //
  private val id_symbol = "id"
  private val title_symbol = "title"
  private val updated_symbols = Set("updated", "update", "updates")
  private val published_symbols = Set("published", "created", "create", "creates")

  //
  private def id_value = {
    field_string(id_symbol)
  }

  private def title_value = {
    field_string(title_symbol)
  }

  private def updated_value = {
    field_vdatetime_symbols_option(updated_symbols)
  }

  private def published_value = {
    field_vdatetime_symbols_option(published_symbols)
  }

  // Atom
  private def atom_id = {
    new AtomId(atomId getOrElse id_value)
  }

  private def atom_title = {
    new AtomTitle(atomTitle getOrElse title_value)
  }

  private def atom_updated = {
    atomUpdated orElse updated_value match {
      case Some(dt) => new AtomUpdated(dt)
      case None => new AtomUpdated() // XXX
    }
  }

  private def atom_published = {
    (atomPublished orElse published_value).map(new AtomPublished(_))
  }

  private def atom_subtitle = None

  private def atom_categories = atomCategories.map(AtomCategory)

  private def atom_authors = atomAuthors.map(AtomAuthor)

  private def atom_contributers = atomContributers.map(AtomContributer)

  private def atom_rights = atomRights.map(AtomRights)

  private def atom_source = atomSource.map(AtomSource)

  private def atom_summary = atomSummary.map(AtomSummary)

  private def atom_content = new InlineOtherAtomContent(toJson, "application/json") // XXX content field, atomContent 

  private def atom_icon = None

  private def atom_logo = None

  private def atom_links = Nil

  private def atom_generator = None

  private def atom_extensionElements = Nil // XXX

  private def atom_entries = Nil

  def toAtomFeed: AtomFeed = {
    new AtomFeed() {
      atomId = atom_id
      atomTitle = atom_title
      atomUpdated = atom_updated
      atomSubtitle = atom_subtitle
      atomCategories ++= atom_categories
      atomAuthors ++= atom_authors
      atomContributers ++= atom_contributers
      atomRights = atom_rights
      atomIcon = atom_icon
      atomLogo = atom_logo
      atomLinks ++= atom_links
      atomGenerator = atom_generator
      atomExtensionElements ++= atom_extensionElements
      atomEntries ++= atom_entries
    }
  }

  def toAtomEntry: AtomEntry = {
    new AtomEntry() {
      atomId = atom_id
      atomTitle = atom_title
      atomUpdated = atom_updated
      atomPublished = atom_published
      atomCategories ++= atom_categories
      atomAuthors ++= atom_authors
      atomContributers ++= atom_contributers
      atomRights = atom_rights
      atomSource = atom_source
      atomSummary = atom_summary
      atomContent = atom_content
      atomLinks ++= atom_links
      atomExtensionElements ++= atom_extensionElements
    }
  }

  private def atom_string(value: AnyRef) = {
    value match {
      case v: java.util.Date => VDateTime(v).toString
      case _ => value.toString
    }
  }

  private def field_string(atom: String, value: String = ""): String = {
    fields.find(_._1 == atom) match {
      case Some((_, v)) => atom_string(v)
      case None => value
    }
  }

  private def field_string_symbols(atoms: Set[String], value: String = ""): String = {
    fields.find(kv => atoms.contains(kv._1)) match {
      case Some((_, v)) => atom_string(v)
      case None => value
    }
  }

  private def field_string_symbols_option(atoms: Set[String]): Option[String] = {
    fields.find(kv => atoms.contains(kv._1)) match {
      case Some((_, v)) => Some(atom_string(v))
      case None => None
    }
  }

  private def field_vdatetime_symbols_option(atoms: Set[String]): Option[VDateTime] = {
    fields.find(kv => atoms.contains(kv._1)) match {
      case Some((_, v: VDateTime)) => Some(v)
      case Some((_, v: java.util.Date)) => Some(VDateTime(v))
      case None => None
    }
  }

  def toJson: String = UJson.seq2Json(fields)

  def toCsv: String = {
    fields.map {
      case (key, value) => csv_string(value)
    }.mkString(",")
  }

  def toMap: Map[String, AnyRef] = {
    Map(toArray.map(kv => (kv._1, kv._2)): _*)
  } 

  private def csv_string(value: Any) = {
    value.toString
  }

  def referenceIds(refName: String, ctx: RecordContext): Option[(String, List[Any])] = { 
    def make_list(s: String) = {
      JSONValue.parse(s) match {
        case a: JSONArray => a.iterator.map(Record.normalizeId).toList
        case j => sys.error("json = " + j + "/" + j.getClass)
      }
    }

    schema match {
      case Some(s) => s.fields.find(f => is_key_match(f.name, refName)) match { 
        case Some(f) => f.datatype match {
          case xref: XEntityReference => {
            fields.find(c => is_key_match(f.name, c._1)) match {
              case Some(c) => f.normalize(c._2, ctx) match {
                case Right(r) => Some((xref.uri, List(r)))
                case Left(l) => None
              }
              case None => None
            }
          }
          case _ => {
            fields.find(c => is_key_match(f.name, c._1)) match {
              case Some(c) => f.normalize(c._2, ctx) match {
                case Right(r) => Some((f.name, List(r)))
                case Left(l) => None
              }
              case None => None
            }
          }
        }
        case None => None
      }
      case None => fields.find(f => is_key_match(f._1, refName)) match {
        case Some(f) => f._2 match {
          case l: Seq[_] => Some((refName, l.map(_.toString).toList))
          case s: String if s.startsWith("[") => Some(refName, make_list(s))
          case s: Any => Some(refName, List(s.toString))
        }
        case None => None
      }
    }
  }
}

object Record {
  def apply(data: Tuple2[AnyRef, Any]*): Record = {
    apply(data)
  }

  def apply(data: Traversable[(AnyRef, Any)]): Record = {
    def tokey(s: AnyRef) = s match {
      case v: String => v
      case s => s.toString
    }

    require (!data.exists(_._2 == null))
    
    new Record(_flat_to_multi(data.map(kv => (tokey(kv._1), kv._2.asInstanceOf[AnyRef]))))
  }

  private def _flat_to_multi(kvs: Traversable[(String, AnyRef)]): Traversable[(String, AnyRef)] = {
    val map = new LinkedHashMap[String, ArrayBuffer[AnyRef]]
    for (kv <- kvs) {
      var c = map.get(kv._1)
      if (c.isDefined) {
        val buf = c.get
        buf += kv._2
      } else {
        val buf = new ArrayBuffer[AnyRef]
        buf += kv._2
        map.put(kv._1, buf)
      }
    }
    map.flatMap {
      case (k, v) => {
        v.size match {
          case 0 => None
          case 1 => Some((k, v.head))
          case _ => Some((k, v.toList))
        }
      }
    }
  }

  def apply(data: String): Record = {
    if (data == null || data == "") {
      new Record()
    } else {
      if (data.startsWith("?")) {
        apply(data.substring(1))
      } else {
        val kvs: Traversable[(AnyRef, Any)] = data.split("&").flatMap(kv =>
          kv.indexOf("=") match {
            case -1 => None
            case index => {
              val key = kv.substring(0, index)
              val value = kv.substring(index + 1)
              Some((key, value))
            }
          }
        )
        apply(kvs)
      }
    }
  }

  def fromOptions(data: Tuple2[Symbol, Option[Any]]*): Record = {
    fromOptions(data)
  }

  def fromOptions(data: Traversable[(Symbol, Option[Any])]): Record = {
    apply(data.collect {
      case (k, Some(v)) => (k, v)
    })
  }

  def fromNullables(data: Tuple2[Symbol, Any]*): Record = {
    fromNullables(data)
  }

  def fromNullables(data: Traversable[(Symbol, Any)]): Record = {
    apply(data.collect {
      case (k, v) if v != null => (k, v)
    })
  }

/*
  def apply(data: Product) = {
    val seq = new ArrayBuffer[(Symbol, AnyRef)]
    data match {
      case Tuple2(key: Symbol, value: Any) => {
        seq += ((key, value.asInstanceOf[AnyRef]))
      }
      case t: Traversable[(Symbol, Any)] => for ((key, value) <- t) {
        seq += ((key, value.asInstanceOf[AnyRef]))
      }
      case p: Product => for (elem <- p.productIterator) {
        val pair = elem.asInstanceOf[(Any, Any)]
        val key = pair._1 match {
          case sym: Symbol => sym
          case v => Symbol(v.toString)
        }
        seq += ((key, pair._2.asInstanceOf[AnyRef]))
      }
    }
    new Record(seq)
  }
*/
  def normalizeId(value: Any): Any = {
    try {
      value.toString.toLong
    } catch {
      case e: NumberFormatException => value.toString
    }
  }
}

object ResultSetRecord {
  def apply(rs: ResultSet)(implicit ctx: RecordContext) = {
    val record = new Record()
    val metadata = rs.getMetaData
    val length = metadata.getColumnCount
    val counter = new NameCounter
    for (i <- 1 to length) {
      val columnname = {
        val name = metadata.getColumnName(i)
        val cnt = counter.getCount(name)
        counter.add(name)
        if (cnt == 0) name
        else name + "_" + cnt
      }
      val columntype = metadata.getColumnType(i)
//println("ResultSetRecord: " + columntype + "," + columnname + " = " + ctx.getSqlData(rs, i, columntype))
      record += columnname -> ctx.getSqlData(rs, i, columntype)
    }
    record
  }
}
