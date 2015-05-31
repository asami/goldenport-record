package org.goldenport.record.v1.query

import java.net.URI
import scala.collection.mutable.HashMap
import org.goldenport.util.Control.orP
import org.goldenport.record.v1.sql.SqlU.{symbol2name => to_name, value2literal => to_literal}
import org.goldenport.record.v1._

/**
 * @since   Aug. 29, 2010
 *  version Jun. 26, 2011
 *  version Feb. 16, 2012
 *  version Feb. 17, 2013
 *  version Feb.  6, 2014
 * @version May. 31, 2015
 * @author  ASAMI, Tomoharu
 */
class RecordQuery(val patterns: List[RecordQueryPattern], val slots: List[RecordQuerySlot], val context: RecordQueryContext) {
  def uris = List(base_table)

  private lazy val base_table = {
    patterns.find(_.isInstanceOf[IdPattern]) match {
      case Some(id: IdPattern) => id.uri
      case Some(_) => ???
      case None => orP(slots) {
        case a: AllFieldSlot => a.uri
        case f: FieldSlot => f.uri
      }.getOrElse("notfound")
    }
  }

  private def base_table_name = base_table

  def normalize(ctx: RecordQueryContext): RecordQuery = {
    val evaledpatterns = slots.flatMap {
      case s: RecordQueryVirtualSlot => s.normalizePattern(ctx)
      case _ => Nil
    }
    val allpatterns = patterns ::: evaledpatterns
    new RecordQuery(allpatterns.flatMap(_.normalize(ctx)), slots.flatMap(_.normalize(ctx)).toList, ctx)
  }

  def whereClause: String = {
    patterns.flatMap(_.sqlWhereClauses).mkString(" OR ")
  }

  def sqlStatement: String = {
    def select_column = {
      val alluris: List[String] = {
        slots flatMap {
          case fi: ForwardIncludeSlot => Nil
          case bi: BackwardIncludeSlot => Nil
          case f: FieldSlot => Nil
          case a: AllFieldSlot => List(a.uri.toString)
        }
      }
      val activeSlots = slots flatMap {
        case fi: ForwardIncludeSlot => List(fi)
        case bi: BackwardIncludeSlot => List(bi)
        case f: FieldSlot => {
          if (alluris.contains(f.uri)) Nil
          else List(f)
        }
        case a: AllFieldSlot => Nil
      }
      val names = alluris.map(_ + ".*").toList ::: activeSlots.flatMap(_.sqlColumnNames(context)).toList
      names.mkString(", ")
    }

    def select_from = {
      base_table_name + join_clause
    }

    def join_clause = {
      lazy val join_derivations = {
//        slots.filter(_.isInstanceOf[ForwardIncludeSlot]).
//          map(_.asInstanceOf[ForwardIncludeSlot])
        slots.collect {
          case fi: ForwardIncludeSlot => fi
          case bi: BackwardIncludeSlot => bi
        }
      }

      if (join_derivations.isEmpty) ""
      else {
//        join_derivations.map(d => {
        slots.collect {
          case fi: ForwardIncludeSlot => {
            " INNER JOIN " + _to_sql_table_name(fi.joinUri) + " ON " +
            fi.sqlBaseRefColumnName + "=" + fi.sqlJoinKeyColumnName
          }
          case bi: BackwardIncludeSlot => {
            " INNER JOIN " + _to_sql_table_name(bi.joinUri) + " ON " +
            bi.sqlBaseKeyColumnName + "=" + bi.sqlJoinRefColumnName
          }
        }.mkString
      }
    }

    if (patterns.isEmpty) {
      "SELECT %s FROM %s".format(
        select_column, select_from)
    } else {
      "SELECT %s FROM %s WHERE %s".format(
        select_column, select_from, whereClause)
    }
  }

  private def _to_sql_table_name(uri: URI) = uri.toString
}

object RecordQuery {
/*
  def apply(slots: Seq[RecordQuerySlot]) = {
    new RecordQuery(slots: _*)(EmptyRecordClassSpace)
  }
*/

  def apply(patterns: List[RecordQueryPattern], slots: List[RecordQuerySlot]) = {
    new RecordQuery(patterns, slots, new RootContext)
  }

  def apply(patterns: List[RecordQueryPattern], slots: List[RecordQuerySlot], context: RecordQueryContext) = {
    new RecordQuery(patterns, slots, context)
  }

  def apply(pattern: RecordQueryPattern)(slots: RecordQuerySlot*) = {
    new RecordQuery(List(pattern), slots.toList, new RootContext)
  }

  def apply(slots: RecordQuerySlot*) = {
    new RecordQuery(Nil, slots.toList, new RootContext)
  }
}

abstract class RecordQuerySlot {
  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot]
}

// slotAtom: Symbol => slotName: String
abstract class RecordQueryRealSlot(val slotName: String) extends RecordQuerySlot {
  def columnNames(rqctx: RecordQueryContext): List[String] = {
    List(slotName)
  }

  def sqlColumnNames(rqctx: RecordQueryContext): List[String]
}

abstract class RecordQueryVirtualSlot extends RecordQuerySlot {
  def normalizePattern(rqctx: RecordQueryContext): List[RecordQueryPattern]
}

abstract class RecordQueryPattern {
  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern]
  def sqlWhereClauses: List[String]
}

class IdPattern(val uri: URI, val atom: String, val ids: List[Any]) extends RecordQueryPattern {
  require (isIdValueList(ids))

  def copy(uri: URI = uri, atom: String = atom, ids: List[Any] = ids) = {
    new IdPattern(uri, atom, ids)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQueryPattern] = {
    List(copy())
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = List(sqlColumnName)
  lazy val sqlColumnName = _sql_table_name(uri) + "." + atom

  def sqlWhereClauses = ids.map(sqlColumnName + "=" + to_literal(_))

  private def _sql_table_name(uri: URI) = uri.toString
}

class ForwardIncludeSlot(slotatom: String
    )(val baseUri: URI, val baseRef: String, val joinUri: URI, val joinKey: String, val joinAtoms: List[String]
    ) extends RecordQueryRealSlot(slotatom) {
  def copy(baseUri: URI = baseUri, baseRef: String = baseRef,
           joinUri: URI = joinUri, joinKey: String = joinKey,
           joinAtoms: List[String] = joinAtoms) = {
    new ForwardIncludeSlot(slotName)(baseUri, baseRef, joinUri, joinKey, joinAtoms)  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  override def columnNames(rqctx: RecordQueryContext) = {
    if (joinAtoms.length == 1) List(slotName)
    else joinAtoms.map(a => slotName + "_" + a)
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = {
    joinAtoms.map(_sql_table_name(joinUri) + "." + _)
  }

  def sqlBaseRefColumnName = _sql_table_name(baseUri) + "." + baseRef
  def sqlJoinKeyColumnName = _sql_table_name(joinUri) + "." + joinKey

  private def _sql_table_name(uri: URI) = uri.toString
}

class BackwardIncludeSlot(slotatom: String)
                         (val baseUri: URI, val baseKey: String,
                          val joinUri: URI, val joinRef: String, val joinAtoms: List[String]) extends RecordQueryRealSlot(slotatom) {
  def copy(baseUri: URI = baseUri, baseKey: String = baseKey,
           joinUri: URI = joinUri, joinRef: String = joinRef,
           joinAtoms: List[String] = joinAtoms) = {
    new BackwardIncludeSlot(slotName)(baseUri, baseKey, joinUri, joinRef, joinAtoms)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  override def columnNames(rqctx: RecordQueryContext) = {
    if (joinAtoms.length == 1) List(slotName)
    else joinAtoms.map(a => slotName + "_" + a)
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = {
    joinAtoms.map(_sql_table_name(joinUri) + "." + _)
  }

  def sqlBaseKeyColumnName = _sql_table_name(baseUri) + "." + baseKey
  def sqlJoinRefColumnName = _sql_table_name(joinUri) + "." + joinRef

  private def _sql_table_name(uri: URI) = uri.toString
}

class FieldSlot(slotatom: String)(val uri: URI, val atom: String) extends RecordQueryRealSlot(slotatom) {
  def copy(uri: URI = uri, atom: String = atom) = {
    new FieldSlot(slotatom)(uri, atom)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = List(_sql_table_name(uri) + "." + atom)

  private def _sql_table_name(uri: URI) = uri.toString
}

class AllFieldSlot(val uri: URI) extends RecordQueryRealSlot("*") {
  def copy(uri: URI = uri) = {
    new AllFieldSlot(uri)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = List(_sql_table_name(uri) + ".*")

  private def _sql_table_name(uri: URI) = uri.toString
}

object IdPattern {
  def apply(uri: URI, atom: String, ids: List[Any]) = new IdPattern(uri, atom, ids)

  def apply(uri: URI, atom: String, id: Any) = new IdPattern(uri, atom, List(id))
}

object ForwardIncludeSlot {
  def apply(baseUri: URI, baseRef: String,
            joinUri: URI, joinKey: String, joinName: String) = {
    new ForwardIncludeSlot(baseRef)(baseUri, baseRef, joinUri, joinKey, List(joinName))
  }
/*
  def apply(atom: String)(uri: URI, cderive: CDerivation) = {
    cderive.expression match {
      case er: EntityReferenceAttribute => {
        val baseuri = uri
        val baseref = er.refAttribute
        val joinuri = er.entity
        val joinkey = er.keyAttribute
        val joinname = er.valueAttribute
        new ForwardIncludeSlot(atom)(baseuri, baseref, joinuri, joinkey, List(joinname))
      }
    }
  }
*/
}

object BackwardIncludeSlot {
  def apply(baseUri: URI, baseKey: String,
            joinUri: URI, joinRef: String, joinAtom: String) = {
    new BackwardIncludeSlot(_last_name(joinUri))(baseUri, baseKey, joinUri, joinRef, List(joinAtom))
  }

  private def _last_name(uri: URI) = uri.toString // XXX: joinUri.last
}

object FieldSlot {
  def apply(uri: URI, atom: String) = new FieldSlot(atom)(uri, atom)
}

object AllFieldSlot {
  def apply(uri: URI) = new AllFieldSlot(uri)
}

// XXX
// FLWOR = For Let Where OrderBy Return
object Entity {
  def apply(uri: URI)(pattern: RecordQueryFieldPattern)(fields: RecordQueryField*) = {
    new Entity(uri)(pattern)(fields.toList)
  }
}

object EntityAll {
  def apply(uri: URI, id: Any) = new EntityAll(uri, "id", List(id))
  def apply(uri: URI, atom: String, id: Any) = new EntityAll(uri, atom, List(id))
}

object Id {
  def apply(atom: String, values: List[Any]) = new Id(atom, values)

  def apply(atom: String, value: Any) = new Id(atom, value)

  def apply(values: List[Any]) = new Id(values)

  def apply(value: Any) = new Id(value)
}

object Field {
  def apply(atom: String) = new Field(atom)
}

object AllFields extends AllFields {
  def apply() = new AllFields
}

object IncludeEntity {
  def apply(uri: URI, refAttr: String)(fields: RecordQueryField*) = {
    new IncludeEntity(uri)(new ForwardRef(refAttr))(fields.toList)
  }

  def apply(uri: URI, pattern: RecordQueryFieldPattern)(fields: RecordQueryField*) = {
    new IncludeEntity(uri)(pattern)(fields.toList)
  }
}

object ForwardRef {
  def apply(atom: String) = new ForwardRef(atom)
}

object BackRef {
  def apply(atom: String) = new BackwardRef(atom)
}

/*
 * Virtual slot.
 * Before query execution, virtual slots are normalized to real slots.
 */
class Entity(val uri: URI)(val pattern: RecordQueryFieldPattern)(val fields: List[RecordQueryField]) extends RecordQueryVirtualSlot {
  def copy(uri: URI = uri)(pattern: RecordQueryFieldPattern = pattern)(fields: List[RecordQueryField] = fields) = {
    new Entity(uri)(pattern)(fields)
  }

  def normalizePattern(ctx: RecordQueryContext): List[RecordQueryPattern] = {
    val c = new EntityContext(this, ctx)
    pattern.normalize(c) ::: fields.flatMap(_.normalizePattern(c))
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    val c = new EntityContext(this, ctx)
    fields.flatMap(_.normalize(c))
  }

/*
  def sqlColumnNames(rqctx: RecordQueryContext) = {
    val ctx = new EntityContext(this, rqctx)
    fields.flatMap(_.sqlColumnNames(ctx)).toList
  }
*/
}

abstract class RecordQueryContext(val parent: Option[RecordQueryContext] = None, val space: RecordClassSpace = EmptyRecordClassSpace) {
  private val variable_values = new HashMap[String, Any]

  def variable(atom: String) = variable_values(atom)

  def getVariable(atom: String): Option[Any] = variable_values.get(atom)

  def setVariable(atom: String, value: Any) {
    variable_values += atom -> value
  }

  def ids = variable("id").asInstanceOf[List[Any]]

  def entityUri: Option[URI]

  def setIds(ids: List[Any]) {
    require (isIdValueList(ids))
    variable_values += "id" -> ids
  }
}

class RootContext extends RecordQueryContext {
  def entityUri = None
}

class EntityContext(val entity: Entity, parent: RecordQueryContext) extends RecordQueryContext(Some(parent)) {
  def entityUri = Some(entity.uri)

  override def variable(atom: String): Any = {
    getVariable(atom) match {
      case Some(value) => value
      case None => parent.variable(atom)
    }
  }
}

//
abstract class RecordQueryFieldPattern {
  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern]
//  def sqlWhereClauses(rqctx: RecordQueryContext): List[String]
}

class Id(val atom: String, val ids: List[Any]) extends RecordQueryFieldPattern {
  def this(atom: String, id: Any) = this(atom, List(id))
  def this(ids: List[Any]) = this("id", ids)
  def this(id: Any) = this(List(id))

  require (isIdValueList(ids))

  private def sql_column_name = atom
  private def sql_column_name(uri: URI) = uri.toString + "." + atom

  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern] = {
    List(new IdPattern(rqctx.entityUri.get, atom, ids.flatMap {
      case v: Var => rqctx.variable(v.atom).asInstanceOf[List[Any]]
      case v => List(v)
    }))
  }
/*  
  def sqlWhereClauses(rqctx: RecordQueryContext) = rqctx.entityUri match {
    case Some(uri) => ids.map(sql_column_name(uri) + "=" + to_literal(_))
    case None => ids.map(sql_column_name + "=" + to_literal(_))
  }
*/
}

class ForwardRef(val base: String, val join: String = "id") extends RecordQueryFieldPattern {
  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern] = {
    sys.error("XXX")
  }
}

class BackwardRef(val join: String, val base: String = "id") extends RecordQueryFieldPattern {
  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern] = {
    sys.error("XXX")
  }
}

//
abstract class RecordQueryField {
  def normalizePattern(rqctx: RecordQueryContext): List[RecordQueryPattern] = Nil
  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot]
//  def sqlColumnNames(rqctx: RecordQueryContext): List[String]
}

class Field(val atom: String) extends RecordQueryField {
  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(new FieldSlot(atom)(rqctx.entityUri.get, atom))
  }
/*
  def sqlColumnNames(rqctx: RecordQueryContext): List[String] = {
    rqctx.entityUri match {
      case Some(uri) => List(uri.name + "." + atom.name)
      case None => List(atom.name)
    }
  }
*/
}

class AllFields extends RecordQueryField {
  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(new AllFieldSlot(rqctx.entityUri.get))
  }
/*
  def sqlColumnNames(rqctx: RecordQueryContext): List[String] = {
    rqctx.entityUri match {
      case Some(uri) => List(uri.name + ".*")
      case None => List("*")
    }
  }
*/
}

class IncludeEntity(val uri: URI)(val pattern: RecordQueryFieldPattern)(val fields: List[RecordQueryField]) extends RecordQueryField {

  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot] = {
    def join_atoms = fields.flatMap {
      case f: Field => List(f.atom)
      case a: AllFields => sys.error("XXX")
    }

    def forward_ref(fr: ForwardRef) = {
      val baseuri = rqctx.entityUri.get
      val baseref = fr.base
      val joinuri = uri
      val joinkey = fr.join
      val joinatoms = join_atoms
      List(new ForwardIncludeSlot(baseref)(baseuri, baseref, joinuri, joinkey, joinatoms))
    }

    def backward_ref(br: BackwardRef) = {
      val baseuri = rqctx.entityUri.get
      val basekey = br.base
      val joinuri = uri
      val joinref = br.join
      val joinatoms = join_atoms
      List(new BackwardIncludeSlot(_last_name(joinuri))(baseuri, basekey, joinuri, joinref, joinatoms))
    }

    pattern match {
      case fr: ForwardRef => forward_ref(fr)
      case br: BackwardRef => backward_ref(br)
      case _ => sys.error("XXX")
    }
  }

  private def _last_name(uri: URI): String = uri.toString // XXX joinuri.last
/*
  def sqlColumnNames(rqctx: RecordQueryContext): List[String] = {
    rqctx.entityUri match {
      case Some(uri) => List("*")
      case None => List(uri.name + ".*")
    }
  }
*/
}

//
class EntityAll(val uri: URI, val atom: String, val ids: List[Any]) extends RecordQueryVirtualSlot {
  def copy(uri: URI = uri, atom: String = atom, ids: List[Any] = ids) = {
    new EntityAll(uri, atom, ids)
  }

  def normalizePattern(ctx: RecordQueryContext): List[RecordQueryPattern] = {
    List(new IdPattern(uri, atom, ids))
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    sys.error("XXX: EntityAll")
  }

/*
  def sqlColumnNames(rqctx: RecordQueryContext) = List(sqlColumnName)
  lazy val sqlColumnName = uri.name + ".*"
*/
}

//
case class Var(val atom: String)
