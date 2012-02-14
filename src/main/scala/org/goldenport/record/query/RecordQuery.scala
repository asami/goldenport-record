package org.goldenport.record.query

import scala.collection.mutable.HashMap
import org.goldenport.util.{QSymbol, QSymbolSet}
import org.goldenport.util.Control.orP
import org.goldenport.record.sql.SqlU.{symbol2name => to_name, value2literal => to_literal}
import org.goldenport.record._

/**
 * @since   Aug. 29, 2010
 * @version Jun. 26, 2011
 * @author  ASAMI, Tomoharu
 */
class RecordQuery(val patterns: List[RecordQueryPattern], val slots: List[RecordQuerySlot], val context: RecordQueryContext) {
  def uris: QSymbolSet = {
    QSymbolSet(base_table)
  }

  private lazy val base_table = {
    patterns.find(_.isInstanceOf[IdPattern]) match {
      case Some(id: IdPattern) => id.uri
      case None => orP(slots) {
        case a: AllFieldSlot => a.uri
        case f: FieldSlot => f.uri
      }.getOrElse(QSymbol('notfound))
    }
  }

  private def base_table_name = base_table.name

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
      val alluris = QSymbolSet {
        slots flatMap {
          case fi: ForwardIncludeSlot => Nil
          case bi: BackwardIncludeSlot => Nil
          case f: FieldSlot => Nil
          case a: AllFieldSlot => List(a.uri)
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
      val names = alluris.map(_.name + ".*").toList ::: activeSlots.flatMap(_.sqlColumnNames(context)).toList
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
            " INNER JOIN " + fi.joinUri.name + " ON " +
            fi.sqlBaseRefColumnName + "=" + fi.sqlJoinKeyColumnName
          }
          case bi: BackwardIncludeSlot => {
            " INNER JOIN " + bi.joinUri.name + " ON " +
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

abstract class RecordQueryRealSlot(val slotAtom: Symbol) extends RecordQuerySlot {
  def columnAtoms(rqctx: RecordQueryContext): List[Symbol] = {
    List(slotAtom)
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

class IdPattern(val uri: QSymbol, val atom: Symbol, val ids: List[Any]) extends RecordQueryPattern {
  require (isIdValueList(ids))

  def copy(uri: QSymbol = uri, atom: Symbol = atom, ids: List[Any] = ids) = {
    new IdPattern(uri, atom, ids)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQueryPattern] = {
    List(copy())
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = List(sqlColumnName)
  lazy val sqlColumnName = uri.name + "." + atom.name

  def sqlWhereClauses = ids.map(sqlColumnName + "=" + to_literal(_))
}

class ForwardIncludeSlot(slotatom: Symbol)(val baseUri: QSymbol, val baseRef: Symbol, val joinUri: QSymbol, val joinKey: Symbol, val joinAtoms: List[Symbol]) extends RecordQueryRealSlot(slotatom) {
  def copy(baseUri: QSymbol = baseUri, baseRef: Symbol = baseRef,
           joinUri: QSymbol = joinUri, joinKey: Symbol = joinKey,
           joinAtoms: List[Symbol] = joinAtoms) = {
    new ForwardIncludeSlot(slotAtom)(baseUri, baseRef, joinUri, joinKey, joinAtoms)  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  override def columnAtoms(rqctx: RecordQueryContext) = {
    if (joinAtoms.length == 1) List(slotAtom)
    else joinAtoms.map(a => Symbol(slotAtom.name + "_" + a.name))
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = {
    joinAtoms.map(joinUri.name + "." + _.name)
  }

  def sqlBaseRefColumnName = baseUri.name + "." + baseRef.name
  def sqlJoinKeyColumnName = joinUri.name + "." + joinKey.name
}

class BackwardIncludeSlot(slotatom: Symbol)
                         (val baseUri: QSymbol, val baseKey: Symbol,
                          val joinUri: QSymbol, val joinRef: Symbol, val joinAtoms: List[Symbol]) extends RecordQueryRealSlot(slotatom) {
  def copy(baseUri: QSymbol = baseUri, baseKey: Symbol = baseKey,
           joinUri: QSymbol = joinUri, joinRef: Symbol = joinRef,
           joinAtoms: List[Symbol] = joinAtoms) = {
    new BackwardIncludeSlot(slotAtom)(baseUri, baseKey, joinUri, joinRef, joinAtoms)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  override def columnAtoms(rqctx: RecordQueryContext) = {
    if (joinAtoms.length == 1) List(slotAtom)
    else joinAtoms.map(a => Symbol(slotAtom.name + "_" + a.name))
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = {
    joinAtoms.map(joinUri.name + "." + _.name)
  }

  def sqlBaseKeyColumnName = baseUri.name + "." + baseKey.name
  def sqlJoinRefColumnName = joinUri.name + "." + joinRef.name
}

class FieldSlot(slotatom: Symbol)(val uri: QSymbol, val atom: Symbol) extends RecordQueryRealSlot(slotatom) {
  def copy(uri: QSymbol = uri, atom: Symbol = atom) = {
    new FieldSlot(slotatom)(uri, atom)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = List(uri.name + "." + atom.name)
}

class AllFieldSlot(val uri: QSymbol) extends RecordQueryRealSlot(Symbol("*")) {
  def copy(uri: QSymbol = uri) = {
    new AllFieldSlot(uri)
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    List(this)
  }

  def sqlColumnNames(rqctx: RecordQueryContext) = List(uri.name + ".*")
}

object IdPattern {
  def apply(uri: QSymbol, atom: Symbol, ids: List[Any]) = new IdPattern(uri, atom, ids)

  def apply(uri: QSymbol, atom: Symbol, id: Any) = new IdPattern(uri, atom, List(id))
}

object ForwardIncludeSlot {
  def apply(baseUri: QSymbol, baseRef: Symbol,
            joinUri: QSymbol, joinKey: Symbol, joinName: Symbol) = {
    new ForwardIncludeSlot(baseRef)(baseUri, baseRef, joinUri, joinKey, List(joinName))
  }
/*
  def apply(atom: Symbol)(uri: QSymbol, cderive: CDerivation) = {
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
  def apply(baseUri: QSymbol, baseKey: Symbol,
            joinUri: QSymbol, joinRef: Symbol, joinAtom: Symbol) = {
    new BackwardIncludeSlot(joinUri.last)(baseUri, baseKey, joinUri, joinRef, List(joinAtom))
  }
}

object FieldSlot {
  def apply(uri: QSymbol, atom: Symbol) = new FieldSlot(atom)(uri, atom)
}

object AllFieldSlot {
  def apply(uri: QSymbol) = new AllFieldSlot(uri)
}

// XXX
// FLWOR = For Let Where OrderBy Return
object Entity {
  def apply(uri: QSymbol)(pattern: RecordQueryFieldPattern)(fields: RecordQueryField*) = {
    new Entity(uri)(pattern)(fields.toList)
  }
}

object EntityAll {
  def apply(uri: QSymbol, id: Any) = new EntityAll(uri, 'id, List(id))
  def apply(uri: QSymbol, atom: Symbol, id: Any) = new EntityAll(uri, atom, List(id))
}

object Id {
  def apply(atom: Symbol, values: List[Any]) = new Id(atom, values)

  def apply(atom: Symbol, value: Any) = new Id(atom, value)

  def apply(values: List[Any]) = new Id(values)

  def apply(value: Any) = new Id(value)
}

object Field {
  def apply(atom: Symbol) = new Field(atom)
}

object AllFields extends AllFields {
  def apply() = new AllFields
}

object IncludeEntity {
  def apply(uri: QSymbol, refAttr: Symbol)(fields: RecordQueryField*) = {
    new IncludeEntity(uri)(new ForwardRef(refAttr))(fields.toList)
  }

  def apply(uri: QSymbol, pattern: RecordQueryFieldPattern)(fields: RecordQueryField*) = {
    new IncludeEntity(uri)(pattern)(fields.toList)
  }
}

object ForwardRef {
  def apply(atom: Symbol) = new ForwardRef(atom)
}

object BackRef {
  def apply(atom: Symbol) = new BackwardRef(atom)
}

/*
 * Virtual slot.
 * Before query execution, virtual slots are normalized to real slots.
 */
class Entity(val uri: QSymbol)(val pattern: RecordQueryFieldPattern)(val fields: List[RecordQueryField]) extends RecordQueryVirtualSlot {
  def copy(uri: QSymbol = uri)(pattern: RecordQueryFieldPattern = pattern)(fields: List[RecordQueryField] = fields) = {
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
  private val variable_values = new HashMap[Symbol, Any]

  def variable(atom: Symbol) = variable_values(atom)

  def getVariable(atom: Symbol): Option[Any] = variable_values.get(atom)

  def setVariable(atom: Symbol, value: Any) {
    variable_values += atom -> value
  }

  def ids = variable('id).asInstanceOf[List[Any]]

  def entityUri: Option[QSymbol]

  def setIds(ids: List[Any]) {
    require (isIdValueList(ids))
    variable_values += 'id -> ids
  }
}

class RootContext extends RecordQueryContext {
  def entityUri = None
}

class EntityContext(val entity: Entity, parent: RecordQueryContext) extends RecordQueryContext(Some(parent)) {
  def entityUri = Some(entity.uri)

  override def variable(atom: Symbol): Any = {
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

class Id(val atom: Symbol, val ids: List[Any]) extends RecordQueryFieldPattern {
  def this(atom: Symbol, id: Any) = this(atom, List(id))
  def this(ids: List[Any]) = this('id, ids)
  def this(id: Any) = this(List(id))

  require (isIdValueList(ids))

  private def sql_column_name = atom.name
  private def sql_column_name(uri: QSymbol) = uri.name + "." + atom.name

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

class ForwardRef(val base: Symbol, val join: Symbol = 'id) extends RecordQueryFieldPattern {
  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern] = {
    error("XXX")
  }
}

class BackwardRef(val join: Symbol, val base: Symbol = 'id) extends RecordQueryFieldPattern {
  def normalize(rqctx: RecordQueryContext): List[RecordQueryPattern] = {
    error("XXX")
  }
}

//
abstract class RecordQueryField {
  def normalizePattern(rqctx: RecordQueryContext): List[RecordQueryPattern] = Nil
  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot]
//  def sqlColumnNames(rqctx: RecordQueryContext): List[String]
}

class Field(val atom: Symbol) extends RecordQueryField {
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

class IncludeEntity(val uri: QSymbol)(val pattern: RecordQueryFieldPattern)(val fields: List[RecordQueryField]) extends RecordQueryField {

  def normalize(rqctx: RecordQueryContext): List[RecordQuerySlot] = {
    def join_atoms = fields.flatMap {
      case f: Field => List(f.atom)
      case a: AllFields => error("XXX")
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
      List(new BackwardIncludeSlot(joinuri.last)(baseuri, basekey, joinuri, joinref, joinatoms))
    }

    pattern match {
      case fr: ForwardRef => forward_ref(fr)
      case br: BackwardRef => backward_ref(br)
      case _ => error("XXX")
    }
  }
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
class EntityAll(val uri: QSymbol, val atom: Symbol, val ids: List[Any]) extends RecordQueryVirtualSlot {
  def copy(uri: QSymbol = uri, atom: Symbol = atom, ids: List[Any] = ids) = {
    new EntityAll(uri, atom, ids)
  }

  def normalizePattern(ctx: RecordQueryContext): List[RecordQueryPattern] = {
    List(new IdPattern(uri, atom, ids))
  }

  def normalize(ctx: RecordQueryContext): List[RecordQuerySlot] = {
    error("XXX: EntityAll")
  }

/*
  def sqlColumnNames(rqctx: RecordQueryContext) = List(sqlColumnName)
  lazy val sqlColumnName = uri.name + ".*"
*/
}

//
case class Var(val atom: Symbol)
