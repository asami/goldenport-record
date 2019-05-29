package org.goldenport.record.v2

import scalaz._, Scalaz._
import java.net.URI
import org.goldenport.RAISE

/*
 * Derived from SqlSchema.
 * 
 * @since   Jan.  9, 2013
 *  version Mar. 12, 2013
 *  version Apr. 26, 2013
 *  version Jun. 24, 2013
 *  version Feb.  6, 2014
 *  version Jan. 27, 2015
 *  version May. 26, 2017
 * @version May. 29, 2019
 * @author  ASAMI, Tomoharu
 */
trait SqlAction {
  def id: String
  /**
   * Use inserted id
   */
  def useId: Boolean = false

  def create(schema: Schema): SqlActionCommand = {
    val cs = schema.columns.flatMap(accept_method).map {
      case (c, m) => (c, m.create(c))
    }
    val id = schema.columns.filter(is_id)
    create_ActionCommand(schema, cs, id)
  }

  protected def create_ActionCommand(schema: Schema, columns: Seq[(Column, SqlMethodCommand)], ids: Seq[Column]): SqlActionCommand = {
    val id = ids.headOption
    create_ActionCommand(schema, columns, id)
  }

  protected def create_ActionCommand(schema: Schema, columns: Seq[(Column, SqlMethodCommand)], id: Option[Column]): SqlActionCommand = {
    sys.error("Implements create_ActionCommand in SqlAction.")
  }

  protected def accept_method(column: Column): Option[(Column, SqlMethod)] = {
    column.sql.methods.find(_.id == id).map((column, _))
  }

  protected def is_id(column: Column): Boolean = {
    column.sql.isId
  }
}

trait SqlActionCommand {
  def schema: Schema
  def columns: Seq[(Column, SqlMethodCommand)]
  def ids: Seq[Column]

  lazy val methods = columns.map(_._2)

  def beforeInsert(source: ActionContext): ActionContext = {
    before_Insert(source)
  }

  // legacy
  def beforeInsert(xs: Seq[ActionContext]): Seq[ActionContext] = {
    before_Insert(xs)
  }

  def beforeInserts(xs: ActionContextChunk): ActionContextChunk =
    before_Inserts(xs)

  def afterInsert(source: ActionContext): ActionContext = {
    after_Insert(source)
  }

  // legacy
  def afterInsert(xs: Seq[ActionContext]): Seq[ActionContext] = {
    after_Insert(xs)
  }

  def afterInserts(xs: ActionContextChunk): ActionContextChunk =
    after_Inserts(xs)

  def beforeUpdate(source: ActionContext): ActionContext = {
    before_Update(source)
  }

  // legacy
  def beforeUpdate(xs: Seq[ActionContext]): Seq[ActionContext] = {
    before_Update(xs)
  }

  def beforeUpdates(xs: ActionContextChunk): ActionContextChunk =
    before_Updates(xs)

  def afterUpdate(source: ActionContext): ActionContext = {
    after_Update(source)
  }

  // legacy
  def afterUpdate(xs: Seq[ActionContext]): Seq[ActionContext] = {
    after_Update(xs)
  }

  def afterUpdates(xs: ActionContextChunk): ActionContextChunk =
    after_Updates(xs)

  def beforeDelete(source: ActionContext): ActionContext = {
    before_Delete(source)
  }

  def beforeDelete(xs: Seq[ActionContext]): Seq[ActionContext] = {
    before_Delete(xs)
  }

  def afterDelete(source: ActionContext): ActionContext = {
    after_Delete(source)
  }

  def afterDelete(xs: Seq[ActionContext]): Seq[ActionContext] = {
    after_Delete(xs)
  }

  protected def before_Insert(source: ActionContext): ActionContext = {
    source
  }

  // legacy
  protected def before_Insert(xs: Seq[ActionContext]): Seq[ActionContext] = {
    xs.map(before_Insert(_))
  }

  protected def before_Inserts(p: ActionContextChunk): ActionContextChunk = {
    val r = before_Insert(p.toActionContexts)
    p.withActionContexts(r)
  }

  protected def after_Insert(source: ActionContext): ActionContext = source

  // legacy
  protected def after_Insert(xs: Seq[ActionContext]): Seq[ActionContext] = {
    xs.map(after_Insert(_))
  }

  protected def after_Inserts(p: ActionContextChunk): ActionContextChunk =
    p.run(after_Insert)

  protected def before_Update(source: ActionContext): ActionContext = {
    source
  }

  // legacy
  protected def before_Update(xs: Seq[ActionContext]): Seq[ActionContext] = {
    xs.map(before_Update(_))
  }

  protected def before_Updates(p: ActionContextChunk): ActionContextChunk =
    p.run(before_Update)

  protected def after_Update(source: ActionContext): ActionContext = source

  // legacy
  protected def after_Update(xs: Seq[ActionContext]): Seq[ActionContext] = {
    xs.map(after_Update(_))
  }

  protected def after_Updates(p: ActionContextChunk): ActionContextChunk =
    p.run(after_Update)

  protected def before_Delete(source: ActionContext): ActionContext = {
    source
  }

  protected def before_Delete(xs: Seq[ActionContext]): Seq[ActionContext] = {
    xs.map(before_Delete(_))
  }

  protected def after_Delete(source: ActionContext): ActionContext =
    source

  protected def after_Delete(xs: Seq[ActionContext]): Seq[ActionContext] = {
    xs.map(after_Delete(_))
  }

  protected def before_records(source: ActionContext): ActionContext = {
    methods.foldLeft(source)((a, x) => {
      x.insert.beforeRecords(source, a)
    })
  }

  protected def after_records(source: ActionContext): ActionContext = {
    methods.foldLeft(source)((a, x) => {
      x.insert.afterRecords(source, a)
    })
  }

  //
  protected final def insert_records_driver(driver: MutateDriver, context: ActionContext): ActionContext = {
    insert_records_driver(driver, context.outRecords)
    context
  }

  protected final def insert_records_driver(driver: MutateDriver, records: Seq[Record]) {
    for (r <- records) {
      driver.insert(r)
    }
  }

  protected final def insert_records_id_driver(driver: MutateDriver, context: ActionContext, key: String = ActionContext.KEY_REFERENCE_IDS): ActionContext = {
    val ids = insert_records_id_driver(driver, context.outRecords)
    context.setProperty(key, ids)
  }

  protected final def insert_records_id_driver(driver: MutateDriver, records: Seq[Record]): Seq[String] = {
    for (r <- records) yield {
      driver.insertId(r)
    }
  }

  //
  protected final def insert_records(f: Record => Unit, context: ActionContext): ActionContext = {
    insert_records(f, context.outRecords)
    context
  }

  protected final def insert_records(f: Record => Unit, records: Seq[Record]) {
    for (r <- records) {
      f(r)
    }
  }

  protected final def insert_records_id(f: Record => Any, context: ActionContext, key: String = ActionContext.KEY_REFERENCE_IDS): ActionContext = {
    val ids = insert_records_id(f, context.outRecords)
    context.setProperty(key, ids)
  }

  protected final def insert_records_id(f: Record => Any, records: Seq[Record]): Seq[Any] = {
    for (r <- records) yield {
      f(r)
    }
  }

  //
  protected final def get_main_record_id(context: ActionContext): Option[Any] = {
    get_record_ids(context, ActionContext.KEY_MAIN_ID) match {
      case Some(Nil) => None
      case Some(xs) => Some(xs.head)
      case None => None
    }
  }

  protected final def get_reference_record_ids(context: ActionContext): Option[Seq[Any]] = {
    get_record_ids(context, ActionContext.KEY_REFERENCE_IDS)
  }

  protected final def get_record_ids(context: ActionContext, key: String): Option[Seq[Any]] = {
    context.getProperty(key)
  }
}

case class SqlActionCommands(commands: Seq[SqlActionCommand]) {
  def beforeInsert(
    record: Record,
    conn: Option[java.sql.Connection]
  ): ActionContext = {
    commands.foldLeft(ActionContext(record, connection = conn))((a, x) => x.beforeInsert(a))
  }

  // legacy
  def beforeInsert(
    rs: Seq[Record],
    conn: Option[java.sql.Connection]
  ): Seq[ActionContext] = {
    val acs = rs.map(ActionContext(_, connection = conn))
    commands.foldLeft(acs)((a, x) => x.beforeInsert(a))
  }

  def beforeInserts(
    rs: Seq[Record],
    conn: Option[java.sql.Connection]
  ): ActionContextChunk = {
    val acs = rs.map(ActionContext(_))
    val acc = ActionContextChunk(acs, conn)
    commands.foldLeft(acc)((z, x) => x.beforeInserts(z))
  }

  def afterInsert(source: ActionContext): ActionContext =
    commands./:(source)((z, x) => x.afterInsert(z))

  // legacy
  def afterInsert(rs: Seq[ActionContext]): Seq[ActionContext] =
    rs.map(afterInsert)

  def afterInserts(p: ActionContextChunk): ActionContextChunk =
    commands.foldLeft(p)((z, x) => x.afterInserts(z))

  def beforeUpdate(
    record: Record,
    conn: Option[java.sql.Connection]
  ): ActionContext = {
    commands.foldLeft(ActionContext(record, connection = conn))((a, x) => x.beforeUpdate(a))
  }

  def beforeUpdate(
    rs: Seq[Record],
    conn: Option[java.sql.Connection]
  ): Seq[ActionContext] = {
    val acs = rs.map(ActionContext(_, connection = conn))
    commands.foldLeft(acs)((a, x) => x.beforeUpdate(a))
  }

  def beforeUpdates(
    rs: Seq[Record],
    conn: Option[java.sql.Connection]
  ): ActionContextChunk = {
    val acs = rs.map(ActionContext(_))
    val acc = ActionContextChunk(acs, conn)
    commands.foldLeft(acc)((z, x) => x.beforeUpdates(z))
  }

  def afterUpdate(source: ActionContext): ActionContext =
    commands./:(source)((z, x) => x.afterUpdate(z))

  def afterUpdate(rs: Seq[ActionContext]): Seq[ActionContext] =
    rs.map(afterUpdate)

  def afterUpdates(p: ActionContextChunk): ActionContextChunk =
    p.run(afterUpdate)

  def beforeDelete(
    record: Record,
    conn: Option[java.sql.Connection]
  ): ActionContext = {
    commands.foldLeft(ActionContext(record, connection = conn))((a, x) => x.beforeDelete(a))
  }

  def beforeDelete(
    rs: Seq[Record],
    conn: Option[java.sql.Connection]
  ): Seq[ActionContext] = {
    val acs = rs.map(ActionContext(_, connection = conn))
    commands.foldLeft(acs)((a, x) => x.beforeDelete(a))
  }

  def afterDelete(source: ActionContext): ActionContext =
    commands./:(source)((z, x) => x.afterDelete(z))

  def afterDelete(rs: Seq[ActionContext]): Seq[ActionContext] =
    rs.map(afterDelete)
}

/*
case class IdGeneratorAction(
  f: Record => String
) extends SqlAction {
  override def create_ActionCommand(schema: Schema, columns: Seq[(Column, SqlMethodCommand)], ids: Seq[Column]): SqlActionCommand = {
    IdGeneratorActionCommand(this, schema, columns, ids)
  }
}

case class IdGeneratorActionCommand(
  action: IdGeneratorAction,
  schema: Schema,
  columns: Seq[(Column, SqlMethodCommand)],
  ids: Seq[Column]
) extends SqlActionCommand {
  override def before_Insert(context: ActionContext) = {
    context.copy(context.in ::+ "id" -> "action-id")
  }
}
*/

/*
 * Inserts an entity with:
 * - inserting just an association entity to add an 1-N reference to existed entity.
 * - inserting both a target entity and an association entity to refer with it.
 * - inserting both a target entity with file and an association entity to refer with it.
 * - updating a reference to the inserted entity in an existed entity.
 * - updating a reference to an exist target entity in the inserted entity.
 * - updating a reference to an newly inserting target entity in the inserted entity.
 *
 * Updates an entity with:
 * - inserting an association entity to add an 1-N reference to existed entity.
 * - inserting both a target entity and an association entity to refer with it.
 * - inserting both a target entity with file and an association entity to refer with it.
 * - updating a reference to the inserted entity in an existed entity.
 * - updating a reference to an exist target entity in the inserted entity.
 * - updating a reference to an newly inserting target entity in the inserted entity.
 * - updating a reference to an newly inserting target entity with file in the inserted entity.
 *
 * Deletes an entity with:
 * - removes association: 
 *   - deleting the association entity.
 *   - updating the reference attribute of target entity to erase the reference.
 * - deleting the target entity or not.
 * - physical vs. logical
 * 
 * Variation points are:
 * - association representation: source attribute, target attribute, simple assocition entity, first-class association entity
 * - inserting target entity or using existed entity.
 * - file upload in:
 *   - none
 *   - source attribute
 *   - first-class association entity
 *   - target attribute
 * - ownership of target entity
 *   - owner. When update and delete, cascading delete.
 *   - not owner. When update and delete, removes just reference.
 */

/**
 * <ul>
 * <li>simple association entity
 * <li>useing existed entity
 * <li>no file upload
 * <li>no owner
 * </ul>
 */
case class AssociationAction(
  association: MutateDriver, // SqlDriver,
  mainColumnName: String,
  referenceColumnName: String,
  id: String = "association"
) extends SqlAction {
  override val useId = true

  override def create_ActionCommand(schema: Schema, columns: Seq[(Column, SqlMethodCommand)], ids: Seq[Column]): SqlActionCommand = {
    AssociationActionCommand(this, schema, columns, ids)
  }
}

case class AssociationActionCommand(
  action: AssociationAction,
  schema: Schema,
  columns: Seq[(Column, SqlMethodCommand)],
  ids: Seq[Column]
) extends SqlActionCommand {
  override def after_Insert(context: ActionContext) = {
//    log_trace("AssociationActionCommand#after_Insert = " + context)
    val a = context.buildMainReference(action.mainColumnName, action.referenceColumnName)
    val b = after_records(a)
    insert_records_driver(action.association, b)
    context
  }
}

/**
 * <ul>
 * <li>target entity
 * <li>append
 * <li>file upload
 * <li>no owner
 * </ul>
 */
case class TargetFileAction(
  target: MutateDriver, // SqlDriver,
  columnName: String,
  uploader: Seq[InputFile] => Seq[UploadFile],
  key: String = null,
  id: String = "target-file"
) extends SqlAction {
  override val useId = true

  override protected def create_ActionCommand(schema: Schema, columns: Seq[(Column, SqlMethodCommand)], ids: Seq[Column]): SqlActionCommand = {
    TargetFileActionCommand(this, schema, columns, ids)
  }
}

case class TargetFileActionCommand(
  action: TargetFileAction,
  schema: Schema,
  columns: Seq[(Column, SqlMethodCommand)],
  ids: Seq[Column]
) extends SqlActionCommand {
  val key = Option(action.key) | action.columnName

  override def before_Insert(source: ActionContext): ActionContext = {
//    log_trace("TargetFileActionCommand#before_Insert context = " + source)
//    val ins = source.in.inputFiles(key)
    val ins = source.in.inputFiles
    val files = action.uploader(ins)
    val a = before_records(source)
    val b = a.addUploadFiles(action.columnName, files)
//    log_trace("TargetFileActionCommand#before_Insert = " + b)
    val c = insert_records_id_driver(action.target, b)
    c
  }
}

/**
 * <ul>
 * <li>simple association entity
 * <li>inserting target entity or useing existed entity depends on record
 * <li>file upload
 * <li>no owner
 * </ul>
 */
case class TargetFileAssociationAction(
  target: TargetFileAction,
  association: AssociationAction,
  id: String = "target-file-association"
) extends SqlAction {
  override val useId = true

  override protected def create_ActionCommand(schema: Schema, columns: Seq[(Column, SqlMethodCommand)], ids: Seq[Column]): SqlActionCommand = {
    TargetFileAssociationActionCommand(this, schema, columns, ids)
  }
}

case class TargetFileAssociationActionCommand(
  action: TargetFileAssociationAction,
  schema: Schema,
  columns: Seq[(Column, SqlMethodCommand)],
  ids: Seq[Column]
) extends SqlActionCommand {
  val target = action.target.create(schema)
  val association = action.association.create(schema)

  override def before_Insert(source: ActionContext): ActionContext = {
    source |> target.beforeInsert |> association.beforeInsert
  }

  override def after_Insert(source: ActionContext) = {
    target.afterInsert(source)
    association.afterInsert(source)
    source
  }
}
