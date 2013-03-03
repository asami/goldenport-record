package org.goldenport.record.v2

/*
 * Derived from SqlColumn.
 * 
 * @since   Jan.  9, 2013
 * @version Mar.  2, 2013
 * @author  ASAMI, Tomoharu
 */
case class SqlMethod(
  id: String,
  insert: SqlMethodAction,
  update: SqlMethodAction,
  delete: SqlMethodAction = PhysicalDelete
) {
  def useId = insert.useId
  def isMain = insert.isMain // XXX

  def create(column: Column) = SqlMethodCommand(
    column,
    insert.create(this, column),
    update.create(this, column)
  )
}
object NullMethod extends SqlMethod("", NullAction, NullAction)

case class SqlMethodCommand(
  column: Column,
  insert: SqlMethodActionCommand,
  update: SqlMethodActionCommand
)

/*
 * Insert
 */
trait SqlMethodAction {
  def useId: Boolean = false
  def isMain: Boolean = true

//  def associationTableName: Option[String] = None
//  def referencedTableName: Option[String] = None

  def create(parent: SqlMethod, column: Column): SqlMethodActionCommand
}

trait SqlMethodActionCommand {
  def buildRecourds(source: ActionContext, target: ActionContext): ActionContext = {
    target
  }

  def beforeRecords(source: ActionContext, target: ActionContext): ActionContext = {
    buildRecourds(source, target)
  }

  def afterRecords(source: ActionContext,
                   target: ActionContext): ActionContext = {
    buildRecourds(source, target)
  }
}

case object NullAction extends SqlMethodAction {
  def create(parent: SqlMethod, column: Column): SqlMethodActionCommand = NullActionCommand
}

case object NullActionCommand extends SqlMethodActionCommand {
}

/*
 * Insert - Value
 */
case class ValueInsert(
  tableName: String, columnName: Option[String] = None
) extends SqlMethodAction {
  def create(parent: SqlMethod, column: Column) = {
    ValueInsertCommand(tableName, columnName)
  }
}

case class ValueInsertCommand(
  tableName: String, columnName: Option[String] = None
) extends SqlMethodActionCommand {
  
}

/*
 * Insert - Entity
 */
case class EntityInsert(
  tableName: String, columnName: Option[String] = None
) extends SqlMethodAction {
  override def isMain = false

  def create(parent: SqlMethod, column: Column) = {
    EntityInsertCommand(tableName, columnName)
  }
}

case class EntityInsertCommand(
  tableName: String, columnName: Option[String] = None
) extends SqlMethodActionCommand {
}

/*
 * Insert - Association
 */
case class AssociationInsert(
  tableName: String, columnName: Option[String] = None
) extends SqlMethodAction {
  override def isMain = false

  def create(parent: SqlMethod, column: Column) = {
    AssociationInsertCommand(tableName, columnName)
  }
}

case class AssociationInsertCommand(
  tableName: String, columnName: Option[String] = None
) extends SqlMethodActionCommand {
}

/*
 * Insert - File
 */
case class FileInsert(filename: String) extends SqlMethodAction {
  def create(parent: SqlMethod, column: Column) = {
    sys.error("???")
  }
}

/*
 * Delete - File
 */
case object PhysicalDelete extends SqlMethodAction {
  def create(parent: SqlMethod, column: Column) = {
    sys.error("???")
  }
}
