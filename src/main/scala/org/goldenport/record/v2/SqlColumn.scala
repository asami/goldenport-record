package org.goldenport.record.v2

import org.goldenport.record.sql.SqlDatatype

/*
 * @since   Dec.  8, 2012
 *  version Mar.  4, 2013
 *  version Apr.  7, 2019
 * @version Oct.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class SqlColumn(
  name: String = null,
  datatype: Option[SqlDatatype] = None,
  isId: Boolean = false,
  isAutoId: Boolean = false,
  isUnique: Boolean = false,
  isReadOnly: Boolean = false,
  isAutoCreate: Boolean = false,
  isAutoUpdate: Boolean = false,
  methods: Seq[SqlMethod] = Nil,
  isDerived: Boolean = false, // derived data. ignores in insert/update.
  value: String = null,
  valueGenerator: Record => String = null
) {
  require (!isAutoId || isId, "isAutoId can be true when isId is true.")

  def hasValue = value != null

  def commands(column: Column) = methods.map(_.create(column))

  lazy val getColumnName: Option[String] = Option(name)
}

object NullSqlColumn extends SqlColumn()
