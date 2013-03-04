package org.goldenport.record.v2

/*
 * @since   Dec.  8, 2012
 * @version Mar.  4, 2013
 * @author  ASAMI, Tomoharu
 */
case class SqlColumn(
  name: String = null,
  isId: Boolean = false,
  isAutoId: Boolean = false,
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
}

object NullSqlColumn extends SqlColumn()
