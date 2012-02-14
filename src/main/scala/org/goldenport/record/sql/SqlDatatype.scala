package org.goldenport.record.sql

/**
 * derived from org.goldenport.g3.messages.sql.SqlDatatype
 * 
 * @since   Apr. 12, 2010
 *  version Jun. 26, 2011
 * @version Feb. 14, 2012
 * @author  ASAMI, Tomoharu
 */
abstract class SqlDatatype() {
  def fullName: String
}

abstract class SqlConstraint() {
  def fullName: String
}

class Array extends SqlDatatype {
  def fullName = "Array"
}

object Array extends Array {
  def apply() = new Array
}

class BIGINT extends SqlDatatype {
  def fullName = "BIGINT"
}

object BIGINT extends BIGINT {
  def apply() = new BIGINT
}

class BINARY extends SqlDatatype {
  def fullName = "BINARY"
}

object BINARY extends BINARY {
  def apply() = new BINARY
}

class BIT extends SqlDatatype {
  def fullName = "BIT"
}

object BIT extends BIT {
  def apply() = new BIT
}

class BLOB extends SqlDatatype {
  def fullName = "BLOB"
}

object BLOB extends BLOB {
  def apply() = new BLOB
}

class BOOLEAN extends SqlDatatype {
  def fullName = "BOOLEAN"
}

object BOOLEAN extends BOOLEAN {
  def apply() = new BOOLEAN
}

class CHAR(val length: Int) extends SqlDatatype {
  def fullName = "VARCHAR(%s)".format(length)
}

object CHAR {
  def apply(length: Int) = new CHAR(length)
}

class CLOB extends SqlDatatype {
  def fullName = "CLOB"
}

object CLOB extends CLOB {
  def apply() = new CLOB
}

class DATALINK extends SqlDatatype {
  def fullName = "DATALINK"
}

object DATALINK extends DATALINK {
  def apply() = new DATALINK
}

class DATE extends SqlDatatype {
  def fullName = "DATE"
}

object DATE extends DATE {
  def apply() = new DATE
}

class DECIMAL extends SqlDatatype {
  def fullName = "DECIMAL"
}

object DECIMAL extends DECIMAL {
  def apply() = new DECIMAL
}

class DISTINCT extends SqlDatatype {
  def fullName = "DISTINCT"
}

object DISTINCT extends DISTINCT {
  def apply() = new DISTINCT
}

class DOUBLE extends SqlDatatype {
  def fullName = "DOUBLE"
}

object DOUBLE extends DOUBLE {
  def apply() = new DOUBLE
}

class FLOAT extends SqlDatatype {
  def fullName = "FLOAT"
}

object FLOAT extends FLOAT {
  def apply() = new FLOAT
}

class INTEGER extends SqlDatatype {
  def fullName = "INTEGER"
}

object INTEGER extends INTEGER {
  def apply() = new INTEGER
}

class JAVA_OBJECT extends SqlDatatype {
  def fullName = "JAVA_OBJECT"
}

object JAVA_OBJECT extends JAVA_OBJECT {
  def apply() = new JAVA_OBJECT
}

class LONGNVARCHAR(val length: Int) extends SqlDatatype {
  def fullName = "LONGNVARCHAR(%s)".format(length)
}

object LONGNVARCHAR {
  def apply(length: Int) = new LONGNVARCHAR(length)
}

class LONGVARBINARY(val length: Int) extends SqlDatatype {
  def fullName = "LONGVARBINARY(%s)".format(length)
}

object LONGVARBINARY {
  def apply(length: Int) = new LONGVARBINARY(length)
}

class LONGVARCHAR(val length: Int) extends SqlDatatype {
  def fullName = "LONGVARCHAR(%s)".format(length)
}

object LONGVARCHAR {
  def apply(length: Int) = new LONGVARCHAR(length)
}

class NCHAR(val length: Int) extends SqlDatatype {
  def fullName = "NCHAR(%s)".format(length)
}

object NCHAR {
  def apply(length: Int) = new NCHAR(length)
}

class NCLOB extends SqlDatatype {
  def fullName = "NCLOB"
}

object NCLOB extends NCLOB {
  def apply() = new NCLOB
}

class NULL extends SqlDatatype {
  def fullName = "NULL"
}

object NULL extends NULL {
  def apply() = new NULL
}

class NUMERIC extends SqlDatatype {
  def fullName = "NUMERIC"
}

object NUMERIC extends NUMERIC {
  def apply() = new NUMERIC
}

class NVARCHAR(val length: Int) extends SqlDatatype {
  def fullName = "NVARCHAR(%s)".format(length)
}

object NVARCHAR {
  def apply(length: Int) = new NVARCHAR(length)
}

class OTHER extends SqlDatatype {
  def fullName = "OTHER"
}

object OTHER extends OTHER {
  def apply() = new OTHER
}

class REAL extends SqlDatatype {
  def fullName = "REAL"
}

object REAL extends REAL {
  def apply() = new REAL
}

class REF extends SqlDatatype {
  def fullName = "REF"
}

object REF extends REF {
  def apply() = new REF
}

class ROWID extends SqlDatatype {
  def fullName = "ROWID"
}

object ROWID extends ROWID {
  def apply() = new ROWID
}

class SMALLINT extends SqlDatatype {
  def fullName = "SMALLINT"
}

object SMALLINT extends SMALLINT {
  def apply() = new SMALLINT
}

class SQLXML extends SqlDatatype {
  def fullName = "SQLXML"
}

object SQLXML extends SQLXML {
  def apply() = new SQLXML
}

class STRUCT extends SqlDatatype {
  def fullName = "STRUCT"
}

object STRUCT extends STRUCT {
  def apply() = new STRUCT
}

class TIME extends SqlDatatype {
  def fullName = "TIME"
}

object TIME extends TIME {
  def apply() = new TIME
}

class TIMESTAMP extends SqlDatatype {
  def fullName = "TIMESTAMP"
}

object TIMESTAMP extends TIMESTAMP {
  def apply() = new TIMESTAMP
}

class TINYINT extends SqlDatatype {
  def fullName = "TINYINT"
}

object TINYINT extends TINYINT {
  def apply() = new TINYINT
}

class VARBINARY extends SqlDatatype {
  def fullName = "VARBINARY"
}

object VARBINARY extends VARBINARY {
  def apply() = new VARBINARY
}

class VARCHAR(val length: Int) extends SqlDatatype {
  def fullName = "VARCHAR(%s)".format(length)
}

object VARCHAR extends INTEGER {
  def apply(length: Int) = new VARCHAR(length)
}

// SQL Constraint
class PRIMARY_KEY extends SqlConstraint {
  def fullName = "PRIMARY KEY"
}

object PRIMARY_KEY extends PRIMARY_KEY {
  def apply() = new PRIMARY_KEY
}

class AUTO_INCREMENT extends SqlConstraint {
  def fullName = "AUTO_INCREMENT"
}

object AUTO_INCREMENT extends AUTO_INCREMENT {
  def apply() = new AUTO_INCREMENT
}

class UNIQUE extends SqlConstraint {
  def fullName = "UNIQUE"
}

object UNIQUE extends AUTO_INCREMENT {
  def apply() = new UNIQUE
}
