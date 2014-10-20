package org.goldenport.record.v2

/*
 * @snice   Nov. 26, 2012
 *  version Dec. 20, 2012
 *  version Feb. 20, 2013
 * @version Oct. 20, 2014
 * @author  asami
 */
sealed trait Visibility {
  val create: Boolean
  val read: Boolean
  val update: Boolean
  val delete: Boolean
  val grid: Boolean
  val dataview: Boolean
}

case object PlainVisibility extends Visibility {
  val create = true
  val read = true
  val update = true
  val delete = true
  val grid = true
  val dataview = true
}

case object DetailVisibility extends Visibility {
  val create = true
  val read = true
  val update = true
  val delete = true
  val grid = false
  val dataview = true
}

case object ReadVisibility extends Visibility {
  val create = false
  val read = true
  val update = false
  val delete = false
  val grid = true
  val dataview = true
}

case object HiddenVisibility extends Visibility {
  val create = false
  val read = false
  val update = false
  val delete = false
  val grid = false
  val dataview = false
}

case class CustomVisibility(
  create: Boolean,
  read: Boolean,
  update: Boolean,
  delete: Boolean,
  grid: Boolean,
  dataview: Boolean
)
