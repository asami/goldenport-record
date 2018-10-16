package org.goldenport.record.v2.bag

/*
 * @since   Oct.  9, 2015
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
case class RecordBagSheet[SHEET](
  name: String,
  sheet: SHEET,
  view: RecordBagView,
  mutator: Option[RecordBagMutator] = None
)
