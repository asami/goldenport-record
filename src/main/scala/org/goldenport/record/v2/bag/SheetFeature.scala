package org.goldenport.record.v2.bag

import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import org.goldenport.bag.ChunkBag
import org.goldenport.record.v2._

/*
 * @since   Oct.  8, 2015
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
trait SheetFeature { self: RecordBag =>
  type SHEET
  type ROW

//  def sheetName: Option[String]

  private var _sheets_state: Option[List[RecordBagSheet[SHEET]]] = None

  private def _sheets: List[RecordBagSheet[SHEET]] = {
    _sheets_state getOrElse {
      set_sheets(load_Sheets())
    }
  }

  protected def load_Sheets(): List[RecordBagSheet[SHEET]]

  def recordsR: Process[Task, Record] = {
    _sheets.headOption match {
      case Some(s) => s.view.recordsR
      case None => throw new IllegalStateException("SheetFeature#recordsR: no sheets")
    }
  }

  def recordW: Sink[Task, Record] = ???

  def recordsW: Sink[Task, Seq[Record]] = ???

//  protected def new_Sheets(): List[RecordBagSheet[SHEET]] = Nil
  // protected def append_Row(sheet: SHEET, row: ROW): SHEET = ???
  // protected def replace_Row(sheet: SHEET, row: ROW): SHEET = ???
  // protected def update_Row(sheet: SHEET, row: ROW): SHEET = ???

  protected final def getSheets = _sheets
  protected final def to_sheet = _sheets.headOption getOrElse {
    throw new IllegalStateException("SheetFeature: No sheets")
  }

  protected final def set_sheets(sheets: List[RecordBagSheet[SHEET]]) = {
    _sheets_state = Some(sheets)
    sheets
  }

  protected final def get_sheets = _sheets
  protected final def get_views = _sheets.map(_.view)

  protected final def get_sheet_map: Map[String, RecordBagSheet[SHEET]] = {
    _sheets.groupBy(_.name) flatMap {
      case (k, Nil) => None
      case (k, x :: Nil) => Some(k -> x)
      case (k, xs) => throw new IllegalStateException(s"SheetFeature#get_sheet_map: duplicate sheet name '$k'")
    }
  }

  protected final def append_row(row: ROW) {
//    _sheet = append_Row(_sheet, row)
    ???
  }

  protected final def replace_row(row: ROW) {
//    _sheet = replace_Row(_sheet, row)
    ???
  }

  protected final def update_row(row: ROW) {
    // _sheet = update_Row(_sheet, row)
    ???
  }
}
