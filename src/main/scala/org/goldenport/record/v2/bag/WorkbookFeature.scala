package org.goldenport.record.v2.bag

import org.goldenport.bag.ChunkBag

/*
 * @since   Oct.  8, 2015
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
trait WorkbookFeature { self: RecordBag =>
  type WORKBOOK
  def bag: ChunkBag

  sealed trait ContentState {
    def workbook: WORKBOOK
    def ensureInternal: ContentState
    def ensureExternal: ContentState
    def touchWorkbook(): ContentState = InternalState(workbook)
    def dispose(): Unit = dispose_Workbook(workbook)
  }

  case object NewState extends ContentState {
    lazy val workbook = new_Workbook()
    def ensureInternal: ContentState = this
    def ensureExternal: ContentState = this
  }

  case class SyncState(workbook: WORKBOOK) extends ContentState {
    def ensureInternal: ContentState = this
    def ensureExternal: ContentState = this
  }

  case class ExternalState(workbook: WORKBOOK) extends ContentState {
    def ensureInternal: ContentState = SyncState(load_Workbook())
    def ensureExternal: ContentState = this
  }

  case class InternalState(workbook: WORKBOOK) extends ContentState {
    def ensureInternal: ContentState = this
    def ensureExternal: ContentState = {
      save_Workbook(workbook)
      SyncState(workbook)
    }
  }

  protected def new_Workbook(): WORKBOOK

  protected def load_Workbook(): WORKBOOK

  protected def save_Workbook(workbook: WORKBOOK): Unit

  protected def dispose_Workbook(workbook: WORKBOOK): Unit

  private var _state: ContentState = {
    if (bag.isEmpty)
      new_state()
    else
      SyncState(load_Workbook())
  }

  protected def new_state(): ContentState = NewState

  def flush() {
    workbook_ensure_external()
  }

  def chunkBag = {
    workbook_ensure_external()
    bag
  }

  protected def to_workbook: WORKBOOK = {
    workbook_ensure_internal()
    _state.workbook
  }

  protected def set_workbook(workbook: WORKBOOK) {
    _state = InternalState(workbook)
  }

  protected def workbook_touch() {
    _state = _state.touchWorkbook
  }

  protected def workbook_ensure_internal() {
    _state = _state.ensureInternal
  }

  protected def workbook_ensure_external() {
    _state = _state.ensureExternal
  }

  protected def workbook_dispose_state() {
    _state.dispose()
  }
}
