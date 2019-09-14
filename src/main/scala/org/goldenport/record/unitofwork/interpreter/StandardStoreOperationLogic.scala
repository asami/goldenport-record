package org.goldenport.record.unitofwork.interpreter

import org.goldenport.exception.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.v2.unitofwork.interpreter.JournalStoreOperationLogic

/*
 * @since   Sep. 15, 2018
 *  version Sep. 15, 2018
 * @version Sep. 13, 2019
 * @author  ASAMI, Tomoharu
 */
class StandardStoreOperationLogic() extends JournalStoreOperationLogic {
  protected def generate_id(store: Store): Store.Id = RAISE.notImplementedYetDefect
  protected def get_item_store(store: Store): Option[JournalStoreOperationLogic.ItemStore] = RAISE.notImplementedYetDefect
  def get(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def getSync(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def getShare(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def getExclusive(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def gets(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def getsSync(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def getsShare(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def getsExclusive(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def select(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
  def selectSync(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
  def selectShare(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
  def selectExclusive(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
}

object StandardStoreOperationLogic {
}
