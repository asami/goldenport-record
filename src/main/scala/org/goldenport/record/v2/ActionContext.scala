package org.goldenport.record.v2

import scalaz._, Scalaz._

/*
 * Derived from SqlSchema.
 * 
 * @since   Jan.  5, 2013
 * @version Mar.  3, 2013
 * @author  ASAMI, Tomoharu
 */
case class ActionContext(
  in: Record,
  outs: Map[String, Record] = Map.empty,
  params: Map[String, Seq[Any]] = Map.empty
) {
  def outRecords: Seq[Record] = outs.values.toList

  def addUploadFiles(columnName: String, files: Seq[UploadFile]): ActionContext = {
    val a = for (f <- files) yield {
      val b = outs.get(f.key) match {
        case Some(s) => s ::+ columnName -> f.uri
        case None => create_value(List(columnName -> f.uri))
      }
      (f.key, b)
    }
//    log_trace("ActionContext#addUploadFiles(%s) = %s / %s".format(columnName, files, a))
    copy(outs = outs ++ a)
  }

  protected def create_value(data: Seq[(String, Any)]): Record = {
    Record(Field.create(data),
           principal = in.principal,
           timestamp = in.timestamp)
  }

  def setMainId(id: Any): ActionContext = {
    setParam(ActionContext.KEY_MAIN_ID, id)
  }

  def setReferenceIds(ids: Seq[Any]): ActionContext = {
    setParam(ActionContext.KEY_REFERENCE_IDS, ids)
  }

  def setParam(key: String, value: Any): ActionContext = {
    value match {
      case x: Seq[Any] => copy(params = params + (key -> x))
      case x => copy(params = params + (key -> List(value)))
    }
  }

  def getParam(key: String): Option[Seq[Any]] = {
    params.get(key)
  }

  def getMainIds: Seq[Any] = {
    getParam(ActionContext.KEY_MAIN_ID) match {
      case Some(s) => s
      case None => sys.error("???")
    }
  }

  def getRefrenceIds: Seq[Any] = {
    getParam(ActionContext.KEY_REFERENCE_IDS) match {
      case Some(s) => s
      case None => sys.error("???")
    }
  }

  def buildMainReference(maincolumnname: String, referencecolumnname: String): ActionContext = {
    val a = for (m <- getMainIds; r <- getRefrenceIds) yield {
      create_value(List(maincolumnname -> m,
                        referencecolumnname -> r))
    }
//    log_trace("ActionContext#buildMainReference = " + a)
    setOuts(a)
  }

  def setOuts(rs: Seq[Record]): ActionContext = {
    val a = Stream.from(1).map(_.toString) zip rs
    copy(outs = Map.empty ++ a)
  }
}


object ActionContext {
  val KEY_MAIN_ID = "main_id"
  val KEY_REFERENCE_IDS = "reference_ids"

  val empty = ActionContext(Record(Nil))
}

