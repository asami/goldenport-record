package org.goldenport.record.v2

import scalaz._, Scalaz._

/*
 * Derived from SqlSchema.
 * 
 * @since   Jan.  5, 2013
 *  version Mar. 13, 2013
 *  version Apr.  9, 2013
 * @version May. 10, 2013
 * @author  ASAMI, Tomoharu
 */
case class ActionContext(
  in: Record,
  outs: Map[String, Record] = Map.empty, // TODO sort by key
  properties: Map[String, Seq[Any]] = Map.empty
) {
  require (in != null && (in.fields == Nil || in.opaque != null), "in and record = " + in)

  def outRecords: Seq[Record] = { // TODO sort problem (use key)
    val a = outs.values.toList
//    println("ActionContext in = " + in)
    println("ActionContext#outRecords outs = " + a)
    a.map(x => if (x.opaque == null) x.copy(opaque = in.opaque) else x)
  }

  def addUploadFiles(columnName: String, files: Seq[UploadFile]): ActionContext = {
    val a = for (f <- files) yield {
      val b = outs.get(f.key) match {
        case Some(s) => s ::+ columnName -> f.uri
        case None => create_value(List(columnName -> f.uri))
      }
      (f.key, b)
    }
    println("ActionContext#addUploadFiles(%s) = %s / %s".format(columnName, files, a))
    copy(outs = outs ++ a)
  }

  protected def create_value(data: Seq[(String, Any)]): Record = {
    Record(Field.create(data),
           principal = in.principal,
           timestamp = in.timestamp,
           opaque = in.opaque)
  }

  def setMainId(id: Any): ActionContext = {
    setProperty(ActionContext.KEY_MAIN_ID, id)
  }

  def setReferenceIds(ids: Seq[Any]): ActionContext = {
    setProperty(ActionContext.KEY_REFERENCE_IDS, ids)
  }

  def setReferenceIdsByAttributeName(name: String): ActionContext = {
//    println("ActionContext#setReferenceIdsByAttributeName(%s) = %s".format(name, in.get(name)))
    in.get(name) match {
      case Some(s) => setReferenceIds(s)
      case None => setReferenceIds(Nil)
    }
  }

  def setProperty(key: String, value: Any): ActionContext = {
    value match {
      case x: Seq[_] => copy(properties = properties + (key -> x))
      case x => copy(properties = properties + (key -> List(value)))
    }
  }

  def getProperty(key: String): Option[Seq[Any]] = {
    properties.get(key)
  }

  def getMainIds: Seq[Any] = {
    getProperty(ActionContext.KEY_MAIN_ID) match {
      case Some(s) => s
      case None => sys.error("Check inserted id. In case of auto id, use useId in SqlSchema.")
    }
  }

  def getRefrenceIds: Seq[Any] = {
    getProperty(ActionContext.KEY_REFERENCE_IDS) match {
      case Some(s) => s
      case None => sys.error("KEY_REFERENCE_IDS does not set in ActionContext, use setReferenceIds in ActionContext")
    }
  }

  def getInputFiles(attrname: String): Seq[InputFile] = {
    in.inputFiles.filter(_.key == attrname)
  }

  def buildMainReference(maincolumnname: String, referencecolumnname: String): ActionContext = {
    val a = for (m <- getMainIds; r <- getRefrenceIds) yield {
      create_value(List(maincolumnname -> m,
                        referencecolumnname -> r))
    }
//    log_trace("ActionContext#buildMainReference = " + a)
    setOuts(a)
  }

  def updateIn(fs: Seq[(String, Any)]): ActionContext = {
//    println("ActionContext = " + fs)
    val a = copy(in = in.update(fs))
//    println("ActionContext result = " + a)
    a
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

