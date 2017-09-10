package org.goldenport.record.v2.bag

import scalaz.concurrent.Task
import scalaz.stream._
import scalax.io._
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.Platform.codec.UTF8
import org.goldenport.record.v2._
import org.goldenport.record.v2.bag.RecordBag._
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.bag._

/*
 * @since   Sep. 21, 2015
 *  version Oct. 27, 2015
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
class JsonBag(
  val bag: ChunkBag = new BufferFileBag(),
  val strategy: RecordBag.Strategy = RecordBag.Strategy.plainAuto,
//  val sheetName: Option[String] = None,
  override val name: String = "data"
) extends RecordBag with WorkbookFeature with SheetFeature {
  import JsonBag._
  type WORKBOOK = JsValue // JsObject or JsArray
  type SHEET = JsArray
  override def filenameSuffix = Some("json")
  override def mimetype: String = Strings.mimetype.application_json

  protected def generate_Schema(): Option[Schema] = {
    None // TODO
  }

  def dispose() {
    workbook_dispose_state()
    bag.dispose()
  }

  protected def new_Workbook() = JsNull

  private def _reconstruct_workbook() {
    val a = to_workbook match {
      case JsNull => _reconstruct_workbook_whole()
      case JsArray(array) => _reconstruct_workbook_whole()
      case JsObject(fields) => _reconstruct_workbook_object(fields)
      case m => throw new IllegalArgumentException("Unrecoganized state: $m")
    }
    set_workbook(a)
  }

  private def _reconstruct_workbook_whole() = {
    val a = get_sheets.map(x => x.name -> x.sheet)
    JsObject(a)
  }

  private def _reconstruct_workbook_object(fields: Seq[(String, JsValue)]) = {
    val sheets: Map[String, RecordBagSheet[JsArray]] = get_sheet_map
    case class Z(
      m: Map[String, RecordBagSheet[JsArray]],
      z: Vector[(String, JsValue)] = Vector.empty
    ) {
      def apply(x: (String, JsValue)) = {
        val key = x._1
        m.get(key) match {
          case Some(s) =>
            val a = m - key
            val b = z :+ (key -> s.sheet)
            Z(a, b)
          case None => copy(z = z :+ x)
        }
      }
    }
    val a: Seq[(String, JsValue)] = {
      val Z(m, z) = fields.foldLeft(Z(sheets))(_.apply(_))
      // fields.toList.span(x => keys.contains(x._1)) match {
      //   case (xs, Nil) => xs :+ (name -> JsValueUtils.toJsValue(to_sheet))
      //   case (xs, y :: ys) => xs ::: (name -> JsValueUtils.toJsValue(to_sheet)) :: ys
      // }
      z ++ m.toVector.map {
        case (k, v) => k -> v.sheet
      }
    }
    JsObject(a)
  }

  // private def _reconstruct_workbook_whole() = {
  //   sheetName match {
  //     case Some(name) => JsValueUtils.toJsValue(Record.data(name -> to_sheet))
  //     case None => JsValueUtils.toJsValue(to_sheet)
  //   }
  // }

  // private def _reconstruct_workbook_object(fields: Seq[(String, JsValue)]) = {
  //   val a: List[(String, JsValue)] = sheetName match {
  //     case Some(name) =>
  //       fields.toList.span(_._1 != name) match {
  //         case (xs, Nil) => xs :+ (name -> JsValueUtils.toJsValue(to_sheet))
  //         case (xs, y :: ys) => xs ::: (name -> JsValueUtils.toJsValue(to_sheet)) :: ys
  //       }
  //     case None => ???
  //   }
  //   JsObject(a)
  // }

  // private def _reconstruct_sheet(j: JsValue) {
  //   val a = (sheetName, j) match {
  //     case (_, array: JsArray) => _reconstruct_sheet_array(array)
  //     case (Some(name), JsObject(fields)) => _reconstruct_sheet_object(name, fields)
  //     case (None, JsObject(fields)) => _reconstruct_sheet_object(fields.headOption)
  //     case (_, _) => throw new IllegalArgumentException("Unavailable json: $j")
  //   }
  //   set_sheet(RecordSet(a))
  // }

  // private def _reconstruct_sheet_array(array: JsArray) = {
  //   RecordUtils.js2records(array)
  // }

  // private def _reconstruct_sheet_object(name: String, fields: Seq[(String, JsValue)]): Seq[Record] = {
  //   _reconstruct_sheet_object(fields.find(_._1 == name))
  // }

  // private def _reconstruct_sheet_object(field: Option[(String, JsValue)]): Seq[Record] = {
  //   field match {
  //     case Some(s) => RecordUtils.js2records(s._2)
  //     case None => Nil
  //   }
  // }

  protected def load_Sheets(): List[RecordBagSheet[JsArray]] = {
    def makesheet(sheetname: String, js: JsArray): RecordBagSheet[JsArray] = {
      RecordBagSheet(
        sheetname,
        js,
        View(strategy, this, sheetname, js)
      )
    }
    val sheets = to_workbook match {
      case array: JsArray => List(makesheet(name, array))
      case JsObject(fields) =>
        fields.toList.flatMap {
          case (k, v) => v match {
            case array: JsArray => Some(makesheet(k, array))
            case _ => None
          }
        }
      case x => throw new IllegalStateException(s"Unavailable json: $x")
    }
    sheets
  }

  // private def _reconstruct_sheet(j: JsValue) {
  //   val sheets = load_Sheets()
  //   set_sheets(sheets)
  // }

  protected def save_Workbook(workbook: JsValue) {
    _reconstruct_workbook()
    bag.write(Json.stringify(to_workbook))
  }

  protected def load_Workbook(): JsValue = {
    val j = Json.parse(bag.toText)
//    _reconstruct_sheet(j)
    j
  }

  protected def dispose_Workbook(workbook: JsValue) {
  }

  def withSchema(s: Schema) = {
//    new JsonBag(bag, codec, s).asInstanceOf[this.type]
    ???
  }

  def openAppender(): RecordBag.Appender = ???
  def write(rs: java.sql.ResultSet): Unit = ???
  def writeRecords(rs: Iterator[Record]): Unit = ???
  def writeRecordsWithSchema(rs: Iterator[Record]): Unit = ???

  override def getViews: List[RecordBagView] = {
    workbook_ensure_internal()
    get_views
  }
}

object JsonBag {
  def create(bag: ChunkBag, strategy: RecordBag.Strategy): JsonBag = {
    new JsonBag(bag, strategy)
  }

  def create(bag: ChunkBag, strategy: RecordBag.Strategy, name: String): JsonBag = {
    new JsonBag(bag, strategy, name = name)
  }

  def fromJson(s: String): JsonBag = {
    val bag = new BufferFileBag()
    bag.write(s)
    create(bag, RecordBag.Strategy.virtualAuto)
  }

  case class View(
    strategy: RecordBag.Strategy,
    bag: JsonBag,
    name: String,
    array: JsArray
  ) extends RecordBagView {
    def getSchema: Option[Schema] = None
    def chunkBag = bag.chunkBag

    def recordsR: Process[Task, Record] = {
      Process.emitAll(RecordUtils.js2records(array))
    }
  }
}
