package org.goldenport.record

import scala.collection.immutable.Stream
import scala.collection.immutable.Stream.Empty
import scala.collection.mutable.ArrayBuffer
import java.sql.ResultSet
import com.asamioffice.goldenport.text.UJson
import org.goldenport.util._
import org.goldenport.record.query._
import org.goldenport.atom._

/**
 * derived from org.goldenport.g3.message.
 * 
 * @since   Jun.  9, 2010
 *  version Jul.  3, 2011
 * @version Feb. 14, 2012
 * @author  ASAMI, Tomoharu
 */
// XXX RecordSet should not be Stream itself.
// XXX G3Context and G3AgentContext
abstract class RecordSet extends Stream[Record] {
  def records: Stream[Record] = this
  var id: String = ""
  var title: String = ""
  var updated: VDateTime = VDateTime()

  // Object
  override def toString() = {
    getClass.getSimpleName // XXX
  }

  //
  def takeToString(length: Int): String = {
    val rs = take(length + 1)
    if (rs.isEmpty) "[Empty]"
    else if (rs.length > length) rs.mkString("[", ",", ", ...]")
    else rs.mkString("[", ",", "]")
  }

  def lengthHint: Option[Int] = {
    val hint_length = 1024
    val rs = take(hint_length + 1)
    if (rs.length <= hint_length) Some(rs.length)
    else None
  }

  // caution: force records in memory
  def isValid: Boolean = {
    !records.exists(r => !r.isValid)
  }

  //
  private def atom_id = AtomId(id)

  private def atom_title = AtomTitle(title)

  private def atom_updated = AtomUpdated(updated)

  private def atom_subtitle = None

  private def atom_categories = Nil

  private def atom_authors = Nil

  private def atom_contributers = Nil

  private def atom_rights = None

  private def atom_icon = None

  private def atom_logo = None

  private def atom_links = Nil

  private def atom_generator = None

  private def atom_extensionElements = Nil

  private def atom_entries = map(_.toAtomEntry)

  def toAtomFeed: AtomFeed = {
    new AtomFeed() {
      atomId = atom_id
      atomTitle = atom_title
      atomUpdated = atom_updated
      atomSubtitle = atom_subtitle
      atomCategories ++= atom_categories
      atomAuthors ++= atom_authors
      atomContributers ++= atom_contributers
      atomRights = atom_rights
      atomIcon = atom_icon
      atomLogo = atom_logo
      atomLinks ++= atom_links
      atomGenerator = atom_generator
      atomExtensionElements ++= atom_extensionElements
      atomEntries ++= atom_entries
    }
  }

//  def toJson: String = {
//    jsonString(this.records)
//  }

  private val json_chunk_size = 1000

  def toJson: String = toJson(0, json_chunk_size)
  def toJson1: String = toJson(1, json_chunk_size)

/*
  def toJson(cmd: RestCommand): String = {
    cmd.content match {
      case Some(r: Record) => {
        val start = r.asInt('start, 0)
        val limit = r.asInt('limit, json_chunk_size)
        toJson(start, limit)
      }
      case _ => toJson
    }
  }
*/
  def toJson(start: Int, maxresults: Int): String = {
    require (start >= 0, maxresults > 0)
    val (result, total) = json_to_seq(start, maxresults)
    to_json(result, start, maxresults, total)
  }

  def toJson1(startindex: Int, maxresults: Int): String = {
    require (startindex >= 1, maxresults > 0)
    val (result, total) = json_to_seq(startindex - 1, maxresults)
    to_json(result, startindex - 1, maxresults, total)
  }

  private def json_to_seq(startindex: Int, maxresults: Int): (Seq[Record], Int) = {
    val start = records.drop(startindex)
    val result = start.take(maxresults)
    val total = if (result.length < maxresults) {
      startindex + result.length
    } else {
      val last = start.drop(json_chunk_size)
      if (last.isEmpty) records.length
      else json_chunk_size
    }
    (result, total)
  }

  private def to_json(result: Seq[Record], start: Int, maxresults: Int, total: Int) = {
    UJson.optionTuples2Json(
      Some(("id", id)),
      Some(("title", title)),
      Some(("updated", updated)),
      Some(("content", result.map(_.toJson))),
      Some(("length", result.length)),
      Some(("start-index", start + 1)),
      Some(("start", start)),
      Some(("max-results", maxresults)),
      Some(("limit", maxresults)),
      Some(("total-length", total)),
      Some(("success", true)))
  }

  def toCsv: String = {
    toCsv()
  }

  def toCsv(newline: String = "\n"): String = {
    map(_.toCsv).mkString("", newline, newline)
  }
}

class ListRecordSet(val list: List[Record]) extends RecordSet {
  private var _tail1: Option[Tail] = None

  // Object
  override def toString() = takeToString(3)

  //
  def tailDefined: Boolean = _tail1.isDefined
  override def isEmpty: Boolean = list.isEmpty
  override def head: Record = list.head
  override def tail: Stream[Record] = {
    _tail1 match {
      case Some(tail) => tail
      case None if list.length == 1 => Empty
      case None => _tail1 = Some(new Tail(1));_tail1.get
    }
  }

  class Tail(val index: Int) extends Stream[Record] {
    private var _tail2: Option[Tail] = None

    def tailDefined: Boolean = _tail2.isDefined
    override def isEmpty: Boolean = false
    override def head: Record = list(index)
    override def tail: Stream[Record] = {
      val next = index + 1
      _tail2 match {
        case Some(tail2) => tail2
        case None if list.length == next => Empty
        case None => _tail2 = Some(new Tail(next));_tail2.get
      }
    }
  }
}

abstract class StreamRecordSet extends RecordSet {
  private var _record: Option[Record] = None
  private var _tail: Option[Stream[Record]] = None

  def tailDefined: Boolean = _tail.isDefined

  override def isEmpty: Boolean = {
    load_record
    _record.isEmpty
  }

  override def head: Record = {
    load_record
    _record match {
      case Some(record) => record
      case None => throw new NoSuchElementException()
    }
  }

  override def tail: Stream[Record] = {
    load_record
    if (_record.isDefined) _tail.get
    else throw new UnsupportedOperationException()
  }

  private def load_record {
    if (_tail.isDefined) return
    load_Record() match {
      case Some((record, tail)) => {
        _record = Some(record)
        _tail = Some(tail)
      }
      case None => {
        _record = None
        _tail = Some(Empty)
      }
    }
  }

  protected def load_Record(): Option[(Record, Stream[Record])]
}

// XXX move from StreamRecordSet
abstract class RecordSetStream extends Stream[Record] {
  private var _record: Option[Record] = None
  private var _tail: Option[Stream[Record]] = None

  def tailDefined: Boolean = _tail.isDefined

  override def isEmpty: Boolean = {
    load_record
    _record.isEmpty
  }

  override def head: Record = {
    load_record
    _record match {
      case Some(record) => record
      case None => throw new NoSuchElementException()
    }
  }

  override def tail: Stream[Record] = {
    load_record
    if (_record.isDefined) _tail.get
    else throw new UnsupportedOperationException()
  }

  private def load_record {
    if (_tail.isDefined) return
    load_Record() match {
      case Some((record, tail)) => {
        _record = Some(record)
        _tail = Some(tail)
      }
      case None => {
        _record = None
        _tail = Some(Empty)
      }
    }
  }

  protected def load_Record(): Option[(Record, Stream[Record])]
}

class ResultSetRecordSet(val result: ResultSet)(implicit context: RecordContext) extends StreamRecordSet {
  protected def load_Record(): Option[(Record, Stream[Record])] = {
    if (result.next()) {
      Some((ResultSetRecord(result)(context), 
           new ResultSetRecordSet(result)(context)))
    } else {
      None
    }
  }
}

class TransformRecordSet(val in: Stream[Record])(val transformer: Record => Record)(implicit context: RecordContext) extends StreamRecordSet {
  protected def load_Record() = {
    if (!in.isEmpty) {
      Some((transformer(in.head),
            new TransformRecordSet(in.tail)(transformer)(context)))
    } else {
      None
    }
  }
}

class RecordSchemaRecordSet(val in: Stream[Record], val schema: RecordSchema, val ctx: RecordContext) extends StreamRecordSet {
/*
  private def transform_record(r: Record): Record = {
    val fields = for (i <- 0 until schema.fields.length) yield {
      val f = schema.fields(i)
      r.get(f.name) match {
        case Some(d) => {
          val data = f.normalize(d) match {
            case Right(d) => d
            case Left(e) => new IllegalFormatRecordFieldException(e)
          }
          Option(f.name, data)
        }
        case None => {
          if (f.isAuto) {
            None
          } else if (f.multiplicity != ZeroOne && f.multiplicity != ZeroMore) {
            Option(f.name, new MissingFieldRecordFieldException)
          } else {
            None
          }
        }
      }
    }

    val result = new Record(fields.flatten)
    result.schema = Some(schema)
    result
  }
*/
  protected def load_Record() = {
    if (!in.isEmpty) {
      Some((schema.validate(in.head, ctx),
           new RecordSchemaRecordSet(in.tail, schema, ctx)))
    } else {
      None
    }
  }
}

// 2010-09-09
class RecordQueryRecordSet(val in: Stream[Record], val query: RecordQuery)(implicit context: RecordContext) extends StreamRecordSet {
  private def transform_record(r: Record): Record = {
    if (query.slots.exists(_.isInstanceOf[AllFieldSlot])) {
      r
    } else {
      val fields = query.slots.flatMap {
        case r: RecordQueryRealSlot => r.columnNames(query.context)
        case v: RecordQueryVirtualSlot => Nil
      }
      new Record(
        for (i <- 0 until fields.length) yield {
          (fields(i), r.nth(i))
        }
      )
    }
  }

  protected def load_Record() = {
    if (!in.isEmpty) {
      Some((transform_record(in.head),
            new RecordQueryRecordSet(in.tail, query)(context)))
    } else {
      None
    }
  }
}

// 2010-10-09
class RestRecordSet(val in: Stream[Record]) extends StreamRecordSet {
  private def normalize_data(v: AnyRef) = {
    val string = v.toString
    try {
      string.toLong
    } catch {
      case _ => {
        try {
          string.toDouble
        } catch {
          case _ => string
        }
      }
    }
  }

  private def transform_record(r: Record): Record = {
    Record(
      r.fields map {
        case (k, v) => (k, normalize_data(v))
      }
    )
  }

  protected def load_Record() = {
    if (!in.isEmpty) {
      Some((transform_record(in.head),
            new RestRecordSet(in.tail)))
    } else {
      None
    }
  }
}

// 2010-10-02
class ConcatRecordSet(val recordSets: List[RecordSet]) extends StreamRecordSet {
  protected def load_Record() = {
    _load_record(recordSets)
  }

  private def _load_record(rs: List[RecordSet]): Option[(Record, Stream[Record])] = {
    if (rs.isEmpty) None
    else if (rs.head.isEmpty) {
      if (rs.length == 1) None
      else _load_record(rs.tail)
    } else {
      Some(rs.head.head,
            new ConcatRecordSetStream(rs.head.tail, rs.tail))
    }
  }

  class ConcatRecordSetStream(val stream: Stream[Record], val recordSets: List[RecordSet]) extends RecordSetStream {
    protected def load_Record(): Option[(Record, Stream[Record])] = {
      if (stream.isEmpty) {
        val rss = _normalize_record_sets
        if (rss.isEmpty) {
          None
        } else {
          if (rss.tail.isEmpty) {
            Some((rss.head.head, new ConcatRecordSetStream(rss.head.tail, Nil))) // XXX
          } else {
            Some((rss.head.head, new ConcatRecordSetStream(rss.head.tail, rss.tail)))
          }
        }
      } else {
        val head = stream.head
        val tail = stream.tail
        if (tail.isEmpty) {
          val rss = _normalize_record_sets
          if (rss.isEmpty) {
            Some((head, Empty))
          } else {
            Some((head, new ConcatRecordSetStream(Empty, rss)))
          }
        } else {
          Some((head, new ConcatRecordSetStream(tail, recordSets)))
        }
      }
    }

    private def _normalize_record_sets = {
      recordSets.flatMap(rs => 
        if (rs.isEmpty) None
        else Some(rs)
      )
    }
  }
}

abstract class EntityReferenceRecordSet(val uri: String) extends RecordSet {
  // XXX
}

object RecordSet {
  def apply(records: Record*) = {
    new ListRecordSet(records.toList)
  }

  def apply(records: List[Record]) = {
    new ListRecordSet(records.toList)
  }

  def apply(data: Iterable[Map[Symbol, Any]]) = {
    val records = for (d <- data) yield {
      Record(d)
    }
    new ListRecordSet(records.toList)
  }

  def apply(rs: ResultSet)(implicit ctx: RecordContext) = {
    new ResultSetRecordSet(rs)(ctx)
  }
/*
  def from(records: Product*) = {
    val result = new ArrayBuffer[Record]
    for (r: Product <- records) yield {
      r match {
        case rd: Record => result += rd
        case p: Product => { // Tupple
          val rr = new ArrayBuffer[(Symbol, AnyRef)] // Any -> AnyRef
          for (i <- 0 until p.productArity) {
            p.productElement(i) match {
              case (key: Symbol, value: AnyRef) => {
                rr += ((key, value))
              }
              case (key: String, value: AnyRef) => {
                rr += ((Symbol(key), value))
              }
            }
          }
          Record(rr.toList)
        }
      }
    }
    new ListRecordSet(result.toList)
  }
*/
  val empty = new ListRecordSet(Nil)
}
