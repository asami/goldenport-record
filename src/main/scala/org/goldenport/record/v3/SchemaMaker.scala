package org.goldenport.record.v3

import java.sql.ResultSet
import org.goldenport.util.ListUtils
import org.goldenport.record.v2.{DataType, MZeroOne, MZeroMore}
import org.goldenport.record.v3.sql.SqlUtils

/*
 * See org.goldenport.record.v3.sql
 * 
 * @since   Oct. 31, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
class SchemaMaker() {
  import SchemaMaker._

  def make(p: ResultSet): Schema = {
    Schema.from(SqlUtils.makeSchema(p))
  }

  def make(ps: Seq[IRecord]): Schema = {
    case class Z(columns: List[Slot] = Nil) {
      // TODO MOne, MOneMore
      def r = Schema(columns.map(_.column).toVector)
      def +(rhs: IRecord): Z = {
        case class ZZ(r: List[Slot] = Nil) {
          def +(f: Field): ZZ = {
            val slot = _to_slot(rhs, f)
            val (ls, x, rs) = _split(r, slot)
            ZZ(ls ++ List(_merge(x, slot)) ++ rs)
          }
        }
        Z(rhs.fields./:(ZZ(columns))(_+_).r)
      }
    }
    ps./:(Z())(_+_).r
  }

  private def _to_slot(rec: IRecord, f: Field): Slot =
    ListUtils.split3((_: Field) == f)(rec.fields) match {
      case (ls, Nil, rs) => Slot(_to_column(f), ls.map(_.key.name), rs.map(_.key.name))
      case (ls, cs, rs) => Slot(_to_column(cs.head), ls.map(_.key.name), rs.map(_.key.name))
    }

  private def _to_column(f: Field): Column = {
    val datatype = DataType.guessSeq(f.value.asVector)
    val multiplicity = f.value match {
      case EmptyValue => MZeroOne
      case m: SingleValue => MZeroOne
      case m: MultipleValue => MZeroMore
    }
    Column(f.key.name, datatype, multiplicity)
  }

  private def _split(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    ListUtils.split3((_: Slot).isMatch(s))(ss) match {
      case (ls, Nil, rs) => _split_guess(ss, s)
      case (ls, cs, rs) => (ls, cs.head, rs)
    }

  private def _split_guess(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    s.lefts.reverse match {
      case Nil => _split_guess2(ss, s)
      case x :: xs => _split_guess_left(ss, s, x, xs)
    }

  private def _split_guess2(ss: List[Slot], s: Slot): (List[Slot], Slot, List[Slot]) =
    s.rights match {
      case Nil => (Nil, s, Nil)
      case x :: xs => _split_guess_right(ss, s, x, xs)
    }

  private def _is_slot(name: String)(slot: Slot): Boolean = slot.isMatch(name)

  private def _split_guess_left(ss: List[Slot], s: Slot, name: String, candidates: List[String]): (List[Slot], Slot, List[Slot]) =
    ListUtils.split3(_is_slot(name))(ss) match {
      case (Nil, Nil, _) => (Nil, s, Nil)
      case (ls, Nil, _) => candidates match {
        case Nil => _split_guess2(ss, s)
        case x :: xs => _split_guess_left(ss, s, x, xs)
      }
      case (ls, cs, rs) => (ls ++ cs, s, rs)
    }
    // ss.toList.span(_.isMatch(name)) match {
    //   case (Nil, Nil) => (Nil, s, Nil)
    //   case (Nil, rs) => (List(rs.head), s, rs.tail)
    //   case (ls, Nil) => candidates match {
    //     case Nil => _split_guess2(ss, s)
    //     case x :: xs => _split_guess_left(ss, s, x, xs)
    //   }
    //   case (ls, rs) => (ls :+ rs.head, s, rs.tail)
    // }

  private def _split_guess_right(ss: List[Slot], s: Slot, name: String, candidates: List[String]): (List[Slot], Slot, List[Slot]) =
    ListUtils.split3(_is_slot(name))(ss) match {
      case (Nil, Nil, _) => (Nil, s, Nil)
      case (ls, Nil, _) => candidates match {
        case Nil => (ls, s, Nil)
        case x :: xs => _split_guess_right(ss, s, x, xs)
      }
      case (ls, cs, rs) => (ls, s, cs ++ rs)
    }
    // ss.toList.span(_.isMatch(name)) match {
    //   case (Nil, Nil) => (Nil, s, Nil)
    //   case (Nil, rs) => (Nil, s, rs)
    //   case (ls, Nil) => candidates match {
    //     case Nil => (ls, s, Nil)
    //     case x :: xs => _split_guess_right(ss, s, x, xs)
    //   }
    //   case (ls, rs) => (ls, s, rs)
    // }

  private def _merge(z: Slot, n: Slot): Slot = {
    case class Z(xs: Vector[String]) {
      def r = xs.toList
      def +(rhs: String) = if (r.contains(rhs)) this else Z(xs :+ rhs)
    }
    val l = n.lefts./:(Z(z.lefts.toVector))(_+_).r
    val r = n.rights./:(Z(z.rights.toVector))(_+_).r
    Slot(n.column, l, r)
  }
}

object SchemaMaker {
  case class Slot(column: Column, lefts: List[String], rights: List[String]) {
    def isMatch(p: Slot): Boolean = isMatch(p.column.name)
    def isMatch(p: String): Boolean = column.name == p
  }

  def make(p: ResultSet): Schema = {
    new SchemaMaker().make(p)
  }
}
