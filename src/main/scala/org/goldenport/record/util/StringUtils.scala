package org.goldenport.record.util

import org.goldenport.Strings

/*
 * @since   Jan.  1, 2014
 * @version Jan.  1, 2015
 * @author  ASAMI, Tomoharu
 */
object StringUtils {
  /*
   * LTSV
   */
  def tokeyvalue(s: String, ds: String = ":"): (String, String) = {
    val i = s.indexOf(ds)
    if (i == -1) (s, "")
    else (s.substring(0, i).trim, s.substring(i + 1))
  }

  def ltsv2seq(s: String): Seq[(String, String)] = {
    Strings.totokens(s, "\t") map { x =>
      tokeyvalue(x)
    }
  }

  def ltsv2map(s: String): Map[String, String] = {
    val a = Strings.totokens(s, "\t") map { x =>
      tokeyvalue(x)
    }
    Map.empty ++ a
  }

  def normalizeLtsvLine(s: String): String = {
    @annotation.tailrec
    def clearlastnewlines(s: String): String = {
      s.lastOption match {
        case Some(c) if c == '\n' || c == '\r' => clearlastnewlines(s.init)
        case _ => s
      }
    }
    if (s.contains("\n") | s.contains("\r")) {
      val s1 = clearlastnewlines(s)
      s1.replace("\n", """\n""").replace("\r", """\r""")
    } else {
      s
    }
  }
}
