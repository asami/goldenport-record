package org.goldenport.record.util

import java.util.Locale
import org.goldenport.record.util.{I18NString => RI18NString}

/*
 * @since   May. 24, 2017
 *  version May. 24, 2017
 * @version Aug.  1, 2017
 * @author  ASAMI, Tomoharu
 */
case class I18NString(
  en: String,
  ja: String,
  byLocale: List[(Locale, String)],
  parameters: IndexedSeq[Any]
) {
  def update(f: String => String): I18NString = I18NString(
    f(en), f(ja), byLocale.map(x => (x._1, f(x._2))), parameters
  )

  def get(locale: Locale): String = LocaleUtils.get(byLocale)(locale) orElse _get_japanese(locale) getOrElse en

  private def _get_japanese(locale: Locale) =
    if (locale.getLanguage == Locale.JAPANESE.getLanguage)
      Some(ja)
    else
      None
}

object I18NString {
  def apply(s: String): I18NString = I18NString(s, s, Nil, Vector.empty)
  def apply(en: String, ja: String): I18NString = I18NString(en, ja, Nil, Vector.empty)
  def apply(en: String, ja: String, params: IndexedSeq[Any]): I18NString = I18NString(en, ja, Nil, params)

  def create(locale: Locale, s: String): I18NString = {
    val a = if (LocaleUtils.isEnglish(locale) || LocaleUtils.isJapanese(locale))
      List.empty
    else
      List((locale, s))
    I18NString(s, s, a, Vector.empty)
  }
}
