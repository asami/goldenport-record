package org.goldenport.record.util

import java.util.Locale

/*
 * @since   Aug.  1, 2017
 * @version Aug.  1, 2017
 * @author  ASAMI, Tomoharu
 */
object LocaleUtils {
  lazy val LANGUAGE_ENGLISH = Locale.ENGLISH.getLanguage
  lazy val LANGUAGE_JPAPNESE = Locale.JAPANESE.getLanguage

  def get(master: Seq[(Locale, String)])(locale: Locale): Option[String] = {
    (Option(locale.getLanguage), Option(locale.getCountry), Option(locale.getVariant)) match {
      case (None, None, None) => None
      case (Some(l), None, None) => getByLanguage(master)(l)
      case (Some(l), Some(c), None) => getByLanguageCountry(master)(l, c)
      case (Some(l), Some(c), Some(v)) => getByLanguageCountryVariant(master)(l, c, v)
      case _ => None
    }
  }

  def getByLanguage(master: Seq[(Locale, String)])(l: String): Option[String] =
    master.find(x => x._1.getLanguage == l).map(_._2)

  def getByLanguageCountry(master: Seq[(Locale, String)])(l: String, c: String): Option[String] =
    master.find(x => x._1.getLanguage == l && x._1.getCountry == c).map(_._2) orElse getByLanguage(master)(l)

  def getByLanguageCountryVariant(master: Seq[(Locale, String)])(l: String, c: String, v: String): Option[String] =
    master.find(x => x._1.getLanguage == l && x._1.getCountry == c && x._1.getVariant == v).map(_._2) orElse getByLanguageCountry(master)(l, c)

  def isEnglish(locale: Locale): Boolean = locale.getLanguage == LANGUAGE_ENGLISH
  def isJapanese(locale: Locale): Boolean = locale.getLanguage == LANGUAGE_JPAPNESE
}
