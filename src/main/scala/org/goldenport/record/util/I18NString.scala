package org.goldenport.record.util

/*
 * @since   May. 24, 2017
 * @version May. 24, 2017
 * @author  ASAMI, Tomoharu
 */
case class I18NString(
  enMessage: String,
  jaMessage: String,
  parameters: List[Any]
) {
  def update(f: String => String): I18NString = I18NString(
    f(enMessage), f(jaMessage), parameters
  )
}
object I18NString {
  def create(msg: String): I18NString = I18NString(msg, msg, Nil)
  def create(en: String, ja: String): I18NString = I18NString(en, ja, Nil)
}
